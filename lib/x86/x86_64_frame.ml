module X86_64_Frame : sig
  include Frame.FRAME

  val rax : Temp.temp
  val rdx : Temp.temp
  val rsp : Temp.temp
end = struct
  type access =
    | InReg of Temp.temp
    | InFrame of int
        (** Offset from frame pointer, in downward direction of stack.
            E.g. if this is 8, the access is located at $fp-8. *)

  (** Stack layout for x86 64 bit. See also page 127.
      Note: in internal functions, argument 1 is always a static link to the
      statically-enclosing frame.
      {v
        ...higher addr
               |-----------|
               |argument n |                                -
               |     .     |                                |
      incoming |     .     | previous                       | generated
        args   |     .     | frame                          | during call
               |argument 8 |                                |
               |argument 7 | (args 1-6 passed in registers) -
               |    RA     |                                - side effect of call 
               |  old ebp  |                                - gen during prolog
      FP ----> |-----------|-----------
               |  local 1  | current
               |  local 2  | frame
               |     .     |
               |     .     |
               |     .     |
               |  local n  |
      SP ----> |-----------|
                             ...lower addr
      v}
   *)
  type frame =
    { name : Temp.label
    ; formals : access list
    ; sp_offset : int ref
          (** current offset from frame pointer, where all memory addresses
            between (fp-sp_offset) and (fp) are being used for variables
            in the frame. Like [InFrame], this grows in the negative direction. *)
    }

  type frag = Proc of frame * Ir.stmt | String of Temp.label * string
  type proc = {prolog : string; body : Assem.instr list; epilog : string}
  type register = string

  module RegisterSet = Set.Make (struct
    type t = register

    let compare = compare
  end)

  (** https://wiki.cdot.senecacollege.ca/wiki/X86_64_Register_and_Instruction_Quick_Start *)
  let registers =
    [
    "rax"; "rbx"; "rcx"; "rdx";
    "rbp"; (* base pointer (fp) *)
    "rsp"; (* stack pointer *)
    "rsi"; (* register source index (source for data copies) *)
    "rdi"; (* register destination index (destination for data copies) *)
    "r8";  "r9";  "r10"; "r11";
    "r12"; "r13"; "r14"; "r15";
    ] [@ocamlformat "disable"]

  let reg_temps = List.map (fun reg -> (reg, Temp.newtemp ())) registers
  let temp_map = List.map (fun (r, t) -> (t, r)) reg_temps
  let temp_of_reg reg = List.assoc reg reg_temps
  let rax = List.assoc "rax" reg_temps
  let rdx = List.assoc "rdx" reg_temps
  let rsp = List.assoc "rsp" reg_temps
  let rbp = List.assoc "rbp" reg_temps
  let fp = rbp
  let rv = rax
  let sp = rsp
  let special_regs = [fp; rv; sp]
  (* Registers live at procedure exit. Mentioned to avoid register allocation
     here. *)

  let arg_regs =
    ["rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9"] |> List.map temp_of_reg

  let callee_saves = ["rbx"; "r12"; "r13"; "r14"; "r15"] |> List.map temp_of_reg
  let caller_saves = arg_regs @ (["rax"; "r10"; "r11"] |> List.map temp_of_reg)
  let calldefs = rsp :: rbp :: caller_saves
  (* registers modified/defined by a [call] instr.
     [rsp] - RA is pushed on.
     [rbp] - Updated by function prolog.
     [caller_saves] - mark as being re-defined on call entry, so that we do not
       attempt to allocate temps used across procedure calls to these registers,
       or if they are allocated, that a spill to save in memory will be done. *)

  let wordsize = 8 (* 64-bit *)

  let expr_of_access = function
    | InReg reg, _ -> Ir.Temp reg (* just return the register access *)
    | InFrame offset, addr_orig_fp ->
        (* fetch the variable its true address *)
        Ir.Mem (Ir.BinOp (Ir.Minus, addr_orig_fp, Ir.Const offset))

  let alloc_local1 sp_offset = function
    | false -> InReg (Temp.newtemp ()) (* doesn't escape *)
    | true ->
        (* Escapes; allocate on the stack.
           SP          old SP               FP
           | new local | ...existing locals | ...incoming args
            -----------
            [wordsize]
        *)
        sp_offset := !sp_offset + wordsize;
        InFrame !sp_offset

  let alloc_local fr = alloc_local1 fr.sp_offset

  let new_frame name formals =
    let sp_offset = ref 0 in
    {name; formals = List.map (alloc_local1 sp_offset) formals; sp_offset}

  let name {name; _} = name
  let formals {formals; _} = formals
  let external_call fn args = Ir.Call (Ir.Name fn, args)

  (* Reference:
     https://www.cs.virginia.edu/~evans/cs216/guides/x86.html *)
  let codegen _f stmt =
    let open Temp in
    let module A = Assem in
    let ice why = failwith ("ICE (x86_64_codegen): " ^ why) in
    let ilist = ref [] in
    let emit instr = ilist := instr :: !ilist in
    let alloc gen =
      let t = newtemp () in
      gen t;
      t
    in
    let emit_oper assem dst src = emit (A.Oper {assem; dst; src; jmp = None}) in
    let emit_jmp assem jmp =
      emit (A.Oper {assem; dst = []; src = []; jmp = Some jmp})
    in
    (* Maximal Munch *)
    let rec munch_stmt = function
      | Ir.Label lab -> emit (A.Label {assem = string_of_label lab ^ ":"; lab})
      | Ir.Expr e -> ignore (munch_expr e)
      (********)
      (* Move *)
      (********)
      | Ir.Mov (Ir.Temp where, Ir.Const what) ->
          emit_oper (Printf.sprintf "mov `d0, %d" what) [where] []
      | Ir.Mov (Ir.Temp where, Ir.Mem (Ir.BinOp (Ir.Plus, s0, Ir.Const off)))
       |Ir.Mov (Ir.Temp where, Ir.Mem (Ir.BinOp (Ir.Plus, Ir.Const off, s0))) ->
          emit_oper
            (Printf.sprintf "mov `d0, [`s0 + %d]" off)
            [where] [munch_expr s0]
      | Ir.Mov (Ir.Temp where, Ir.Mem (Ir.BinOp (Ir.Minus, s0, Ir.Const off)))
        ->
          emit_oper
            (Printf.sprintf "mov `d0, [`s0 - %d]" off)
            [where] [munch_expr s0]
      (* Needed to enable fast-path for munch_expr@[Ir.Mem] *)
      | Ir.Mov (Ir.Temp where, Ir.Mem what) ->
          let loc = newtemp () in
          munch_stmt (Ir.Mov (Ir.Temp loc, Ir.Temp (munch_expr what)));
          emit_oper "mov `d0, [`s0]" [where] [loc]
      | Ir.Mov (Ir.Temp where, what) ->
          emit
            (A.Mov {assem = "mov `d0, `s0"; dst = where; src = munch_expr what})
      | Ir.Mov (Ir.Mem (Ir.BinOp (Ir.Plus, base, Ir.Const off)), what)
       |Ir.Mov (Ir.Mem (Ir.BinOp (Ir.Plus, Ir.Const off, base)), what) ->
          emit_oper
            (Printf.sprintf "mov [`s0 + %d], `s1" off)
            []
            [munch_expr base; munch_expr what]
      | Ir.Mov (Ir.Mem (Ir.BinOp (Ir.Minus, base, Ir.Const off)), what) ->
          emit_oper
            (Printf.sprintf "mov [`s0 - %d], `s1" off)
            []
            [munch_expr base; munch_expr what]
      | Ir.Mov (Ir.Mem where, what) ->
          emit_oper "mov qword ptr [`s0], `s1" []
            [munch_expr where; munch_expr what]
      | Ir.Mov _ -> ice "not a move to a temp or mem"
      (********)
      (* Jump *)
      (********)
      | Ir.Jmp (Ir.Name lab, _) -> emit_jmp "jmp `j0" [lab]
      | Ir.Jmp _ -> ice "unlabeled jumps cannot be emitted"
      | Ir.CJmp (op, e1, e2, t, f) ->
          let mkop assem src = A.Oper {assem; src; dst = []; jmp = None} in
          let cmp_instr, op =
            match (e1, e2) with
            | e1, Ir.Const n ->
                (mkop (Printf.sprintf "cmp `s0, %d" n) [munch_expr e1], op)
            | Ir.Const n, e2 ->
                ( mkop (Printf.sprintf "cmp `s0, %d" n) [munch_expr e2]
                , Ir.not_relop op )
            | e1, e2 -> (mkop "cmp `s0, `s1" [munch_expr e1; munch_expr e2], op)
          in
          let jump =
            match op with
            | Ir.Eq -> "je"
            | Ir.Neq -> "jne"
            | Ir.Lt -> "jl"
            | Ir.Gt -> "jg"
            | Ir.Leq -> "jle"
            | Ir.Geq -> "jge"
            | Ir.Ugt -> "ja"
            | Ir.Ult -> "jb"
            | Ir.Uge -> "ja"
            | Ir.Ule -> "jbe"
          in
          (* Store both possible landing labels for flow analysis.
             During emit, only the "true" label will be filled in, and
             canonicalization has already ensured that this [CJump] is
             followed by the "false" label. *)
          emit cmp_instr;
          emit_jmp (jump ^ " `j0") [t; f]
      | Ir.Seq _ -> ice "Seqs should be destroyed during canonicalization"
    (* Creates a new temporary [d] assigning [lhs] to it, then applies a binary
       operation that writes back to [d]. Hence we can emit something like
       [a + b] as [c<-a; add c, b; c], avoiding overwriting [a]. *)
    and emit_binop lhs assem srcs =
      alloc (fun d ->
          emit (A.Mov {assem = "mov `d0, `s0"; dst = d; src = munch_expr lhs});
          emit (A.Oper {assem; dst = [d]; src = srcs @ [d]; jmp = None}) )
    and munch_expr = function
      (**************)
      (* Arithmetic *)
      (**************)
      (* Add *)
      | Ir.BinOp (Ir.Plus, s0, Ir.Const 1) | Ir.BinOp (Ir.Plus, Ir.Const 1, s0)
        ->
          emit_binop s0 (Printf.sprintf "inc `d0") []
      | Ir.BinOp (Ir.Plus, s0, Ir.Const s1) | Ir.BinOp (Ir.Plus, Ir.Const s1, s0)
        ->
          emit_binop s0 (Printf.sprintf "add `d0, %d" s1) []
      | Ir.BinOp (Ir.Plus, s0, s1) ->
          emit_binop s0 "add `d0, `s0" [munch_expr s1]
      (* Sub *)
      | Ir.BinOp (Ir.Minus, s0, Ir.Const 1) ->
          emit_binop s0 (Printf.sprintf "dec `d0") []
      | Ir.BinOp (Ir.Minus, s0, Ir.Const s1) ->
          emit_binop s0 (Printf.sprintf "sub `d0, %d" s1) []
      | Ir.BinOp (Ir.Minus, s0, s1) ->
          emit_binop s0 "sub `d0, `s0" [munch_expr s1]
      (* Mul *)
      | Ir.BinOp (Ir.Mul, s0, Ir.Const s1) | Ir.BinOp (Ir.Mul, Ir.Const s1, s0)
        ->
          alloc (fun d ->
              emit
                (A.Oper
                   { assem = Printf.sprintf "imul `d0, `s0, %d" s1
                   ; dst = [d]
                   ; src = [munch_expr s0]
                   ; jmp = None } ) )
      | Ir.BinOp (Ir.Mul, s0, s1) ->
          emit_binop s0 "imul `d0, `s0" [munch_expr s1]
      (* Div *)
      | Ir.BinOp (Ir.Div, s0, s1) ->
          let divisor = munch_expr s1 in
          alloc (fun result ->
              (* 1. dividend must be placed in rax.
                 2. extend qword to octoword rdx:rax.
                 3. idiv divisor
                 4. retrieve quotient from rax.
                 No need to save/restore rax/rdx, as we should not assign any
                 values other than these to them at this time. The register
                 allocator will make suere rax/rdx are only used in places that
                 do not interfere with this anyway. *)
              munch_stmt (Ir.Mov (Ir.Temp rax, s0));
              emit_oper "cqto" [rax; rdx] [rax];
              emit_oper "idiv `s0" [rax; rdx] [divisor; rax; rdx];
              munch_stmt (Ir.Mov (Ir.Temp result, Ir.Temp rax)) )
      (*********)
      (* Logic *)
      (*********)
      (* And *)
      | Ir.BinOp (Ir.And, s0, Ir.Const s1) | Ir.BinOp (Ir.And, Ir.Const s1, s0)
        ->
          emit_binop s0 (Printf.sprintf "and `d0, %d" s1) []
      | Ir.BinOp (Ir.And, s0, s1) -> emit_binop s0 "and `d0, `s0" [munch_expr s1]
      (* Or *)
      | Ir.BinOp (Ir.Or, s0, Ir.Const s1) | Ir.BinOp (Ir.Or, Ir.Const s1, s0) ->
          emit_binop s0 (Printf.sprintf "or `d0, %d" s1) []
      | Ir.BinOp (Ir.Or, s0, s1) -> emit_binop s0 "or `d0, `s0" [munch_expr s1]
      (* Xor *)
      | Ir.BinOp (Ir.Xor, s0, Ir.Const s1) | Ir.BinOp (Ir.Xor, Ir.Const s1, s0)
        ->
          emit_binop s0 (Printf.sprintf "xor `d0, %d" s1) []
      | Ir.BinOp (Ir.Xor, s0, s1) -> emit_binop s0 "xor `d0, `s0" [munch_expr s1]
      (* Shl *)
      | Ir.BinOp (Ir.Shl, s0, Ir.Const s1) ->
          emit_binop s0 (Printf.sprintf "shl `d0, %d" s1) []
      | Ir.BinOp (Ir.Shl, _, _) -> ice "cannot generate variable shls"
      | Ir.BinOp (Ir.Shr, s0, Ir.Const s1) ->
          emit_binop s0 (Printf.sprintf "shr `d0, %d" s1) []
      | Ir.BinOp (Ir.Shr, _, _) -> ice "cannot generate variable shrs"
      | Ir.BinOp (Ir.Sar, s0, Ir.Const s1) ->
          emit_binop s0 (Printf.sprintf "sar `d0, %d" s1) []
      | Ir.BinOp (Ir.Sar, _, _) -> ice "cannot generate variable sars"
      (*********)
      (* Loads *)
      (*********)
      | Ir.Temp t -> t
      | Ir.Const n ->
          alloc (fun d -> emit_oper (Printf.sprintf "mov `d0, %d" n) [d] [])
      | Ir.Name lab ->
          alloc (fun d ->
              emit_oper
                (Printf.sprintf "lea `d0, [rip + %s]" (string_of_label lab))
                [d] [] )
      | Ir.Mem _ as mem -> alloc (fun d -> munch_stmt (Ir.Mov (Ir.Temp d, mem)))
      (********)
      (* Call *)
      (********)
      | Ir.Call (Ir.Name fn, args) ->
          (* No need to save/restore caller-save registers at this time.
             Instead, it is enough to mark caller-save registers in the [dst] of
             the call instr. This indicates to the register allocator that these
             registers will be changed by the call, and so will avoid allocating
             temps that need to exist across calls to caller-save registers, or
             will spill them.
             Suppose rE is callee-save, rR is caller-save. If we have
               t214 def ------ CALL ------------ RET ------ t214 use
                               rR def     rR use
             with t214, rR interfering
             then two possible allocations are
               t214 def ------ CALL ------------ RET ------ t214 use
               =rE             rR def     rR use            =rE
             or
               t214 def ------ CALL ------------ RET ------ t214 use
               MEM<-rR         rR def     rR use            rR<-MEM
             if rR is never used in the call, then there will be no interference
             and we may allocate it in rR without a save anyway.
             See page 237.
          *)
          let args = munch_args args in
          emit
            (A.Oper
               { assem = Printf.sprintf "call %s" (string_of_label fn)
               ; src = args
               ; dst = calldefs
               ; jmp = None } );
          rv
      | Ir.Call _ -> ice "non-label calls not permitted"
      | Ir.ESeq _ -> ice "ESeqs should be eliminated during canonicalization"
    and munch_args args =
      let rec eat_stack = function
        | [] -> ()
        | arg1 :: argns ->
            (* Arguments are pushed onto stack in reverse order:
                 argn
                 ...
                 arg1
               so handle rest of args first *)
            eat_stack argns;
            emit
              (A.Oper
                 { assem = "push `s0"
                 ; dst = [rsp]
                 ; src = [munch_expr arg1]
                 ; jmp = None } )
      in
      let rec eat_argregs = function
        | [], _ -> []
        | args, [] ->
            eat_stack args;
            []
        | arg :: rest_args, reg :: rest_regs ->
            munch_stmt (Ir.Mov (Ir.Temp reg, arg));
            reg :: eat_argregs (rest_args, rest_regs)
      in
      eat_argregs (args, arg_regs)
    in
    munch_stmt stmt;
    List.rev !ilist

  let proc_entry_exit1 fr body =
    (* Move formals into their accesses. *)
    let rec eat_stack n = function
      | [] -> []
      | formal1 :: formalns ->
          (* Arguments are pushed onto stack in reverse order:
               argn
               ...
               arg1
               RA
               old FP
               -------- FP
             so we want to pop first formal first, then rest.
             Also, the nth argument on the stack is (2+n)*ws above the FP. *)
          let offset = (2 + n) * wordsize in
          Ir.Mov
            ( expr_of_access (formal1, Ir.Temp fp)
            , Ir.Mem (Ir.BinOp (Ir.Plus, Ir.Temp fp, Ir.Const offset)) )
          :: eat_stack n formalns
    in
    let rec eat_argregs = function
      | [], _ -> []
      | formals, [] -> eat_stack 0 formals
      | formal :: rest_formals, reg :: rest_regs ->
          Ir.Mov (expr_of_access (formal, Ir.Temp fp), Ir.Temp reg)
          :: eat_argregs (rest_formals, rest_regs)
    in
    let load_args = eat_argregs (fr.formals, arg_regs) in
    (* Save, then restore callee-save registers. Register allocation should
       coalesce and eliminate most of these moves. *)
    let callee_saves =
      List.map (fun t -> (Ir.Temp t, Ir.Temp (Temp.newtemp ()))) callee_saves
    in
    let saves = List.map (fun (og, t) -> Ir.Mov (t, og)) callee_saves in
    let restores = List.map (fun (og, t) -> Ir.Mov (og, t)) callee_saves in
    Ir.seq (saves @ load_args @ [body] @ restores)

  let proc_entry_exit2 _ body =
    body
    @ [ Assem.Oper
          { assem = ""
          ; (* Mark special/reserved registers as still being live (necessary)
               at procedure exit. This means that the registers are live
               throughout, which prevents the register allocator from trying to
               use them for any other purpose (page 209). *)
            src = [fp; rv; sp] @ callee_saves
          ; dst = []
          ; jmp = None } ]

  let proc_entry_exit3 {name; sp_offset; _} body =
    let fr_size = !sp_offset in
    { prolog =
        Printf.sprintf {|%s:
  push rbp
  mov rbp, rsp
  sub rsp, %d|}
          (Temp.string_of_label name)
          fr_size
    ; body
    ; epilog = {|  mov rsp, rbp
  pop rbp
  ret|} }

  type allocation = (Temp.temp, register) Hashtbl.t

  let assem_of_string (lab, str) =
    Printf.sprintf {|%s:
  .quad %d
  .ascii %s|} (Temp.string_of_label lab)
      (String.length str) (String.escaped str)

  let assem_of_frame (frame, instrs, coloring) =
    let open Assem in
    let {prolog; body; epilog} = proc_entry_exit3 frame instrs in
    let string_of_temp t = t |> Hashtbl.find coloring in
    let body =
      body
      |> List.map (string_of_instr string_of_temp)
      |> List.map (fun s -> "  " ^ s)
      |> String.concat "\n"
    in
    String.concat "\n" [prolog; body; epilog]

  let emit strings frames =
    let strings = List.map assem_of_string strings in
    let frames = List.map assem_of_frame frames in
    String.concat "\n"
      ({|BITS 64
section .text

global _start
|} :: (strings @ frames))
end
