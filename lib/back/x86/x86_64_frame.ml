let ice why = failwith ("ICE (x86-64): " ^ why)

module LabelSet = Temp.LabelSet
module Print = Util.Print

module X86_64_Frame : sig
  include Frame.FRAME

  val rax : Temp.temp
  val rdx : Temp.temp
  val rsp : Temp.temp
end = struct
  type access =
    | InReg of Temp.temp
    | InFrame of int * string
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
    ; formals : (access * string) list
    ; next_local_name : int ref
          (** Name to give next local allocated on the frame. For name
              generation purposes only. *)
    ; sp_offset : int ref
          (** current offset from frame pointer, where all memory addresses
            between (fp-sp_offset) and (fp) are being used for variables
            in the frame. Like [InFrame], this grows in the negative direction. *)
    ; mutable called_externs : LabelSet.t }

  let is_main name = Temp.string_of_label name = "_start"

  type frag = Proc of frame * Ir.stmt | String of Temp.label * string

  type proc =
    { prolog : Assem.instr list
    ; body : Assem.instr list
    ; epilog : Assem.instr list }

  type register =
    | Rax  (** return value register *)
    | Rbx
    | Rcx
    | Rdx
    | Rbp  (** base pointer (fp) *)
    | Rsp  (** stack pointer *)
    | Rsi  (** register source index (source for data copies) *)
    | Rdi  (** register destination index (destination for data copies) *)
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15

  let string_of_register = function
    | Rax -> "rax"
    | Rbx -> "rbx"
    | Rcx -> "rcx"
    | Rdx -> "rdx"
    | Rbp -> "rbp"
    | Rsp -> "rsp"
    | Rsi -> "rsi"
    | Rdi -> "rdi"
    | R8 -> "r8"
    | R9 -> "r9"
    | R10 -> "r10"
    | R11 -> "r11"
    | R12 -> "r12"
    | R13 -> "r13"
    | R14 -> "r14"
    | R15 -> "r15"

  module RegisterSet = Set.Make (struct
    type t = register

    let compare = compare
  end)

  (** https://wiki.cdot.senecacollege.ca/wiki/X86_64_Register_and_Instruction_Quick_Start *)
  let registers =
    [ Rax; Rbx; Rcx; Rdx; Rbp; Rsp; Rsi; Rdi; R8; R9; R10; R11; R12; R13; R14
    ; R15 ]

  let reg_temps = List.map (fun reg -> (reg, Temp.newtemp ())) registers
  let temp_map = List.map (fun (r, t) -> (t, r)) reg_temps
  let temp_of_reg reg = List.assoc reg reg_temps
  let rax = List.assoc Rax reg_temps
  let rdx = List.assoc Rdx reg_temps
  let rsp = List.assoc Rsp reg_temps
  let rbp = List.assoc Rbp reg_temps
  let fp = rbp
  let rv = rax
  let sp = rsp
  let special_regs = [fp; rv; sp]
  (* Registers live at procedure exit. Mentioned to avoid register allocation
     here. *)

  let arg_regs = [Rdi; Rsi; Rdx; Rcx; R8; R9] |> List.map temp_of_reg
  let callee_saves = [Rbx; R12; R13; R14; R15] |> List.map temp_of_reg
  let caller_saves = arg_regs @ ([Rax; R10; R11] |> List.map temp_of_reg)
  let calldefs = caller_saves
  (* registers modified/defined by a [call] instr.
     [rsp] - RA is pushed on.
     [rbp] - Updated by function prolog.
     [caller_saves] - mark as being re-defined on call entry, so that we do not
       attempt to allocate temps used across procedure calls to these registers,
       or if they are allocated, that a spill to save in memory will be done. *)

  let wordsize = 8 (* 64-bit *)

  let expr_of_access = function
    | InReg reg, _ -> Ir.Temp reg (* just return the register access *)
    | InFrame (offset, name), addr_orig_fp ->
        (* fetch the variable its true address *)
        Ir.Mem (Ir.BinOp (Ir.Minus, addr_orig_fp, Ir.Const offset), name)

  let address_of_access a =
    match expr_of_access a with
    | Ir.Mem (addr, _) -> addr
    | Ir.Temp _ -> ice "access is not the stack"
    | _ -> ice "unreachable"

  let name_or_newlocal name next_local_name =
    match name with
    | Some n -> n
    | None ->
        let n = "local" ^ string_of_int !next_local_name in
        incr next_local_name;
        n

  let alloc_local1 next_local_name sp_offset name = function
    | false -> InReg (Temp.newtemp ()) (* doesn't escape *)
    | true ->
        (* Escapes; allocate on the stack.
           SP          old SP               FP
           | new local | ...existing locals | ...incoming args
            -----------
            [wordsize] ^ access
        *)
        sp_offset := !sp_offset + wordsize;
        let name = name_or_newlocal name next_local_name in
        InFrame (!sp_offset, name)

  let alloc_local fr name = alloc_local1 fr.next_local_name fr.sp_offset name

  let alloc_stack {sp_offset; next_local_name; _} name size =
    let name = name_or_newlocal name next_local_name in
    sp_offset := !sp_offset + size;
    InFrame (!sp_offset, name)

  let new_frame name formal_names formals =
    let sp_offset = ref 0 in
    let next_local_name = ref 1 in
    { name
    ; formals =
        List.map2
          (fun n esc -> (alloc_local1 next_local_name sp_offset (Some n) esc, n))
          formal_names formals
    ; next_local_name
    ; sp_offset
    ; called_externs = (* We always call exit! *)
                       LabelSet.singleton Temp.ttexit }

  let name {name; _} = name
  let formals {formals; _} = formals |> List.map fst

  let external_call fr fn args =
    fr.called_externs <- LabelSet.add fn fr.called_externs;
    Ir.Call (Ir.Name fn, args)

  (** Marker so we know what labels to pull back during emit. *)
  let label_marker = "~~X86LABEL~~"

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
    let filter_cmts = List.filter (fun c -> c <> "") in
    let emit_oper assem dst src cmts =
      emit (A.Oper {assem; dst; src; jmp = None; comments = filter_cmts cmts})
    in
    let emit_jmp assem jmp cmts =
      emit
        (A.Oper
           { assem
           ; dst = []
           ; src = []
           ; jmp = Some jmp
           ; comments = filter_cmts cmts } )
    in
    (* Maximal Munch *)
    let rec munch_stmt = function
      | Ir.Label lab ->
          emit
            (A.Label
               { assem = label_marker ^ string_of_label lab ^ ":"
               ; lab
               ; comments = [] } )
      | Ir.Expr e -> ignore (munch_expr e)
      (********)
      (* Move *)
      (********)
      | Ir.Mov (Ir.Temp where, Ir.Const 0, cmt) ->
          emit_oper "xor `d0, `d0" [where] [where] [cmt]
      | Ir.Mov (Ir.Temp where, Ir.Const what, cmt) ->
          emit_oper (Printf.sprintf "mov `d0, %d" what) [where] [] [cmt]
      | Ir.Mov
          (Ir.Temp where, Ir.Mem (Ir.BinOp (Ir.Plus, s0, Ir.Const off), _), cmt)
       |Ir.Mov
          (Ir.Temp where, Ir.Mem (Ir.BinOp (Ir.Plus, Ir.Const off, s0), _), cmt)
        ->
          emit_oper
            (Printf.sprintf "mov `d0, [`s0 + %d]" off)
            [where] [munch_expr s0] [cmt]
      | Ir.Mov
          (Ir.Temp where, Ir.Mem (Ir.BinOp (Ir.Minus, s0, Ir.Const off), _), cmt)
        ->
          emit_oper
            (Printf.sprintf "mov `d0, [`s0 - %d]" off)
            [where] [munch_expr s0] [cmt]
      (* Needed to enable fast-path for munch_expr@[Ir.Mem] *)
      | Ir.Mov (Ir.Temp where, Ir.Mem (what, _), cmt) ->
          let loc = newtemp () in
          munch_stmt (Ir.Mov (Ir.Temp loc, Ir.Temp (munch_expr what), cmt));
          emit_oper "mov `d0, [`s0]" [where] [loc] [cmt]
      | Ir.Mov (Ir.Temp where, what, cmt) ->
          emit
            (A.Mov
               { assem = "mov `d0, `s0"
               ; dst = where
               ; src = munch_expr what
               ; comments = filter_cmts [cmt] } )
      | Ir.Mov
          (Ir.Mem (Ir.BinOp (Ir.Plus, base, Ir.Const off), _), Ir.Const imm, cmt)
       |Ir.Mov
          (Ir.Mem (Ir.BinOp (Ir.Plus, Ir.Const off, base), _), Ir.Const imm, cmt)
        ->
          emit_oper
            (Printf.sprintf "mov qword [`s0 + %d], %d" off imm)
            [] [munch_expr base] [cmt]
      | Ir.Mov (Ir.Mem (Ir.BinOp (Ir.Plus, base, Ir.Const off), _), what, cmt)
       |Ir.Mov (Ir.Mem (Ir.BinOp (Ir.Plus, Ir.Const off, base), _), what, cmt)
        ->
          emit_oper
            (Printf.sprintf "mov [`s0 + %d], `s1" off)
            []
            [munch_expr base; munch_expr what]
            [cmt]
      | Ir.Mov
          ( Ir.Mem (Ir.BinOp (Ir.Minus, base, Ir.Const off), _)
          , Ir.Const imm
          , cmt ) ->
          emit_oper
            (Printf.sprintf "mov qword [`s0 - %d], %d" off imm)
            [] [munch_expr base] [cmt]
      | Ir.Mov (Ir.Mem (Ir.BinOp (Ir.Minus, base, Ir.Const off), _), what, cmt)
        ->
          emit_oper
            (Printf.sprintf "mov [`s0 - %d], `s1" off)
            []
            [munch_expr base; munch_expr what]
            [cmt]
      | Ir.Mov (Ir.Mem (where, _), what, cmt) ->
          emit_oper "mov qword [`s0], `s1" []
            [munch_expr where; munch_expr what]
            [cmt]
      | Ir.Mov _ -> ice "not a move to a temp or mem"
      (********)
      (* Jump *)
      (********)
      | Ir.Jmp (Ir.Name lab, _) -> emit_jmp "jmp `j0" [lab] []
      | Ir.Jmp _ -> ice "unlabeled jumps cannot be emitted"
      | Ir.CJmp (op, e1, e2, t, f, cmt) ->
          let mkop assem src =
            A.Oper {assem; src; dst = []; jmp = None; comments = [cmt]}
          in
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
          emit_jmp (jump ^ " `j0") [t; f] []
      | Ir.Seq _ -> ice "Seqs should be destroyed during canonicalization"
    (* Creates a new temporary [d] assigning [lhs] to it, then applies a binary
       operation that writes back to [d]. Hence we can emit something like
       [a + b] as [c<-a; add c, b; c], avoiding overwriting [a]. *)
    and emit_binop lhs assem srcs =
      alloc (fun d ->
          emit
            (A.Mov
               { assem = "mov `d0, `s0"
               ; dst = d
               ; src = munch_expr lhs
               ; comments = [] } );
          emit
            (A.Oper
               {assem; dst = [d]; src = srcs @ [d]; jmp = None; comments = []}
            ) )
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
                   ; jmp = None
                   ; comments = [] } ) )
      | Ir.BinOp (Ir.Mul, s0, s1) ->
          emit_binop s0 "imul `d0, `s0" [munch_expr s1]
      (* Div *)
      | Ir.BinOp (Ir.Div, s0, s1) ->
          let divisor = munch_expr s1 in
          (* 1. dividend must be placed in rax.
             2. extend qword to octoword rdx:rax.
             3. idiv divisor
             4. retrieve quotient from rax.
             No need to save/restore rax/rdx, as we should not assign any
             values other than these to them at this time. The register
             allocator will make suere rax/rdx are only used in places that
             do not interfere with this anyway.

             NB: still, we do have to be a bit careful. To avoid the case
             where a temp serves as a dividend more than once, store it in a
             fresh temp that gets moved into [rax]. This helps the register
             allocator later on when it tries to determine what unique moves
             are associated with a temp. *)
          alloc (fun result ->
              let _ =
                alloc (fun dividend ->
                    munch_stmt (Ir.Mov (Ir.Temp dividend, s0, ""));
                    munch_stmt
                      (Ir.Mov (Ir.Temp rax, Ir.Temp dividend, "dividend"));
                    emit_oper "cqo" [rdx; rax] [rax] [];
                    emit_oper "idiv `s0" [rax; rdx] [divisor; rax; rdx] [];
                    munch_stmt (Ir.Mov (Ir.Temp result, Ir.Temp rax, "quotient")) )
              in
              () )
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
          alloc (fun d -> emit_oper (Printf.sprintf "mov `d0, %d" n) [d] [] [])
      | Ir.Name lab ->
          alloc (fun d ->
              emit_oper
                (Printf.sprintf "lea `d0, [rel %s]" (string_of_label lab))
                [d] [] [] )
      | Ir.Mem (_, cmt) as mem ->
          alloc (fun d -> munch_stmt (Ir.Mov (Ir.Temp d, mem, cmt)))
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
          let args_on_stack = max (List.length args - List.length arg_regs) 0 in
          let args_stack_sz = ((args_on_stack * wordsize) + 15) / 16 * 16 in
          let args_stack_padding = args_stack_sz - (args_on_stack * wordsize) in
          if args_stack_padding > 0 then
            emit_oper
              (Printf.sprintf "sub rsp, %d" args_stack_padding)
              [rsp] [rsp]
              ["Pad stack for alignment"];
          let args = munch_args (string_of_label fn) args in
          let fnname = string_of_label fn in
          emit_oper (Printf.sprintf "call %s" fnname) calldefs args [];
          (* Cleanup stack of any pushed-on args after call. *)
          if args_stack_sz > 0 then
            emit_oper
              (Printf.sprintf "add rsp, %d" args_stack_sz)
              [rsp] [rsp]
              [Printf.sprintf "Deallocate %s args" fnname];
          rv
      | Ir.Call _ -> ice "non-label calls not permitted"
      | Ir.ESeq _ -> ice "ESeqs should be eliminated during canonicalization"
    and munch_args fn args =
      let rec eat_stack n = function
        | [] -> ()
        | arg1 :: argns ->
            (* Arguments are pushed onto stack in reverse order:
                 argn
                 ...
                 arg1
               so handle rest of args first. *)
            eat_stack (n + 1) argns;
            emit_oper "push `s0" [rsp] [munch_expr arg1; rsp]
              ["arg" ^ string_of_int n ^ ":" ^ fn]
      in
      let rec eat_argregs n = function
        | [], _ -> []
        | args, [] ->
            eat_stack n args;
            []
        | arg :: rest_args, reg :: rest_regs ->
            munch_stmt
              (Ir.Mov (Ir.Temp reg, arg, "arg" ^ string_of_int n ^ ":" ^ fn));
            reg :: eat_argregs (n + 1) (rest_args, rest_regs)
      in
      eat_argregs 1 (args, arg_regs)
    in
    munch_stmt stmt;
    List.rev !ilist

  let fetch_from_access fr temp mem cmt =
    let i =
      Ir.Mov (Ir.Temp temp, expr_of_access (mem, Ir.Temp fp), cmt) |> codegen fr
    in
    List.iter (Assem.add_comment cmt) i;
    i

  let store_to_access fr mem temp cmt =
    let i =
      Ir.Mov (expr_of_access (mem, Ir.Temp fp), Ir.Temp temp, cmt) |> codegen fr
    in
    List.iter (Assem.add_comment cmt) i;
    i

  let proc_entry_exit1 fr body =
    (* Move formals into their accesses. *)
    let rec eat_stack n = function
      | [] -> []
      | (formal1, name) :: formalns ->
          (* Arguments are pushed onto stack in reverse order:
               argn
               ...
               arg1
               RA           <- added by call
               old FP       <- added during proc_entry_exit3
               -------- FP
             so we want to pop first formal first, then rest.
             Also, the nth argument on the stack is (2+n)*ws above the FP. *)
          let offset = (2 + n) * wordsize in
          Ir.Mov
            ( expr_of_access (formal1, Ir.Temp fp)
            , Ir.Mem (Ir.BinOp (Ir.Plus, Ir.Temp fp, Ir.Const offset), name)
            , name )
          :: eat_stack (n + 1) formalns
    in
    let rec eat_argregs = function
      | [], _ -> []
      | formals, [] -> eat_stack 0 formals
      | (formal, name) :: rest_formals, reg :: rest_regs ->
          Ir.Mov (expr_of_access (formal, Ir.Temp fp), Ir.Temp reg, name)
          :: eat_argregs (rest_formals, rest_regs)
    in
    let load_args = eat_argregs (fr.formals, arg_regs) in
    (* Save, then restore callee-save registers. Register allocation should
       coalesce and eliminate most of these moves. *)
    let callee_saves =
      List.map (fun t -> (Ir.Temp t, Ir.Temp (Temp.newtemp ()))) callee_saves
    in
    let saves = List.map (fun (og, t) -> Ir.Mov (t, og, "")) callee_saves in
    let restores = List.map (fun (og, t) -> Ir.Mov (og, t, "")) callee_saves in
    Ir.seq (saves @ load_args @ [body] @ restores)

  let proc_entry_exit2 f body =
    body
    @ [ Assem.Oper
          { assem = ""
          ; (* Mark special/reserved registers as still being live (necessary)
               at procedure exit. This means that the registers are live
               throughout, which prevents the register allocator from trying to
               use them for any other purpose (page 209). *)
            src = [fp; rv; sp] @ callee_saves
          ; dst = []
          ; jmp = None
          ; comments = [] } ]
    @
    if is_main f.name then
      let t = Ir.Temp (Temp.newtemp ()) in
      List.concat_map (codegen ())
        [ Ir.Mov (t, Ir.Temp rax, "")
        ; Ir.Expr (Ir.Call (Ir.Name Temp.ttexit, [t])) ]
    else []

  let proc_entry_exit3 {name; sp_offset; _} body =
    let fr_size = !sp_offset in
    (* https://www.uclibc.org/docs/psABI-x86_64.pdf
       We need to align the frame to be a multiple of 16 bytes so that when
       [rsp] obeys the x86 calling convention whenever we call another procedure
       from this function.
       Section 3.2.2: "the value (%rsp + 8) is always a multiple of 16 when
         control is transferred to the function entry point."
       Section 3.4.1: "%rsp ... is guaranteed to be 16-byte aligned at process
         entry."
       TODO: we also need to make sure the stack is aligned if we push any args
         onto the stack.
       TODO: we don't need to do this for leaf functions. *)
    let fr_size = (fr_size + 15) / 16 * 16 in
    (* Check:
       - At this point our stack looks like
           ...prev frame
           argn
           ...
           arg1
           RA           <- added by call
       - [push rbp] saves prev fp on stack
       - [mov rbp, rsp] fixes up the present fp
       - [sub rsp, ...] allocates space for locals
       - ... body
       - [mov rsp, rbp] deallocates the space we gave for the locals
       - [pop rbp] retrieves last fp (this was the last thing on the stack
           before locals)
       - [ret] pops return address, which was the last this on the stack before
           we pushed the old fp
       - The calling function will be responsible for cleaning up the arguments
           pushed onto the stack.
    *)
    let module A = Assem in
    let is_main = Temp.string_of_label name = "_start" in
    (* Prolog if is main:
         lab:
           and rsp, 0xFFFFFFFFFFFFFFF0
           mov rbp, rsp
           sub rsp, #frame_size
       if not main:
         lab:
           push rbp
           mov rbp, rsp
           sub rsp, #frame_size
    *)
    let prolog =
      codegen () (Ir.Label name)
      @ [ ( if is_main then
            A.Oper
              { assem = "and rsp, 0xFFFFFFFFFFFFFFF0"
              ; dst = [rsp]
              ; src = [rsp]
              ; jmp = None
              ; comments = ["16-byte alignment"] }
          else
            A.Oper
              { assem = "push rbp"
              ; dst = [rsp]
              ; src = [rbp; rsp]
              ; jmp = None
              ; comments = [] } )
        ; A.Mov {assem = "mov rbp, rsp"; dst = rbp; src = rsp; comments = []}
        ; A.Oper
            { assem = "sub rsp, " ^ string_of_int fr_size
            ; dst = [rsp]
            ; src = [rsp]
            ; jmp = None
            ; comments = [] } ]
    in
    (* Epilog if is main:
           <nothing>
       if not main:
           mov rsp, rbp
           pop rbp
           ret
    *)
    let epilog =
      if is_main then []
      else
        [ A.Mov {assem = "mov rsp, rbp"; dst = rsp; src = rbp; comments = []}
        ; A.Oper
            { assem = "pop rbp"
            ; dst = [rsp]
            ; src = [rbp; rsp]
            ; jmp = None
            ; comments = [] }
        ; A.Oper
            {assem = "ret"; dst = [rsp]; src = [rsp]; jmp = None; comments = []}
        ]
    in
    {prolog; body; epilog}

  type allocation = (Temp.temp, register) Hashtbl.t

  let assem_of_string (lab, str) =
    Printf.sprintf {|%s:
  dq %d
  db `%s`|} (Temp.string_of_label lab)
      (String.length str) (String.escaped str)

  let assem_of_frame (frame, instrs, string_of_temp) =
    let open Assem in
    let {prolog; body; epilog} = proc_entry_exit3 frame instrs in
    let ws_of_label_marker = String.make (String.length label_marker) ' ' in
    prolog @ body @ epilog
    |> fmt_instrs string_of_temp ";" (* eliminate_moves *) true
    |> Print.reflow 2 |> Print.lines
    (* pullback labels *)
    |> List.map (fun s ->
           Str.replace_first
             (Str.regexp
                (Printf.sprintf {|\( +\)%s\([^ ]*\)\( *\)|} label_marker) )
             (Printf.sprintf {|\2\1%s\3|} ws_of_label_marker)
             s )
    |> String.concat "\n"

  let emit_common strings frames main =
    let open Temp in
    let externs =
      List.fold_left
        (fun a (b, _, _) -> LabelSet.union a b.called_externs)
        LabelSet.empty frames
      |> LabelSet.to_seq |> List.of_seq
      |> List.map (fun e -> "extern " ^ string_of_label e)
    in
    let strings = List.map assem_of_string strings in
    let frames = List.map assem_of_frame frames in
    ["; syntax:nasm"; "BITS 64"; "section .text"]
    @ (if externs = [] then [] else [""] (* newline padding for externs *))
    @ externs
    @ [""; Printf.sprintf "global %s" (string_of_label main); ""]
    @ strings @ frames
    |> String.concat "\n"

  let emit strings frames main =
    emit_common strings
      (List.map
         (fun (f, i, a) ->
           (f, i, fun t -> string_of_register (Hashtbl.find a t)) )
         frames )
      main

  let reserved_temps = List.map snd reg_temps
  let reserved_labels = []

  module Debug = struct
    let emit_debug strings frames string_of_temp main =
      emit_common strings
        (List.map (fun (f, i) -> (f, i, string_of_temp)) frames)
        main
  end
end
