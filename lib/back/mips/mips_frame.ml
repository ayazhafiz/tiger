module MipsFrame : Frame.FRAME = struct
  type access =
    | InReg of Temp.temp
    | InFrame of int  (** offset from frame pointer *)

  (** See page 127 for Appel's suggested stack layout.
                             ...higher addr
               |-----------|
               |argument n |
               |     .     |
      incoming |     .     | previous
        args   |     .     | frame
               |argument 2 |
               |argument 1 |
               |static link|            (actually seen as an argument)
      FP ----> |-----------|-----------
               |local vars | current
               |           | frame
               |-----------|
               |return addr|
               |           |
               |   temps   |
               |           |
               |   saved   |
               | registers |
               |-----------|
               |argument n |
               |     .     |
      outgoing |     .     |
        args   |     .     |
               |argument 2 |
               |argument 1 |
               |static link|            (actually seen as an argument)
      SP ----> |-----------|-----------
                             next frame
                             ...lower addr
   *)
  type frame =
    { name : Temp.label
    ; formals : access list
    ; mutable sp_offset : int
          (** current offset from frame pointer, where all memory addresses
            between (fp+sp_offset) and (fp) are being used for variables
            in the frame. *)
    }

  type frag = Proc of frame * Ir.stmt | String of Temp.label * string
  type proc = {prolog : string; body : Assem.instr list; epilog : string}
  type register = string

  let string_of_register r = r

  module RegisterSet = Set.Make (struct
    type t = register

    let compare = compare
  end)

  (** https://en.wikipedia.org/wiki/MIPS_architecture#Calling_conventions
      Name     Number   Use                           Callee must preserve?
      $zero    $0       constant 0                    N/A
      $at      $1       assembler temporary           No
      $v0–$v1  $2–$3    values for function returns   No
                        and expression evaluation
      $a0–$a3  $4–$7    function arguments            No
      $t0–$t7  $8–$15	  temporaries                   No
      $s0–$s7  $16–$23  saved temporaries             Yes
      $t8–$t9  $24–$25  temporaries                   No
      $k0–$k1  $26–$27  reserved for OS kernel        N/A
      $gp      $28      global pointer                Yes
      $sp	     $29      stack pointer                 Yes
      $fp	     $30      frame pointer                 Yes
      $ra	     $31      return address                N/A *)
  let registers =
    [
    "$zero"; "$at";
    "$v0"; "$v1"; (* returns, expr evals *)
    "$a0"; "$a1"; "$a2"; "$a3"; (* fn args *)
    "$t0"; "$t1"; "$t2"; "$t3"; "$t4"; "$t5"; "$t6"; "$t7"; "$t8"; "$t9"; (* caller-save temps *)
    "$s0"; "$s1"; "$s2"; "$s3"; "$s4"; "$s5"; "$s6"; "$s7"; (* callee-save temps *)
    "$k0"; "$k1"; (* kernel reserved *)
    "$gp"; "$sp"; "$fp"; "$ra" (* etc globals *)
    ] [@ocamlformat "disable"]

  let reg_temps = List.map (fun reg -> (reg, Temp.newtemp ())) registers
  let temp_map = List.map (fun (r, t) -> (t, r)) reg_temps
  let temp_of_reg reg = List.assoc reg reg_temps
  let fp = List.assoc "$fp" reg_temps
  let rv = List.assoc "$v0" reg_temps
  let ra = List.assoc "$ra" reg_temps
  let sp = List.assoc "$sp" reg_temps
  let zero = List.assoc "$zero" reg_temps
  let special_regs = [fp; rv; ra; sp; zero]
  let arg_regs = ["$a0"; "$a1"; "$a2"; "$a3"] |> List.map temp_of_reg

  let callee_saves =
    List.filter (fun (r, _) -> String.sub r 0 2 = "$s") reg_temps
    |> List.map snd

  let caller_saves =
    ( List.filter (fun (r, _) -> String.sub r 0 2 = "$t") reg_temps
    |> List.map snd )
    @ arg_regs

  let calldefs = rv :: ra :: caller_saves
  let wordsize = 4

  let expr_of_access = function
    | InReg reg, _ -> Ir.Temp reg (* just return the register access *)
    | InFrame offset, addr_orig_fp ->
        (* fetch the variable its true address *)
        Ir.Mem (Ir.BinOp (Ir.Plus, addr_orig_fp, Ir.Const offset), "")

  let max_formals_registers = 4

  let new_frame name _ formals =
    let rec alloc formals nfregs arg_offset =
      match (formals, nfregs, arg_offset) with
      | [], _, _ -> []
      (* formal escapes, we need it on stack *)
      | true :: rest, nfregs, offset ->
          InFrame offset :: alloc rest nfregs (offset + wordsize)
      (* no more registers left for this argument, put on stack *)
      | false :: rest, 0, offset ->
          InFrame offset :: alloc rest 0 (offset + wordsize)
      (* doesn't escape, we can put it in a fresh register *)
      | false :: rest, nfregs, offset ->
          InReg (Temp.newtemp ()) :: alloc rest (nfregs - 1) offset
    in
    {name; formals = alloc formals max_formals_registers 0; sp_offset = 0}

  let name {name; _} = name
  let formals {formals; _} = formals

  let alloc_local f _ = function
    | false -> InReg (Temp.newtemp ()) (* doesn't escape *)
    | true ->
        (* Escapes; allocate on the stack.
           SP          old SP               FP
           | new local | ...existing locals | ...incoming args
            -----------
            [wordsize]
        *)
        f.sp_offset <- f.sp_offset - wordsize;
        InFrame f.sp_offset

  let external_call _ fn args = Ir.Call (Ir.Name fn, args)

  let codegen _f stmt =
    let open Temp in
    let module A = Assem in
    let ice why = failwith ("ICE (mips_codegen): " ^ why) in
    let ilist = ref [] in
    let emit instr = ilist := instr :: !ilist in
    let alloc gen =
      let t = newtemp () in
      gen t;
      t
    in
    let alloc_oper assem src jmp =
      alloc (fun d -> emit (A.Oper {assem; dst = [d]; src; jmp; comments = []}))
    in
    let emit_jmp assem src jmp =
      emit (A.Oper {assem; dst = []; src; jmp = Some jmp; comments = []})
    in
    (* Maximal Munch *)
    let rec munch_stmt = function
      | Ir.Label lab ->
          emit (A.Label {assem = string_of_label lab ^ ":"; lab; comments = []})
      | Ir.Expr e ->
          let _ = munch_expr e in
          ()
      (********)
      (* Move *)
      (********)
      (* TODO: more analysis to detect conditional moves. *)
      | Ir.Mov (Ir.Temp where, Ir.Const what, _) ->
          emit
            (A.Oper
               { assem = Printf.sprintf "li `d0, %d" what
               ; dst = [where]
               ; src = []
               ; jmp = None
               ; comments = [] } )
      | Ir.Mov
          (Ir.Temp where, Ir.Mem (Ir.BinOp (Ir.Plus, s0, Ir.Const off), _), _)
       |Ir.Mov
          (Ir.Temp where, Ir.Mem (Ir.BinOp (Ir.Plus, Ir.Const off, s0), _), _)
        ->
          emit
            (A.Mov
               { assem = Printf.sprintf "lw `d0, %d(`s0)" off
               ; dst = where
               ; src = munch_expr s0
               ; comments = [] } )
      | Ir.Mov (where, what, _) ->
          emit
            (A.Mov
               { assem = "move `d0, `s0"
               ; dst = munch_expr where
               ; src = munch_expr what
               ; comments = [] } )
      (********)
      (* Jump *)
      (********)
      | Ir.Jmp (Ir.Name lab, _) -> emit_jmp "j `j0" [] [lab]
      | Ir.Jmp (e, lands) -> emit_jmp "jr `s0" [munch_expr e] lands
      | Ir.CJmp (Ir.Lt, e1, Ir.Const 0, t, f, _)
       |Ir.CJmp (Ir.Gt, Ir.Const 0, e1, t, f, _) ->
          (* Store both possible landing labels for flow analysis.
             During emit, only the "true" label will be filled in, and
             canonicalization has already ensured that this [CJump] is
             followed by the "false" label. *)
          emit_jmp "bltz `s0, `j0" [munch_expr e1] [t; f]
      | Ir.CJmp (Ir.Gt, e1, Ir.Const 0, t, f, _)
       |Ir.CJmp (Ir.Lt, Ir.Const 0, e1, t, f, _) ->
          emit_jmp "bgtz `s0, `j0" [munch_expr e1] [t; f]
      | Ir.CJmp (Ir.Leq, e1, Ir.Const 0, t, f, _)
       |Ir.CJmp (Ir.Geq, Ir.Const 0, e1, t, f, _) ->
          emit_jmp "blez `s0, `j0" [munch_expr e1] [t; f]
      | Ir.CJmp (Ir.Geq, e1, Ir.Const 0, t, f, _)
       |Ir.CJmp (Ir.Leq, Ir.Const 0, e1, t, f, _) ->
          emit_jmp "bgez `s0, `j0" [munch_expr e1] [t; f]
      | Ir.CJmp (Ir.Neq, e1, e2, t, f, _) ->
          emit_jmp "bne `s0, `s1, `j0" [munch_expr e1; munch_expr e2] [t; f]
      | Ir.CJmp (Ir.Eq, e1, e2, t, f, _) ->
          emit_jmp "beq `s0, `s1, `j0" [munch_expr e1; munch_expr e2] [t; f]
      | Ir.CJmp (Ir.Lt, e1, e2, t, f, _) ->
          emit_jmp "blt `s0, `s1, `j0" [munch_expr e1; munch_expr e2] [t; f]
      | Ir.CJmp (Ir.Gt, e1, e2, t, f, _) ->
          emit_jmp "bgt `s0, `s1, `j0" [munch_expr e1; munch_expr e2] [t; f]
      | Ir.CJmp (Ir.Leq, e1, e2, t, f, _) ->
          emit_jmp "ble `s0, `s1, `j0" [munch_expr e1; munch_expr e2] [t; f]
      | Ir.CJmp (Ir.Geq, e1, e2, t, f, _) ->
          emit_jmp "bge `s0, `s1, `j0" [munch_expr e1; munch_expr e2] [t; f]
      | Ir.CJmp (Ir.Ult, e1, e2, t, f, _) ->
          emit_jmp "bltu `s0, `s1, `j0" [munch_expr e1; munch_expr e2] [t; f]
      | Ir.CJmp (Ir.Ule, e1, e2, t, f, _) ->
          emit_jmp "bleu `s0, `s1, `j0" [munch_expr e1; munch_expr e2] [t; f]
      | Ir.CJmp (Ir.Ugt, e1, e2, t, f, _) ->
          emit_jmp "bgtu `s0, `s1, `j0" [munch_expr e1; munch_expr e2] [t; f]
      | Ir.CJmp (Ir.Uge, e1, e2, t, f, _) ->
          emit_jmp "bgeu `s0, `s1, `j0" [munch_expr e1; munch_expr e2] [t; f]
      | Ir.Seq _ -> ice "Seqs should be destroyed during canonicalization"
    and munch_expr = function
      (**************)
      (* Arithmetic *)
      (**************)
      | Ir.BinOp (Ir.Plus, s0, Ir.Const s1) | Ir.BinOp (Ir.Plus, Ir.Const s1, s0)
        ->
          alloc_oper ("addi `d0, `s0, " ^ string_of_int s1) [munch_expr s0] None
      | Ir.BinOp (Ir.Plus, s0, s1) ->
          alloc_oper "add `d0, `s0, `s1" [munch_expr s0; munch_expr s1] None
      | Ir.BinOp (Ir.Minus, s0, s1) ->
          alloc_oper "sub `d0, `s0, `s1" [munch_expr s0; munch_expr s1] None
      | Ir.BinOp (Ir.Mul, s0, s1) ->
          alloc_oper "mul `d0, `s0, `s1" [munch_expr s0; munch_expr s1] None
      | Ir.BinOp (Ir.Div, s0, s1) ->
          (* TODO: dest of this should actually be LO *)
          alloc_oper "div `d0, `s0, `s1" [munch_expr s0; munch_expr s1] None
      (*********)
      (* Logic *)
      (*********)
      | Ir.BinOp (Ir.And, s0, Ir.Const s1) | Ir.BinOp (Ir.And, Ir.Const s1, s0)
        ->
          alloc_oper ("andi `d0, `s0, " ^ string_of_int s1) [munch_expr s0] None
      | Ir.BinOp (Ir.And, s0, s1) ->
          alloc_oper "and `d0, `s0, `s1" [munch_expr s0; munch_expr s1] None
      | Ir.BinOp (Ir.Or, s0, Ir.Const s1) | Ir.BinOp (Ir.Or, Ir.Const s1, s0) ->
          alloc_oper ("ori `d0, `s0, " ^ string_of_int s1) [munch_expr s0] None
      | Ir.BinOp (Ir.Or, s0, s1) ->
          alloc_oper "or `d0, `s0, `s1" [munch_expr s0; munch_expr s1] None
      | Ir.BinOp (Ir.Xor, s0, Ir.Const s1) | Ir.BinOp (Ir.Xor, Ir.Const s1, s0)
        ->
          alloc_oper ("xori `d0, `s0, " ^ string_of_int s1) [munch_expr s0] None
      | Ir.BinOp (Ir.Xor, s0, s1) ->
          alloc_oper "xor `d0, `s0, `s1" [munch_expr s0; munch_expr s1] None
      | Ir.BinOp (Ir.Shl, s0, Ir.Const s1) | Ir.BinOp (Ir.Shl, Ir.Const s1, s0)
        ->
          alloc_oper ("sll `d0, `s0, " ^ string_of_int s1) [munch_expr s0] None
      | Ir.BinOp (Ir.Shl, s0, s1) ->
          alloc_oper "sllv `d0, `s0, `s1" [munch_expr s0; munch_expr s1] None
      | Ir.BinOp (Ir.Shr, s0, Ir.Const s1) | Ir.BinOp (Ir.Shr, Ir.Const s1, s0)
        ->
          alloc_oper ("srl `d0, `s0, " ^ string_of_int s1) [munch_expr s0] None
      | Ir.BinOp (Ir.Shr, s0, s1) ->
          alloc_oper "srlv `d0, `s0, `s1" [munch_expr s0; munch_expr s1] None
      | Ir.BinOp (Ir.Sar, s0, Ir.Const s1) | Ir.BinOp (Ir.Sar, Ir.Const s1, s0)
        ->
          alloc_oper ("sra `d0, `s0, " ^ string_of_int s1) [munch_expr s0] None
      | Ir.BinOp (Ir.Sar, s0, s1) ->
          alloc_oper "srav `d0, `s0, `s1" [munch_expr s0; munch_expr s1] None
      (*********)
      (* Loads *)
      (*********)
      | Ir.Temp t -> t
      | Ir.Const n -> alloc_oper ("li `d0, " ^ string_of_int n) [] None
      | Ir.Name lab -> alloc_oper ("la `d0, " ^ string_of_label lab) [] None
      | Ir.Mem (Ir.BinOp (Ir.Plus, s0, Ir.Const off), _)
       |Ir.Mem (Ir.BinOp (Ir.Plus, Ir.Const off, s0), _) ->
          alloc_oper (Printf.sprintf "lw `d0, %d(`s0)" off) [munch_expr s0] None
      | Ir.Mem (s0, _) -> alloc_oper "lw `d0, 0(`s0)" [munch_expr s0] None
      (********)
      (* Call *)
      (********)
      | Ir.Call (fn, args) ->
          let temp_store = List.map (fun r -> (newtemp (), r)) caller_saves in
          let mov where what = Ir.Mov (Ir.Temp where, Ir.Temp what, "") in
          (* Save caller-save registers *)
          List.iter (fun (save, reg) -> munch_stmt (mov save reg)) temp_store;
          let fn = munch_expr fn in
          emit
            (A.Oper
               { assem = "jalr `s0"
               ; src = fn :: munch_args args
               ; dst = calldefs
               ; jmp = None
               ; comments = [] } );
          (* Restore caller-save registers *)
          List.iter (fun (save, reg) -> munch_stmt (mov reg save)) temp_store;
          rv
      | Ir.ESeq _ -> ice "ESeqs should be eliminated during canonicalization"
    and munch_args args =
      (* TODO: need to actually allocate incoming args as we did on the frame. *)
      List.map munch_expr args
    in
    munch_stmt stmt;
    List.rev !ilist

  let proc_entry_exit1 _ body = body

  let proc_entry_exit2 _ body =
    body
    @ [ Assem.Oper
          { assem = ""
          ; (* Mark registers as still being live (necessary) at procedure exit. *)
            src = [zero; ra; sp] @ callee_saves
          ; dst = []
          ; jmp = None
          ; comments = [] } ]

  let proc_entry_exit3 {name; _} body =
    { prolog = "PROCEDURE " ^ Temp.string_of_label name ^ "\n"
    ; body
    ; epilog = "END " ^ Temp.string_of_label name ^ "\n" }

  type allocation = (Temp.temp, register) Hashtbl.t

  let emit _ _ = failwith "todo"
  let reserved_temps = [] (* TODO *)

  let reserved_labels = [] (* TODO *)

  let fetch_from_access _ = failwith "todo"
  let store_to_access _ = failwith "todo"

  module Debug = struct let emit_debug _ _ _ = failwith "todo" end
end
