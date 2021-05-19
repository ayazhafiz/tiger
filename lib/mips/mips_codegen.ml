open Temp
module A = Assem

let ice why = failwith ("ICE (mips_codegen): " ^ why)

(** Reference:
    https://www.cs.cmu.edu/afs/cs/academic/class/15740-f97/public/doc/mips-isa.pdf
    https://www.dsi.unive.it/~gasparetto/materials/MIPS_Instruction_Set.pdf
    https://inst.eecs.berkeley.edu/~cs61c/resources/MIPS_help.html *)

module MipsCodegen : Codegen.CODEGEN = struct
  module Frame = Mips_frame.MipsFrame

  let codegen _f stmt =
    let ilist = ref [] in
    let emit instr = ilist := instr :: !ilist in
    let alloc gen =
      let t = newtemp () in
      gen t;
      t
    in
    let alloc_oper assem src jmp =
      alloc (fun d -> emit (A.Oper {assem; dst = [d]; src; jmp}))
    in
    let emit_jmp assem src jmp =
      emit (A.Oper {assem; dst = []; src; jmp = Some jmp})
    in
    (* Registers trashed (definitely used) by a function inside a call. *)
    let calldefs = Frame.rv :: Frame.ra :: Frame.caller_saves in
    (* Maximal Munch *)
    let rec munch_stmt = function
      | Ir.Label lab -> emit (A.Label {assem = string_of_label lab ^ ":"; lab})
      | Ir.Expr e ->
          let _ = munch_expr e in
          ()
      (********)
      (* Move *)
      (********)
      (* TODO: more analysis to detect conditional moves. *)
      | Ir.Mov (Ir.Temp where, Ir.Const what) ->
          emit
            (A.Oper
               { assem = Printf.sprintf "li `d0, %d" what
               ; dst = [where]
               ; src = []
               ; jmp = None })
      | Ir.Mov (Ir.Temp where, Ir.Mem (Ir.BinOp (Ir.Plus, s0, Ir.Const off)))
       |Ir.Mov (Ir.Temp where, Ir.Mem (Ir.BinOp (Ir.Plus, Ir.Const off, s0))) ->
          emit
            (A.Mov
               { assem = Printf.sprintf "lw `d0, %d(`s0)" off
               ; dst = where
               ; src = munch_expr s0 })
      | Ir.Mov (where, what) ->
          emit
            (A.Mov
               { assem = "move `d0, `s0"
               ; dst = munch_expr where
               ; src = munch_expr what })
      (********)
      (* Jump *)
      (********)
      | Ir.Jmp (Ir.Name lab, _) -> emit_jmp "j `j0" [] [lab]
      | Ir.Jmp (e, lands) -> emit_jmp "jr `s0" [munch_expr e] lands
      | Ir.CJmp (Ir.Lt, e1, Ir.Const 0, t, f)
       |Ir.CJmp (Ir.Gt, Ir.Const 0, e1, t, f) ->
          (* Store both possible landing labels for flow analysis.
             During emit, only the "true" label will be filled in, and
             canonicalization has already ensured that this [CJump] is
             followed by the "false" label. *)
          emit_jmp "bltz `s0, `j0" [munch_expr e1] [t; f]
      | Ir.CJmp (Ir.Gt, e1, Ir.Const 0, t, f)
       |Ir.CJmp (Ir.Lt, Ir.Const 0, e1, t, f) ->
          emit_jmp "bgtz `s0, `j0" [munch_expr e1] [t; f]
      | Ir.CJmp (Ir.Leq, e1, Ir.Const 0, t, f)
       |Ir.CJmp (Ir.Geq, Ir.Const 0, e1, t, f) ->
          emit_jmp "blez `s0, `j0" [munch_expr e1] [t; f]
      | Ir.CJmp (Ir.Geq, e1, Ir.Const 0, t, f)
       |Ir.CJmp (Ir.Leq, Ir.Const 0, e1, t, f) ->
          emit_jmp "bgez `s0, `j0" [munch_expr e1] [t; f]
      | Ir.CJmp (Ir.Neq, e1, e2, t, f) ->
          emit_jmp "bne `s0, `s1, `j0" [munch_expr e1; munch_expr e2] [t; f]
      | Ir.CJmp (Ir.Eq, e1, e2, t, f) ->
          emit_jmp "beq `s0, `s1, `j0" [munch_expr e1; munch_expr e2] [t; f]
      | Ir.CJmp (Ir.Lt, e1, e2, t, f) ->
          emit_jmp "blt `s0, `s1, `j0" [munch_expr e1; munch_expr e2] [t; f]
      | Ir.CJmp (Ir.Gt, e1, e2, t, f) ->
          emit_jmp "bgt `s0, `s1, `j0" [munch_expr e1; munch_expr e2] [t; f]
      | Ir.CJmp (Ir.Leq, e1, e2, t, f) ->
          emit_jmp "ble `s0, `s1, `j0" [munch_expr e1; munch_expr e2] [t; f]
      | Ir.CJmp (Ir.Geq, e1, e2, t, f) ->
          emit_jmp "bge `s0, `s1, `j0" [munch_expr e1; munch_expr e2] [t; f]
      | Ir.CJmp (Ir.Ult, e1, e2, t, f) ->
          emit_jmp "bltu `s0, `s1, `j0" [munch_expr e1; munch_expr e2] [t; f]
      | Ir.CJmp (Ir.Ule, e1, e2, t, f) ->
          emit_jmp "bleu `s0, `s1, `j0" [munch_expr e1; munch_expr e2] [t; f]
      | Ir.CJmp (Ir.Ugt, e1, e2, t, f) ->
          emit_jmp "bgtu `s0, `s1, `j0" [munch_expr e1; munch_expr e2] [t; f]
      | Ir.CJmp (Ir.Uge, e1, e2, t, f) ->
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
      | Ir.Mem (Ir.BinOp (Ir.Plus, s0, Ir.Const off))
       |Ir.Mem (Ir.BinOp (Ir.Plus, Ir.Const off, s0)) ->
          alloc_oper (Printf.sprintf "lw `d0, %d(`s0)" off) [munch_expr s0] None
      | Ir.Mem s0 -> alloc_oper "lw `d0, 0(`s0)" [munch_expr s0] None
      (********)
      (* Call *)
      (********)
      | Ir.Call (fn, args) ->
          let temp_store =
            List.map (fun r -> (newtemp (), r)) Frame.caller_saves
          in
          let mov where what = Ir.Mov (Ir.Temp where, Ir.Temp what) in
          (* Save caller-save registers *)
          List.iter (fun (save, reg) -> munch_stmt (mov save reg)) temp_store;
          let fn = munch_expr fn in
          emit
            (A.Oper
               { assem = "jalr `s0"
               ; src = fn :: munch_args args
               ; dst = calldefs
               ; jmp = None });
          (* Restore caller-save registers *)
          List.iter (fun (save, reg) -> munch_stmt (mov reg save)) temp_store;
          Frame.rv
      | Ir.ESeq _ -> ice "ESeqs should be eliminated during canonicalization"
    and munch_args args =
      (* TODO: need to actually allocate incoming args as we did on the frame. *)
      List.map munch_expr args
    in
    munch_stmt stmt;
    List.rev !ilist
end
