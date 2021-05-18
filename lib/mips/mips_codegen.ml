open Temp
module A = Assem

let ice why = failwith ("ICE (mips_codegen): " ^ why)

(** Reference:
    https://www.cs.cmu.edu/afs/cs/academic/class/15740-f97/public/doc/mips-isa.pdf
    https://www.dsi.unive.it/~gasparetto/materials/MIPS_Instruction_Set.pdf
    https://inst.eecs.berkeley.edu/~cs61c/resources/MIPS_help.html *)

module MipsCodegen : Codegen.CODEGEN = struct
  module Frame = Mips_frame.MipsFrame

  let codegen f stmt =
    let ilist = ref [] in
    let emit instr = ilist := instr :: !ilist in
    let alloc gen =
      let t = newtemp () in
      gen t;
      t in
    let alloc_oper assem src jmp =
      alloc (fun d -> emit (A.Oper {assem; dst = [d]; src; jmp})) in
    (* Maximal Munch *)
    let rec munch_stmt = function
      | Ir.Label lab -> emit (A.Label {assem = string_of_label lab ^ ":"; lab})
    and munch_expr expr =
      match expr with
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
      | Ir.Const n -> alloc_oper ("li `d0, " ^ string_of_int n) [] None
      | Ir.Name lab -> alloc_oper ("la `d0, " ^ string_of_label lab) [] None
      | Ir.Temp t -> t
      | Ir.Mem addr
    in
    munch_stmt stmt;
    List.rev !ilist
end
