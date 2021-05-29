open Temp
module A = Assem

let ice why = failwith ("ICE (x86_64_codegen): " ^ why)

(** Reference:
    https://www.cs.virginia.edu/~evans/cs216/guides/x86.html *)

module X86_64_Codegen : Codegen.CODEGEN = struct
  module Frame = X86_64_frame.X86_64_Frame

  let codegen _f stmt =
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
              let rax = Ir.Temp Frame.rax in
              (* 1. dividend must be placed in rax.
                 2. extend qword to octoword rdx:rax.
                 3. idiv divisor
                 4. retrieve quotient from rax.
                 No need to save/restore rax/rdx, as we should not assign any
                 values other than these to them at this time. The register
                 allocator will make suere rax/rdx are only used in places that
                 do not interfere with this anyway. *)
              munch_stmt (Ir.Mov (rax, s0));
              emit_oper "cqto" [Frame.rax; Frame.rdx] [Frame.rax];
              emit_oper "idiv `s0" [Frame.rax; Frame.rdx]
                [divisor; Frame.rax; Frame.rdx];
              munch_stmt (Ir.Mov (Ir.Temp result, rax)) )
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
               ; dst = Frame.calldefs
               ; jmp = None } );
          Frame.rv
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
               so handle rest of args first
            *)
            eat_stack argns;
            emit
              (A.Oper
                 { assem = "push `s0"
                 ; dst = [Frame.rsp]
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
      eat_argregs (args, Frame.arg_regs)
    in
    munch_stmt stmt;
    List.rev !ilist
end
