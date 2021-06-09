module LS = Temp.LabelSet

let ice why = failwith ("ICE (canon): " ^ why)

(*****************)
(* Linearization *)
(*****************)

(** Naive estimation of whether a statement and expression can commute in
    evaluation. *)
let commutes = function
  | Ir.Expr (Ir.Const _), _ -> true
  | _, Ir.Name _ -> true
  | _, Ir.Const _ -> true
  | _ -> false

let nop = Ir.Expr (Ir.Const 0)
let seq lst = List.filter (fun s -> s <> nop) lst |> Ir.seq

let rec reorder = function
  | [] -> (nop, [])
  | (Ir.Call _ as e) :: rest ->
      (* Move call to toplevel (page 178): wrap call in ESeq, which will then get
         percolated upwards. *)
      let t = Temp.newtemp () in
      let wrap = Ir.ESeq (Ir.Mov (Ir.Temp t, e, "push call up"), Ir.Temp t) in
      reorder (wrap :: rest)
  | e :: rest ->
      let s, e = do_expr e in
      let rest_s, rest_e = reorder rest in
      (* At this point we have that the order of computation is:
           s e rest_s rest_e
         We want to push the "rest_s" statement upwards. *)
      if commutes (rest_s, e) then (seq [s; rest_s], e :: rest_e)
      else
        (* We can't push it up immediately, so treat "e" itself as a statement. *)
        let te = Temp.newtemp () in
        (* s ; Mov (te, e) ; rest_s *)
        let stmts = seq [s; Ir.Mov (Ir.Temp te, e, "push stmt up"); rest_s] in
        (stmts, Ir.Temp te :: rest_e)

and reorder_stmt subexprs build =
  let stmts, exprs = reorder subexprs in
  seq [stmts; build exprs]

and reorder_expr subexprs build =
  let stmts, exprs = reorder subexprs in
  (stmts, build exprs)

and do_stmt = function
  | Ir.Seq (s1, s2) -> seq [do_stmt s1; do_stmt s2]
  | Ir.Jmp (e, labs) ->
      reorder_stmt [e] (function [e] -> Ir.Jmp (e, labs) | _ -> ice "bad args")
  | Ir.CJmp (op, e1, e2, t, f, cmt) ->
      reorder_stmt [e1; e2] (function
        | [e1; e2] -> Ir.CJmp (op, e1, e2, t, f, cmt)
        | _ -> ice "bad args" )
  | Ir.Mov (Ir.Temp t, Ir.Call (f, args), cmt) ->
      (* Keep call on toplevel *)
      reorder_stmt (f :: args) (function
        | f :: args -> Ir.Mov (Ir.Temp t, Ir.Call (f, args), cmt)
        | _ -> ice "bad args" )
  | Ir.Mov (Ir.Temp t, e, cmt) ->
      reorder_stmt [e] (function
        | [e] -> Ir.Mov (Ir.Temp t, e, cmt)
        | _ -> ice "bad args" )
  | Ir.Mov (Ir.Mem (m, cmt1), e, cmt2) ->
      reorder_stmt [m; e] (function
        | [m; e] -> Ir.Mov (Ir.Mem (m, cmt1), e, cmt2)
        | _ -> ice "bad args" )
  | Ir.Mov (Ir.ESeq (s, e), e2, cmt) -> do_stmt (Ir.Seq (s, Ir.Mov (e, e2, cmt)))
  | Ir.Expr (Ir.Call (f, args)) ->
      (* Keep call on toplevel *)
      reorder_stmt (f :: args) (function
        | f :: args -> Ir.Expr (Ir.Call (f, args))
        | _ -> ice "bad args" )
  | Ir.Expr e ->
      reorder_stmt [e] (function [e] -> Ir.Expr e | _ -> ice "bad args")
  | s -> reorder_stmt [] (fun _ -> s)

and do_expr = function
  | Ir.Const n -> (nop, Ir.Const n)
  | Ir.Name lab -> (nop, Ir.Name lab)
  | Ir.Temp t -> (nop, Ir.Temp t)
  | Ir.BinOp (op, l, r) ->
      reorder_expr [l; r] (function
        | [l; r] -> Ir.BinOp (op, l, r)
        | _ -> ice "bad args" )
  | Ir.Mem (e, cmt) ->
      reorder_expr [e] (function [e] -> Ir.Mem (e, cmt) | _ -> ice "bad args")
  | Ir.Call (t, args) ->
      reorder_expr (t :: args) (function
        | t :: args -> Ir.Call (t, args)
        | _ -> ice "bad args" )
  | Ir.ESeq (s, e) ->
      let s = do_stmt s in
      let s', e = do_expr e in
      (seq [s; s'], e)

let linearize stmt =
  let rec linear = function
    | Ir.Seq (s1, s2), l -> linear (s1, linear (s2, l))
    | s1, s2 -> s1 :: s2
  in
  linear (do_stmt stmt, [])

(****************)
(* Basic Blocks *)
(****************)

let basic_blocks stmts =
  let finish = Temp.newlabel "done" in
  let rec partition curblock = function
    | [Ir.Jmp _] | [Ir.CJmp _] ->
        ice "unexpected jump at end of final basic block"
    | [] ->
        (* End of stmts, add jump to [finish] *)
        let block = List.rev (Ir.Jmp (Ir.Name finish, [finish]) :: curblock) in
        [block]
    | ((Ir.Jmp _ as fin) | (Ir.CJmp _ as fin)) :: rest ->
        (* Finish current basic block.
           Reverse because we appended stmts to front. *)
        let block = List.rev (fin :: curblock) in
        block :: partition [] rest
    | Ir.Label lab :: _ as stmts when curblock <> [] ->
        (* Current basic block has no terminating jump; add one. *)
        partition curblock (Ir.Jmp (Ir.Name lab, [lab]) :: stmts)
    | (Ir.Label _ as hd) :: rest when curblock = [] ->
        (* Start of new basic block *)
        partition [hd] rest
    | stmts when curblock = [] ->
        (* Start of new basic block has no label; add one. *)
        partition [] (Ir.Label (Temp.newlabel "basic_block") :: stmts)
    | s :: rest -> partition (s :: curblock) rest
  in
  (partition [] stmts, finish)

(********************)
(* Trace Scheduling *)
(********************)

let rec splitlast = function
  | [] -> ice "empty list"
  | [x] -> ([], x)
  | h :: rest ->
      let hd, last = splitlast rest in
      (h :: hd, last)

let labelof = function
  | Ir.Label lab :: _ -> lab
  | _ -> ice "mishaped basic block: doesn't start with label"

let trace_schedule (basic_blocks, finish) =
  let basic_blocks = List.map (fun b -> (labelof b, b)) basic_blocks in
  let todo = List.to_seq basic_blocks |> Hashtbl.of_seq in
  let rec trace block =
    let lab = labelof block in
    Hashtbl.remove todo lab;
    match splitlast block with
    | front, Ir.Jmp (_, nextlabs) -> (
      match
        (List.length nextlabs, List.find_map (Hashtbl.find_opt todo) nextlabs)
      with
      | _, None -> block
      (* If this is the only label we can jump to, the jump can be eliminated
         in favor of a fall-through. *)
      | 1, Some nextblock -> front @ trace nextblock
      | _, Some nextblock -> block @ trace nextblock )
    | front, Ir.CJmp (op, e1, e2, t, f, cmt) -> (
      match (Hashtbl.find_opt todo t, Hashtbl.find_opt todo f) with
      (* Arrange so that false label follows cjump *)
      | _, Some falseblock -> block @ trace falseblock
      (* False label can never follow cjump, but true label can;
         negate the conditional, so that the true label is now the false label. *)
      | Some trueblock, _ ->
          let cjmp' =
            Ir.CJmp (Ir.not_relop op, e1, e2, f, t, Printf.sprintf "!(%s)" cmt)
          in
          front @ [cjmp'] @ trace trueblock
      (* We can't find a false or true label to schedule.
         Instead, make up a dummy "false bridge" that will jump immediately to
         the real false label. *)
      | _, _ ->
          let f' = Temp.newlabel "false_bridge" in
          let cjmp' = Ir.CJmp (op, e1, e2, t, f', "") in
          front @ [cjmp'; Ir.Label f'; Ir.Jmp (Ir.Name f, [f])] )
    | _, _ -> ice "mishaped basic block: doesn't end with label"
  in
  let rec schedule = function
    | [] -> []
    | (lab, block) :: rest when Hashtbl.mem todo lab ->
        let curtrace = trace block in
        curtrace @ schedule rest
    | _ :: rest -> schedule rest
  in
  schedule basic_blocks @ [Ir.Label finish]

(************)
(* Simplify *)
(************)

(** Removes jumps whose target is the immediate next instruction. *)
let rec eliminate_trivial_jumps = function
  | [] -> []
  | [one] -> [one]
  | Ir.Jmp (_, [l1]) :: Ir.Label l2 :: rest when l1 = l2 ->
      eliminate_trivial_jumps rest
  | i :: rest -> i :: eliminate_trivial_jumps rest

(** Removes labels that are... unused. *)
let eliminate_unused_labels instrs =
  let module LT = Temp.LabelHashtbl in
  let unused_labels = LT.create 8 in
  (* Populate *)
  List.iter
    (function
      | Ir.Label lab ->
          assert (not (LT.mem unused_labels lab));
          LT.add unused_labels lab ()
      | _ -> () )
    instrs;
  (* Mark used labels *)
  let rec expr = function
    | Ir.Const _ | Ir.Temp _ -> []
    | Ir.Name lab -> [lab]
    | Ir.BinOp (_, e1, e2) -> expr e1 @ expr e2
    | Ir.Mem (e, _) -> expr e
    | Ir.Call (f, args) -> expr f @ List.concat_map expr args
    | Ir.ESeq _ -> ice "should be eliminated by linearize"
  and stmt = function
    | Ir.Expr e -> expr e
    | Ir.Mov (e1, e2, _) -> expr e1 @ expr e2
    | Ir.Jmp (e1, labs) -> expr e1 @ labs
    | CJmp (_, e1, e2, t, f, _) -> expr e1 @ expr e2 @ [t; f]
    | Seq _ -> ice "should be eliminated by linearize"
    | Label _ -> (* definition, not a use *) []
  in
  List.iter (fun s -> stmt s |> List.iter (LT.remove unused_labels)) instrs;
  (* Remove unused *)
  let rec finish = function
    | [] -> []
    | Ir.Label l :: rest when LT.mem unused_labels l -> finish rest
    | s :: rest -> s :: finish rest
  in
  finish instrs

(** Removes consecutive labels of the form
    label1:
    label2:
      ...code *)
let eliminate_consecutive_labels instrs =
  let rec partition cur_bucket buckets instrs = function
    | [] -> (buckets, instrs)
    | (Ir.Label lab as i) :: rest when cur_bucket = [] ->
        partition (lab :: cur_bucket) buckets (i :: instrs) rest
    | Ir.Label lab :: rest -> partition (lab :: cur_bucket) buckets instrs rest
    | i :: rest ->
        let buckets =
          if cur_bucket = [] then buckets else List.rev cur_bucket :: buckets
        in
        partition [] buckets (i :: instrs) rest
  in
  let buckets, instrs = partition [] [] [] instrs in
  let instrs = List.rev instrs in
  let renaming = Hashtbl.create (List.length buckets) in
  List.iter
    (fun bucket ->
      let name = List.hd bucket in
      List.iter (fun oldlab -> Hashtbl.add renaming oldlab name) bucket )
    buckets;
  let rn lab =
    match Hashtbl.find_opt renaming lab with
    | Some l -> l
    | None -> (* extern *) lab
  in
  (* Rewrite labels *)
  let rec expr e =
    match e with
    | Ir.Const _ | Ir.Temp _ -> e
    | Ir.Name lab -> Ir.Name (rn lab)
    | Ir.BinOp (op, e1, e2) -> Ir.BinOp (op, expr e1, expr e2)
    | Ir.Mem (e, cmt) -> Ir.Mem (expr e, cmt)
    | Ir.Call (f, args) -> Ir.Call (expr f, List.map expr args)
    | Ir.ESeq _ -> ice "should be eliminated by linearize"
  and stmt = function
    | Ir.Label l ->
        assert (rn l = l);
        Ir.Label l
    | Ir.Expr e -> Ir.Expr (expr e)
    | Ir.Mov (e1, e2, cmt) -> Ir.Mov (expr e1, expr e2, cmt)
    | Ir.Jmp (e1, labs) ->
        let targets =
          List.map rn labs |> LS.of_list |> LS.to_seq |> List.of_seq
        in
        Ir.Jmp (expr e1, targets)
    | Ir.CJmp (op, e1, e2, t, f, cmt) ->
        Ir.CJmp (op, expr e1, expr e2, rn t, rn f, cmt)
    | Ir.Seq _ -> ice "should be eliminated by linearize"
  in
  List.map stmt instrs

let simplify instrs =
  instrs
  (* Eliminating trivial jumps first may open up more unused labels. *)
  |> eliminate_trivial_jumps
  |> eliminate_unused_labels |> eliminate_consecutive_labels
