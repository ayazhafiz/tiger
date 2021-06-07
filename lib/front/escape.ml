open Language
module Tbl = Symbol.Table

let ice why = failwith ("ICE (escape): " ^ why)

let mark_above env d' sym =
  match Tbl.find_opt env sym with
  | None (* variable not declared, we should have caught this during sema *) ->
      ice "variable not declared"
  | Some (d, _) when d = d' -> ()
  | Some (d, _) when d > d' -> ice "declared depth greater than current"
  | Some (_, escape) -> escape := true

let rec walk_var env d = function
  | SimpleVar (sym, _) -> mark_above env d sym
  | FieldVar (v, _, _) | SubscriptVar (v, _, _) -> walk_var env d v

and walk_expr env d = function
  | VarExpr (v, _) -> walk_var env d v
  | NilExpr | IntExpr _ | StringExpr _ | BreakExpr -> ()
  | CallExpr {args; _} -> List.iter (walk_expr env d) args
  | OpExpr {left; right; _} ->
      walk_expr env d left;
      walk_expr env d right
  | RecordExpr {fields; _} -> List.map snd fields |> List.iter (walk_expr env d)
  | SeqExpr (exprs, _) -> List.iter (walk_expr env d) exprs
  | AssignExpr {var; expr} ->
      walk_expr env d expr;
      walk_var env d var
  | IfExpr {test; then'; else' = None; _} ->
      List.iter (walk_expr env d) [test; then']
  | IfExpr {test; then'; else' = Some else'; _} ->
      List.iter (walk_expr env d) [test; then'; else']
  | WhileExpr {test; body} -> List.iter (walk_expr env d) [test; body]
  | ForExpr {var; escape; lo; hi; body} ->
      escape := false;
      Tbl.scoped env (fun env ->
          Tbl.add env var (d, escape);
          List.iter (walk_expr env d) [lo; hi; body] )
  | LetExpr {decls; body; _} ->
      Tbl.scoped env (fun env ->
          List.iter (walk_decl env d) decls;
          walk_expr env d body )
  | ArrayExpr {size; init; _} -> List.iter (walk_expr env d) [size; init]

and add_field env d {fld_name; escape; _} =
  escape := false;
  Tbl.add env fld_name (d, escape)

and walk_fn env d {params; body; _} =
  let d' = succ d in
  Tbl.scoped env (fun env ->
      List.iter (add_field env d') params;
      walk_expr env d' body )

and walk_decl env d = function
  | FunctionDecl fns -> List.iter (walk_fn env d) fns
  | VarDecl {name; escape; init; _} ->
      walk_expr env d init;
      escape := false;
      Tbl.add env name (d, escape)
  | TypeDecl _ -> ()

let mark_escapes expr = walk_expr (Tbl.singleton ()) 0 expr
