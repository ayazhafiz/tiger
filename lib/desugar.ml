open Language
open Symbol

type desugared_expr = expr

let expr_of_desugared expr = expr

let while_of_for i escape lo hi body =
  (* page 166: rewrite
       for i := lo to hi do body
     as
       let
           var i := lo
           var limit := hi
       in  while i <= limit do
             ...body;
             i := i + 1
     TODO: handle case where limit=MAXINT
  *)
  let limit = symbol "limit" in
  let dvar_i = VarDecl { name = i; escape; typ = None; init = lo } in
  let dvar_limit =
    VarDecl { name = limit; escape = ref false; typ = None; init = hi }
  in
  let rt_int = ref (Some Type.Int) in
  let rt_unit = ref (Some Type.Unit) in
  let var_i = VarExpr (SimpleVar (i, rt_int), rt_int) in
  let var_limit = VarExpr (SimpleVar (limit, rt_int), rt_int) in
  let while_test =
    OpExpr { left = var_i; oper = LeOp; right = var_limit; ty = rt_int }
  in
  let i_plus_1 =
    OpExpr { left = var_i; oper = PlusOp; right = IntExpr 1; ty = rt_int }
  in
  let incr_i = AssignExpr { var = SimpleVar (i, rt_int); expr = i_plus_1 } in
  let while_body = SeqExpr ([ body; incr_i ], rt_unit) in
  LetExpr
    {
      decls = [ dvar_i; dvar_limit ];
      body = WhileExpr { test = while_test; body = while_body };
      ty = rt_unit;
    }

let rec desugar_var = function
  | SimpleVar (v, ty) -> SimpleVar (v, ty)
  | FieldVar (v, f, ty) -> FieldVar (v, f, ty)
  | SubscriptVar (v, idx, ty) -> SubscriptVar (v, desugar_expr idx, ty)

and desugar_expr = function
  | NilExpr -> NilExpr
  | VarExpr (v, ty) -> VarExpr (desugar_var v, ty)
  | IntExpr n -> IntExpr n
  | StringExpr s -> StringExpr s
  | CallExpr { func; args; ty } ->
      CallExpr { func; args = List.map desugar_expr args; ty }
  | OpExpr { left; oper; right; ty } ->
      OpExpr { left = desugar_expr left; oper; right = desugar_expr right; ty }
  | RecordExpr { typ; fields; ty } ->
      RecordExpr
        {
          typ;
          fields = List.map (fun (s, e) -> (s, desugar_expr e)) fields;
          ty;
        }
  | SeqExpr (exprs, ty) -> SeqExpr (List.map desugar_expr exprs, ty)
  | AssignExpr { var; expr } -> AssignExpr { var; expr = desugar_expr expr }
  | IfExpr { test; then'; else'; ty } ->
      IfExpr
        {
          test = desugar_expr test;
          then' = desugar_expr then';
          else' = Option.map desugar_expr else';
          ty;
        }
  | WhileExpr { test : expr; body : expr } ->
      WhileExpr { test = desugar_expr test; body = desugar_expr body }
  | ForExpr { var = i; escape; lo; hi; body } ->
      while_of_for i escape lo hi body
  | BreakExpr -> BreakExpr
  | LetExpr { decls; body; ty } ->
      LetExpr
        { decls = List.map desugar_decl decls; body = desugar_expr body; ty }
  | ArrayExpr { typ; size; init; ty } ->
      ArrayExpr { typ; size = desugar_expr size; init = desugar_expr init; ty }

and desugar_decl = function
  | FunctionDecl decls ->
      FunctionDecl
        (List.map
           (fun { fn_name; params; result; body } ->
             { fn_name; params; result; body = desugar_expr body })
           decls)
  | VarDecl { name; escape; typ; init } ->
      VarDecl { name; escape; typ; init = desugar_expr init }
  | TypeDecl decls -> TypeDecl decls
