open Language
open Symbol

let string_of_symbol = name

let rec string_of_var = function
  | SimpleVar s -> string_of_symbol s
  | FieldVar (v, s) ->
      Printf.sprintf "%s.%s" (string_of_var v) (string_of_symbol s)
  | SubscriptVar (v, s) ->
      Printf.sprintf "%s[%s]" (string_of_var v) (string_of_expr s)

and string_of_expr = function
  | NilExpr -> "nil"
  | VarExpr v -> string_of_var v
  | IntExpr n -> string_of_int n
  | StringExpr s -> Printf.sprintf "\"%s\"" (String.escaped s)
  | CallExpr { func; args } ->
      Printf.sprintf "%s(%s)" (string_of_symbol func)
        (List.map string_of_expr args |> String.concat ", ")
  | OpExpr { left; oper; right } ->
      Printf.sprintf "%s %s %s" (string_of_expr left) (string_of_oper oper)
        (string_of_expr right)
  | RecordExpr { typ; fields } ->
      Printf.sprintf "%s { %s }" (string_of_symbol typ)
        (List.map string_of_field fields |> String.concat ", ")
  | SeqExpr exprs -> List.map string_of_expr exprs |> String.concat "; "
  | AssignExpr { var; expr } ->
      Printf.sprintf "%s := %s" (string_of_var var) (string_of_expr expr)
  | IfExpr { test; then'; else' = None } ->
      Printf.sprintf "if %s then %s" (string_of_expr test)
        (string_of_expr then')
  | IfExpr { test; then'; else' = Some else' } ->
      Printf.sprintf "if %s then %s else %s" (string_of_expr test)
        (string_of_expr then') (string_of_expr else')
  | WhileExpr { test; body } ->
      Printf.sprintf "while %s do %s" (string_of_expr test)
        (string_of_expr body)
  | ForExpr { var; lo; hi; body; _ } ->
      Printf.sprintf "for %s := %s to %s do %s" (string_of_symbol var)
        (string_of_expr lo) (string_of_expr hi) (string_of_expr body)
  | BreakExpr -> "break"
  | LetExpr { decls; body } ->
      Printf.sprintf "let %s in %s"
        (List.map string_of_decl decls |> String.concat "\n")
        (string_of_expr body)
  | ArrayExpr { typ; size; init } ->
      Printf.sprintf "%s[%s] of %s" (string_of_symbol typ) (string_of_expr size)
        (string_of_expr init)

and string_of_decl = function
  | FunctionDecl decls -> List.map string_of_fun decls |> String.concat "\n"
  | VarDecl { name; typ = None; init; _ } ->
      Printf.sprintf "var %s := %s" (string_of_symbol name)
        (string_of_expr init)
  | VarDecl { name; typ = Some typ; init; _ } ->
      Printf.sprintf "var %s : %s := %s" (string_of_symbol name)
        (string_of_symbol typ) (string_of_expr init)
  | TypeDecl sigs -> List.map string_of_sig sigs |> String.concat "\n"

and string_of_ty = function
  | NameTy sym -> string_of_symbol sym
  | RecordTy fields ->
      Printf.sprintf "{ %s }"
        (List.map string_of_field_ty fields |> String.concat ", ")
  | ArrayTy sym -> Printf.sprintf "array of %s" (string_of_symbol sym)

and string_of_oper = function
  | PlusOp -> "+"
  | MinusOp -> "-"
  | TimesOp -> "*"
  | DivideOp -> "/"
  | EqOp -> "="
  | NeqOp -> "<>"
  | LtOp -> "<"
  | LeOp -> "<="
  | GtOp -> ">"
  | GeOp -> ">="

and string_of_sig { name; ty } =
  Printf.sprintf "type %s = %s" (string_of_symbol name) (string_of_ty ty)

and string_of_field (fld, expr) =
  Printf.sprintf "%s=%s" (string_of_symbol fld) (string_of_expr expr)

and string_of_field_ty { fld_name; typ; _ } =
  Printf.sprintf "%s: %s" (string_of_symbol fld_name) (string_of_symbol typ)

and string_of_fun = function
  | { fn_name; params; result = None; body } ->
      Printf.sprintf "function %s(%s) = %s" (string_of_symbol fn_name)
        (List.map string_of_field_ty params |> String.concat ", ")
        (string_of_expr body)
  | { fn_name; params; result = Some result; body } ->
      Printf.sprintf "function %s(%s): %s = %s" (string_of_symbol fn_name)
        (List.map string_of_field_ty params |> String.concat ", ")
        (string_of_symbol result) (string_of_expr body)
