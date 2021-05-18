open Language
open Symbol

let string_of_symbol = name

let reflow indent what =
  let pre = String.make indent ' ' in
  String.split_on_char '\n' what |> List.map (( ^ ) pre) |> String.concat "\n"

let rec string_of_var (ind : int) = function
  | SimpleVar (s, _) -> string_of_symbol s
  | FieldVar (v, s, _) ->
      Printf.sprintf "%s.%s" (string_of_var ind v) (string_of_symbol s)
  | SubscriptVar (v, s, _) ->
      Printf.sprintf "%s[%s]" (string_of_var ind v) (string_of_ex ind s)

and string_of_ex (ind : int) = function
  | NilExpr -> "nil"
  | VarExpr (v, _) -> string_of_var ind v
  | IntExpr n -> string_of_int n
  | StringExpr s -> Printf.sprintf "\"%s\"" (String.escaped s)
  | CallExpr {func; args; _} ->
      Printf.sprintf "%s(%s)" (string_of_symbol func)
        (List.map (string_of_ex ind) args |> String.concat ", ")
  | OpExpr {left; oper; right; _} ->
      Printf.sprintf "%s %s %s" (string_of_ex ind left) (string_of_oper oper)
        (string_of_ex ind right)
  | RecordExpr {typ; fields; _} ->
      Printf.sprintf "%s {\n%s\n}" (string_of_symbol typ)
        ( List.map (string_of_field ind) fields
        |> String.concat ",\n" |> reflow ind )
  | SeqExpr (exprs, _) ->
      List.map (string_of_ex ind) exprs |> String.concat ";\n"
  | AssignExpr {var; expr} ->
      Printf.sprintf "%s := %s" (string_of_var ind var) (string_of_ex ind expr)
  | IfExpr {test; then'; else' = None; _} ->
      Printf.sprintf "if %s then\n%s" (string_of_ex ind test)
        (string_of_ex ind then' |> reflow ind)
  | IfExpr {test; then'; else' = Some else'; _} ->
      Printf.sprintf "if %s then\n%s\nelse\n%s" (string_of_ex ind test)
        (string_of_ex ind then' |> reflow ind)
        (string_of_ex ind else' |> reflow ind)
  | WhileExpr {test; body} ->
      Printf.sprintf "while %s do\n%s" (string_of_ex ind test)
        (string_of_ex ind body |> reflow ind)
  | ForExpr {var; lo; hi; body; _} ->
      Printf.sprintf "for %s := %s to %s do\n%s" (string_of_symbol var)
        (string_of_ex ind lo) (string_of_ex ind hi)
        (string_of_ex ind body |> reflow ind)
  | BreakExpr -> "break"
  | LetExpr {decls; body; _} ->
      Printf.sprintf "let\n%s\nin\n%s\nend"
        (List.map (string_of_decl ind) decls |> String.concat "\n" |> reflow ind)
        (string_of_ex ind body |> reflow ind)
  | ArrayExpr {typ; size; init; _} ->
      Printf.sprintf "%s[%s] of %s" (string_of_symbol typ)
        (string_of_ex ind size) (string_of_ex ind init)

and string_of_decl ind = function
  | FunctionDecl decls ->
      List.map (string_of_fun ind) decls |> String.concat "\n"
  | VarDecl {name; typ = None; init; _} ->
      Printf.sprintf "var %s := %s" (string_of_symbol name)
        (string_of_ex ind init)
  | VarDecl {name; typ = Some typ; init; _} ->
      Printf.sprintf "var %s : %s := %s" (string_of_symbol name)
        (string_of_symbol typ) (string_of_ex ind init)
  | TypeDecl sigs -> List.map (string_of_sig ind) sigs |> String.concat "\n"

and string_of_ty ind = function
  | NameTy sym -> string_of_symbol sym
  | RecordTy fields ->
      Printf.sprintf "{\n%s\n}"
        ( List.map (string_of_field_ty ind) fields
        |> String.concat ",\n" |> reflow ind )
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

and string_of_sig (ind : int) {name; ty} =
  Printf.sprintf "type %s = %s" (string_of_symbol name) (string_of_ty ind ty)

and string_of_field (ind : int) (fld, expr) =
  Printf.sprintf "%s=%s" (string_of_symbol fld) (string_of_ex ind expr)

and string_of_field_ty _ind {fld_name; typ; _} =
  Printf.sprintf "%s: %s" (string_of_symbol fld_name) (string_of_symbol typ)

and string_of_fun (ind : int) = function
  | {fn_name; params; result = None; body} ->
      Printf.sprintf "function %s(%s) =\n%s" (string_of_symbol fn_name)
        (List.map (string_of_field_ty ind) params |> String.concat ", ")
        (string_of_ex ind body |> reflow ind)
  | {fn_name; params; result = Some result; body} ->
      Printf.sprintf "function %s(%s): %s =\n%s" (string_of_symbol fn_name)
        (List.map (string_of_field_ty ind) params |> String.concat ", ")
        (string_of_symbol result)
        (string_of_ex ind body |> reflow ind)

let string_of_var = string_of_var 2
let string_of_expr = string_of_ex 2
let string_of_decl = string_of_decl 2
let string_of_ty = string_of_ty 2
let fmt_expr fmt e = Format.pp_print_string fmt (string_of_expr e)
