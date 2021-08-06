open Symbol

type realty = Type.ty option ref

type var =
  | SimpleVar of symbol * realty
  | FieldVar of var * symbol * realty
  | SubscriptVar of var * expr * realty

and expr =
  | NilExpr
  | VarExpr of var * realty
  | IntExpr of int
  | StringExpr of string
  | CallExpr of {func : symbol; args : expr list; ty : realty}
  | OpExpr of {left : expr; oper : oper; right : expr; ty : realty}
  | RecordExpr of {typ : symbol; fields : (symbol * expr) list; ty : realty}
  | SeqExpr of expr list * realty
  | AssignExpr of {var : var; expr : expr}
  | IfExpr of {test : expr; then' : expr; else' : expr option; ty : realty}
  | WhileExpr of {test : expr; body : expr}
  | ForExpr of
      {var : symbol; escape : bool ref; lo : expr; hi : expr; body : expr}
  | BreakExpr
  | LetExpr of {decls : decl list; body : expr; ty : realty}
  | ArrayExpr of {typ : symbol; size : expr; init : expr; ty : realty}

and decl =
  | FunctionDecl of fundecl list
  | VarDecl of
      { name : symbol
      ; escape : bool ref
      ; naked_rvalue : bool ref
      ; typ : symbol option
      ; init : expr }
  | TypeDecl of ty_alias list

and ty = NameTy of symbol | RecordTy of field_ty list | ArrayTy of symbol

and oper =
  | PlusOp
  | MinusOp
  | TimesOp
  | DivideOp
  | EqOp
  | NeqOp
  | LtOp
  | LeOp
  | GtOp
  | GeOp

and ty_alias = {name : symbol; ty : ty}

and field_ty = {fld_name : symbol; escape : bool ref; typ : symbol}

and fundecl =
  {fn_name : symbol; params : field_ty list; result : symbol option; body : expr}

let listeq pred l1 l2 =
  List.length l1 = List.length l2 && List.for_all2 pred l1 l2

let rec var_eq v1 v2 =
  match (v1, v2) with
  | SimpleVar (s1, _), SimpleVar (s2, _) -> symeq s1 s2
  | FieldVar (v1, s1, _), FieldVar (v2, s2, _) -> var_eq v1 v2 && symeq s1 s2
  | SubscriptVar (v1, e1, _), SubscriptVar (v2, e2, _) ->
      var_eq v1 v2 && expr_eq e1 e2
  | _ -> false

and expr_eq e1 e2 =
  match (e1, e2) with
  | NilExpr, NilExpr -> true
  | VarExpr (v1, _), VarExpr (v2, _) -> var_eq v1 v2
  | IntExpr n1, IntExpr n2 -> n1 = n2
  | StringExpr s1, StringExpr s2 -> s1 = s2
  | ( CallExpr {func = f1; args = a1; ty = _}
    , CallExpr {func = f2; args = a2; ty = _} ) ->
      symeq f1 f2 && listeq expr_eq a1 a2
  | ( OpExpr {left = l1; oper = o1; right = r1; ty = _}
    , OpExpr {left = l2; oper = o2; right = r2; ty = _} ) ->
      expr_eq l1 l2 && o1 = o2 && expr_eq r1 r2
  | ( RecordExpr {typ = t1; fields = f1; ty = _}
    , RecordExpr {typ = t2; fields = f2; ty = _} ) ->
      symeq t1 t2
      && listeq (fun (s1, e1) (s2, e2) -> symeq s1 s2 && expr_eq e1 e2) f1 f2
  | SeqExpr (e1, _), SeqExpr (e2, _) -> listeq expr_eq e1 e2
  | AssignExpr {var = v1; expr = e1}, AssignExpr {var = v2; expr = e2} ->
      var_eq v1 v2 && expr_eq e1 e2
  | ( IfExpr {test = t1; then' = th1; else' = e1; ty = _}
    , IfExpr {test = t2; then' = th2; else' = e2; ty = _} ) ->
      expr_eq t1 t2 && expr_eq th1 th2 && Option.equal expr_eq e1 e2
  | WhileExpr {test = t1; body = b1}, WhileExpr {test = t2; body = b2} ->
      expr_eq t1 t2 && expr_eq b1 b2
  | ( ForExpr {var = v1; lo = l1; hi = h1; body = b1; escape = _}
    , ForExpr {var = v2; lo = l2; hi = h2; body = b2; escape = _} ) ->
      symeq v1 v2 && expr_eq l1 l2 && expr_eq h1 h2 && expr_eq b1 b2
  | BreakExpr, BreakExpr -> true
  | ( LetExpr {decls = d1; body = b1; ty = _}
    , LetExpr {decls = d2; body = b2; ty = _} ) ->
      listeq decl_eq d1 d2 && expr_eq b1 b2
  | ( ArrayExpr {typ = t1; size = s1; init = i1; ty = _}
    , ArrayExpr {typ = t2; size = s2; init = i2; ty = _} ) ->
      symeq t1 t2 && expr_eq s1 s2 && expr_eq i1 i2
  | _ -> false

and decl_eq d1 d2 =
  match (d1, d2) with
  | FunctionDecl f1, FunctionDecl f2 -> listeq fn_eq f1 f2
  | ( VarDecl {name = n1; typ = t1; init = i1; escape = _; naked_rvalue = _}
    , VarDecl {name = n2; typ = t2; init = i2; escape = _; naked_rvalue = _} )
    ->
      symeq n1 n2 && Option.equal symeq t1 t2 && expr_eq i1 i2
  | TypeDecl t1, TypeDecl t2 -> listeq tyalias_eq t1 t2
  | _ -> false

and ty_eq t1 t2 =
  match (t1, t2) with
  | NameTy t1, NameTy t2 -> symeq t1 t2
  | RecordTy f1, RecordTy f2 -> listeq field_eq f1 f2
  | ArrayTy a1, ArrayTy a2 -> symeq a1 a2
  | _ -> false

and field_eq {fld_name = f1; typ = t1; escape = _}
    {fld_name = f2; typ = t2; escape = _} =
  symeq f1 f2 && symeq t1 t2

and tyalias_eq {name = n1; ty = t1} {name = n2; ty = t2} =
  symeq n1 n2 && ty_eq t1 t2

and fn_eq {fn_name = f1; params = p1; result = r1; body = b1}
    {fn_name = f2; params = p2; result = r2; body = b2} =
  symeq f1 f2 && listeq field_eq p1 p2 && Option.equal symeq r1 r2
  && expr_eq b1 b2

module F = Strictly_annotated

let isseq = function SeqExpr _ -> true | _ -> false
let cons = F.(List.fold_left ( ^^ ) empty)

let rec pr_var =
  let open F in
  function
  | SimpleVar (s, _) -> text (name s)
  | FieldVar (v, s, _) -> pr_var v ^^ text "." ^^ text (name s)
  | SubscriptVar (v, s, _) -> pr_var v ^^ text "[" ^^ pr_expr s ^^ text "]"

and pr_expr ?(wrap_seq = true) e =
  let open F in
  let edoc =
    match e with
    | NilExpr -> text "nil"
    | VarExpr (v, _) -> group (nest 2 (pr_var v))
    | IntExpr n -> text (string_of_int n)
    | StringExpr s -> text (Printf.sprintf "\"%s\"" (String.escaped s))
    | CallExpr {func; args; _} ->
        let lastargi = List.length args - 1 in
        let args =
          List.mapi
            (fun i arg ->
              pr_expr arg
              ^^ if i <> lastargi then text "," ^^ space else breakhint )
            args
          |> cons
        in
        group (nest 2 (text (name func) ^^ text "(" ^. args ^^ text ")"))
    | OpExpr {left; oper; right; _} ->
        (* TODO: check if operator chain passes printing width; if so, print as
           a vertical group. See the many_params test case for an example of
           when this is useful. *)
        group
          (nest 2 (pr_expr left ^^ text " " ^^ pr_oper oper ^| pr_expr right))
    | RecordExpr {typ; fields; _} ->
        let lastfieldi = List.length fields - 1 in
        let fields =
          List.mapi
            (fun i (fld, v) ->
              let suffix =
                if i <> lastfieldi then text "," ^^ space else empty
              in
              group
                (nest 2 (text (name fld) ^^ text "=" ^. pr_expr v ^^ suffix)) )
            fields
          |> cons
        in
        let typ = text (name typ) in
        let rcd = typ ^| text "{" ^| group (nest 2 fields) ^| text "}" in
        group (nest 2 rcd)
    | SeqExpr (exprs, _) ->
        let lastexpri = List.length exprs - 1 in
        let exprs =
          List.mapi
            (fun i e ->
              pr_expr e
              ^^ if i <> lastexpri then text ";" ^^ breakhint else empty )
            exprs
          |> cons
        in
        vgroup exprs
    | AssignExpr {var; expr} ->
        group (nest 2 (pr_var var ^^ text " :=" ^| pr_expr expr))
    | IfExpr {test; then'; else' = None; _} ->
        group
          ( group (nest 2 (text "if" ^| pr_expr test))
          ^| group (nest 2 (text "then" ^| pr_expr then')) )
    | IfExpr {test; then'; else' = Some else'; _} ->
        group
          ( group (nest 2 (text "if" ^| pr_expr test))
          ^| group (nest 2 (text "then" ^| pr_expr then'))
          ^| group (nest 2 (text "else" ^| pr_expr else')) )
    | WhileExpr {test; body} ->
        group
          ( group (nest 2 (text "while" ^| pr_expr test))
          ^| group (nest 2 (text "do" ^| pr_expr body)) )
    | ForExpr {var; lo; hi; body; _} ->
        group
          ( group
              (nest 2
                 (text "for" ^| text (name var) ^^ text " := " ^^ pr_expr lo) )
          ^| group (nest 2 (text "to" ^| pr_expr hi))
          ^| group (nest 2 (text "do" ^| pr_expr body)) )
    | BreakExpr -> text "break"
    | LetExpr {decls; body; _} ->
        let lastdecli = List.length decls - 1 in
        let decls =
          List.mapi
            (fun i d ->
              pr_decl d ^^ if i <> lastdecli then space ^^ space else empty )
            decls
          |> cons
        in
        let body = pr_expr ~wrap_seq:false body in
        vgroup
          ( vgroup (nest 2 (text "let" ^| decls))
          ^| group (nest 2 (text "in" ^| body))
          ^| group (text "end") )
    | ArrayExpr {typ; size; init; _} ->
        group
          ( group (nest 2 (text (name typ) ^^ text "[" ^. pr_expr size))
          ^. group (text "] of " ^^ pr_expr init) )
  in
  if wrap_seq && isseq e then text "(" ^^ edoc ^^ text ")" else edoc

and pr_decl =
  let open F in
  function
  | FunctionDecl decls ->
      let lastdecli = List.length decls - 1 in
      List.mapi
        (fun i fn -> pr_fn fn ^^ if i <> lastdecli then text "\n" else empty)
        decls
      |> cons
  | VarDecl {name; typ = None; init; _} ->
      group
        (nest 2
           (text "var " ^^ text (Symbol.name name) ^^ text " :=" ^| pr_expr init) )
  | VarDecl {name; typ = Some typ; init; _} ->
      group
        (nest 2
           ( text "var "
           ^^ text (Symbol.name name)
           ^| group (text ": " ^^ text (Symbol.name typ))
           ^| group (text ":= " ^^ pr_expr init) ) )
  | TypeDecl sigs ->
      let lastsigi = List.length sigs - 1 in
      List.mapi
        (fun i s -> pr_tysig s ^^ if i <> lastsigi then text "\n" else empty)
        sigs
      |> cons

and pr_ty =
  let open F in
  function
  | NameTy sym -> text (name sym)
  | RecordTy fields ->
      let lastfieldi = List.length fields - 1 in
      let fields =
        List.mapi
          (fun i {fld_name; typ; _} ->
            let suffix = if i <> lastfieldi then text "," ^^ space else empty in
            group
              (nest 2
                 (text (name fld_name) ^^ text ":" ^| text (name typ) ^^ suffix) )
            )
          fields
        |> cons
      in
      group (group (nest 2 (text "{" ^| fields)) ^| group (text "}"))
  | ArrayTy sym -> text "array of " ^^ text (name sym)

and pr_oper op =
  F.text
    ( match op with
    | PlusOp -> "+"
    | MinusOp -> "-"
    | TimesOp -> "*"
    | DivideOp -> "/"
    | EqOp -> "="
    | NeqOp -> "<>"
    | LtOp -> "<"
    | LeOp -> "<="
    | GtOp -> ">"
    | GeOp -> ">=" )

and pr_tysig {name; ty} =
  let open F in
  group
    (nest 2 (text "type " ^^ text (Symbol.name name) ^^ text " =" ^| pr_ty ty))

and pr_fn =
  let open F in
  function
  | {fn_name; params; result; body} ->
      let lastparami = List.length params - 1 in
      let params =
        List.mapi
          (fun i {fld_name; typ; _} ->
            let suffix = if i <> lastparami then text "," ^^ space else empty in
            group (nest 2 (text (name fld_name) ^^ text ":" ^| text (name typ)))
            ^^ suffix )
          params
        |> cons
      in
      let ret =
        match result with
        | None -> empty
        | Some ty -> text " : " ^^ text (name ty)
      in
      group
        ( group
            ( group
                (nest 2
                   ( text "function "
                   ^^ text (name fn_name)
                   ^^ text "(" ^. params ) )
            ^. group (text ")" ^^ ret ^^ text " =") )
        ^| group (pr_expr body) )

let pretty = F.pretty 80 "/* "
let string_of_var v = pr_var v |> pretty
let string_of_expr e = pr_expr e |> pretty
let string_of_decl d = pr_decl d |> pretty
let string_of_ty t = pr_ty t |> pretty
let fmt_expr fmt e = Format.pp_print_string fmt (string_of_expr e)
