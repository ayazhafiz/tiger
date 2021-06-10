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

module F = Format

let with_formatter cb =
  let buf = Buffer.create 128 in
  let fmt = F.formatter_of_buffer buf in
  F.pp_set_geometry fmt ~margin:80 ~max_indent:68;
  cb fmt;
  F.pp_print_flush fmt ();
  Buffer.to_seq buf |> String.of_seq

let isseq = function SeqExpr _ -> true | _ -> false

let rec pr_var f = function
  | SimpleVar (s, _) -> F.fprintf f "%s" (name s)
  | FieldVar (v, s, _) ->
      pr_var f v;
      F.fprintf f "@,.%s" (name s)
  | SubscriptVar (v, s, _) ->
      pr_var f v;
      F.fprintf f "[";
      pr_expr f s;
      F.fprintf f "]"

and pr_expr ?(wrap_seq = true) f e =
  let should_wrap = wrap_seq && isseq e in
  if should_wrap then F.fprintf f "(";
  ( match e with
  | NilExpr -> F.fprintf f "nil"
  | VarExpr (v, _) ->
      F.pp_open_hvbox f 2;
      pr_var f v;
      F.pp_close_box f ()
  | IntExpr n -> F.fprintf f "%d" n
  | StringExpr s -> F.fprintf f "\"%s\"" (String.escaped s)
  | CallExpr {func; args; _} ->
      F.fprintf f "@[<hv 2>%s(@," (name func);
      let lastargi = List.length args - 1 in
      List.iteri
        (fun i arg ->
          pr_expr f arg;
          if i <> lastargi then F.fprintf f ",@ " else F.fprintf f "@," )
        args;
      F.fprintf f ")@]"
  | OpExpr {left; oper; right; _} ->
      F.fprintf f "@[<hov 2>";
      pr_expr f left;
      F.fprintf f " ";
      pr_oper f oper;
      F.fprintf f "@ ";
      pr_expr f right;
      F.fprintf f "@]"
  | RecordExpr {typ; fields; _} ->
      F.fprintf f "@[<hv 2>%s@ {@[<hv 2>@ " (name typ);
      let lastfieldi = List.length fields - 1 in
      List.iteri
        (fun i (fld, v) ->
          F.fprintf f "@[<hov 2>%s=@," (name fld);
          pr_expr f v;
          F.fprintf f "@]";
          if i <> lastfieldi then F.fprintf f ",@ " )
        fields;
      F.fprintf f "@]@ }@]"
  | SeqExpr (exprs, _) ->
      F.fprintf f "@[<v>";
      let lastexpri = List.length exprs - 1 in
      List.iteri
        (fun i e ->
          pr_expr f e;
          if i <> lastexpri then F.fprintf f ";@," )
        exprs;
      F.fprintf f "@]"
  | AssignExpr {var; expr} ->
      F.fprintf f "@[<hov 2>";
      pr_var f var;
      F.fprintf f " :=@ ";
      pr_expr f expr;
      F.fprintf f "@]"
  | IfExpr {test; then'; else'; _} ->
      F.fprintf f "@[<hov>@[<hv 2>if@ ";
      pr_expr f test;
      F.fprintf f "@]@ @[<hv 2>then@ ";
      pr_expr f then';
      ( match else' with
      | Some else' ->
          F.fprintf f "@]@ @[<hv 2>else@ ";
          pr_expr f else'
      | None -> () );
      F.fprintf f "@]@]"
  | WhileExpr {test; body} ->
      F.fprintf f "while ";
      pr_expr f test;
      F.fprintf f " do@[<hv 2>@ ";
      pr_expr f body;
      F.fprintf f "@]"
  | ForExpr {var; lo; hi; body; _} ->
      F.fprintf f "for %s := " (name var);
      pr_expr f lo;
      F.fprintf f " to ";
      pr_expr f hi;
      F.fprintf f "do@[<hv 2>@ ";
      pr_expr f body;
      F.fprintf f "@]"
  | BreakExpr -> F.fprintf f "break"
  | LetExpr {decls; body; _} ->
      F.fprintf f "@[<v 2>let@ ";
      let lastdecli = List.length decls - 1 in
      List.iteri
        (fun i d ->
          pr_decl f d;
          if i <> lastdecli then F.fprintf f "@,@," )
        decls;
      F.fprintf f "@]@.@[<v 2>in@ ";
      pr_expr ~wrap_seq:false f body;
      F.fprintf f "@]@.end@."
  | ArrayExpr {typ; size; init; _} ->
      F.fprintf f "%s[" (name typ);
      pr_expr f size;
      F.fprintf f "] of ";
      pr_expr f init );
  if should_wrap then F.fprintf f ")"

and pr_decl f = function
  | FunctionDecl decls ->
      let lastdecli = List.length decls - 1 in
      List.iteri
        (fun i fn ->
          pr_fn f fn;
          if i <> lastdecli then F.fprintf f "@\n" )
        decls
  | VarDecl {name; typ = None; init; _} ->
      F.fprintf f "@[<hov 2>var %s :=@ " (Symbol.name name);
      pr_expr f init;
      F.fprintf f "@]"
  | VarDecl {name; typ = Some typ; init; _} ->
      F.fprintf f "@[<hov 2>var %s@ :@ %s@ :=@ " (Symbol.name name)
        (Symbol.name typ);
      pr_expr f init;
      F.fprintf f "@]"
  | TypeDecl sigs ->
      let lastsigi = List.length sigs - 1 in
      List.iteri
        (fun i s ->
          pr_tysig f s;
          if i <> lastsigi then F.fprintf f "@\n" )
        sigs

and pr_ty f = function
  | NameTy sym -> F.fprintf f "%s" (name sym)
  | RecordTy fields ->
      F.fprintf f "@[<hv>{@[<hv 2>@ ";
      let lastfieldi = List.length fields - 1 in
      List.iteri
        (fun i {fld_name; typ; _} ->
          F.fprintf f "@[<hov 2>%s:@ %s@]" (name fld_name) (name typ);
          if i <> lastfieldi then F.fprintf f ",@ " )
        fields;
      F.fprintf f "@]@ }@]"
  | ArrayTy sym -> F.fprintf f "array of %s" (name sym)

and pr_oper f op =
  F.fprintf f "%s"
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

and pr_tysig f {name; ty} =
  F.fprintf f "@[<hv 2>type %s =@ " (Symbol.name name);
  pr_ty f ty;
  F.fprintf f "@]"

and pr_fn f = function
  | {fn_name; params; result; body} ->
      F.fprintf f "@[<hv 2>@[<hv 2>function %s(@," (name fn_name);
      let lastparami = List.length params - 1 in
      List.iteri
        (fun i {fld_name; typ; _} ->
          F.fprintf f "%s: %s" (name fld_name) (name typ);
          if i <> lastparami then F.fprintf f ",@ " else F.fprintf f "@," )
        params;
      ( match result with
      | None -> F.fprintf f ") =@]@ "
      | Some ty -> F.fprintf f ") : %s =@]@ " (name ty) );
      pr_expr f body;
      F.fprintf f "@]"

let string_of_var v = with_formatter (fun f -> pr_var f v)
let string_of_expr e = with_formatter (fun f -> pr_expr f e)
let string_of_decl d = with_formatter (fun f -> pr_decl f d)
let string_of_ty t = with_formatter (fun f -> pr_ty f t)
let fmt_expr fmt e = Format.pp_print_string fmt (string_of_expr e)
