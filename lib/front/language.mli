open Symbol

(** A resolved type after typecheck. *)
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

val expr_eq : expr -> expr -> bool
val string_of_var : var -> string
val string_of_expr : expr -> string
val string_of_decl : decl -> string
val string_of_ty : ty -> string
val fmt_expr : Format.formatter -> expr -> unit
