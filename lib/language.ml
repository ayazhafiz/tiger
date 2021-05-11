open Symbol

type var =
  | SimpleVar of symbol
  | FieldVar of var * symbol
  | SubscriptVar of var * expr

and expr =
  | NilExpr
  | VarExpr of var
  | IntExpr of int
  | StringExpr of string
  | CallExpr of { func : symbol; args : expr list }
  | OpExpr of { left : expr; oper : oper; right : expr }
  | RecordExpr of { typ : symbol; fields : (symbol * expr) list }
  | SeqExpr of expr list
  | AssignExpr of { var : var; expr : expr }
  | IfExpr of { test : expr; then' : expr; else' : expr option }
  | WhileExpr of { test : expr; body : expr }
  | ForExpr of {
      var : symbol;
      escape : bool ref;
      lo : expr;
      hi : expr;
      body : expr;
    }
  | BreakExpr
  | LetExpr of { decls : decl list; body : expr }
  | ArrayExpr of { typ : symbol; size : expr; init : expr }

and decl =
  | FunctionDecl of fundecl list
  | VarDecl of {
      name : symbol;
      escape : bool ref;
      typ : symbol option;
      init : expr;
    }
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

and ty_alias = { name : symbol; ty : ty }

and field_ty = { fld_name : symbol; escape : bool ref; typ : symbol }

and fundecl = {
  fn_name : symbol;
  params : field_ty list;
  result : symbol option;
  body : expr;
}
