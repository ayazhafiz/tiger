open Symbol

type ventry =
  | VarEntry of Type.ty  (** ty *)
  | FunEntry of Type.ty list * Type.ty  (** params, return ty *)

let base_tenv =
  let open Symbol.Table in
  let open Type in
  let t = singleton () in
  add t (symbol "int") Int;
  add t (symbol "string") String;
  fun () -> copy t

let base_venv =
  let open Symbol.Table in
  let open Type in
  let t = singleton () in
  add t (symbol "print") (FunEntry ([String], Unit));
  add t (symbol "flush") (FunEntry ([], Unit));
  add t (symbol "get_char") (FunEntry ([], String));
  add t (symbol "ord") (FunEntry ([String], Int));
  add t (symbol "chr") (FunEntry ([Int], String));
  add t (symbol "size") (FunEntry ([String], Int));
  add t (symbol "substring") (FunEntry ([String; Int; Int], String));
  add t (symbol "concat") (FunEntry ([String; String], String));
  add t (symbol "not") (FunEntry ([Int], Int));
  add t (symbol "exit") (FunEntry ([Int], Unit));
  fun () -> copy t
