open Symbol
open Type

type ventry =
  | VarEntry of ty
  | FunEntry of ty list * ty  (** params + return ty *)

let string_of_ventry = function
  | VarEntry ty -> string_of_ty ty
  | FunEntry (p, r) ->
      Printf.sprintf "(%s):%s"
        (List.map string_of_ty p |> String.concat ", ")
        (string_of_ty r)

type tenv = ty Symbol.Table.t

type venv = ventry Symbol.Table.t

let base_tenv =
  let open Symbol.Table in
  let t = singleton () in
  add t (symbol "int") Int;
  add t (symbol "string") String;
  fun () -> copy t

let base_venv =
  let open Symbol.Table in
  let t = singleton () in
  add t (symbol "print") (FunEntry ([ String ], Unit));
  add t (symbol "flush") (FunEntry ([], Unit));
  add t (symbol "getchar") (FunEntry ([], String));
  add t (symbol "ord") (FunEntry ([ String ], Int));
  add t (symbol "chr") (FunEntry ([ Int ], String));
  add t (symbol "size") (FunEntry ([ String ], Int));
  add t (symbol "substring") (FunEntry ([ String; Int; Int ], String));
  add t (symbol "concat") (FunEntry ([ String; String ], String));
  add t (symbol "not") (FunEntry ([ Int ], Int));
  add t (symbol "exit") (FunEntry ([ Int ], Unit));
  fun () -> copy t
