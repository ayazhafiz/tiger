open Symbol

type uniq = unit ref
(** Provides a unique symbol for nominal types *)

let uniq () = ref ()

type ty =
  | Int
  | String
  | Record of (symbol * ty) list * uniq
  | Array of ty * uniq
  | Nil
  | Unit
  | Name of symbol * ty option ref
      (** For use in mutually recursive types, in particular when we have seen
          a type name but not yet seen its definition. *)

let rec string_of_ty = function
  | Int -> "int"
  | String -> "string"
  | Record (fields, _) ->
      Printf.sprintf "{%s}"
        ( List.map
            (fun (sym, ty) ->
              Printf.sprintf "%s: %s" (name sym) (string_of_ty ty))
            fields
        |> String.concat ", " )
  | Array (ty, _) -> Printf.sprintf "[%s]" (string_of_ty ty)
  | Nil -> "nil"
  | Unit -> "()"
  | Name (sym, _) -> name sym
