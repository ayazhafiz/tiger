open Front
module L = Llvm

let ice why = failwith ("ICE (llvm lower): " ^ why)
let top_type ctx = L.pointer_type (L.i8_type ctx)

let llvm_type ctx =
  let open Type in
  let rec tyof = function
    | Int -> L.i64_type ctx
    | String -> L.pointer_type (L.i8_type ctx)
    (* Our arrays and records are passed around as pointer to the real
       array/record, hence the following translation. *)
    | Record (fields, _) ->
        L.pointer_type
          (L.struct_type ctx
             (List.map (fun (_, ty) -> tyof ty) fields |> Array.of_list) )
    | Array (ty, _) -> L.pointer_type (L.pointer_type (tyof ty))
    (* TODO: Can be smarter about the targeted type here? *)
    | Nil -> top_type ctx
    | Unit -> L.void_type ctx
    | Name (_, ty) as t -> (
      match !ty with
      | Some t -> tyof t
      | None -> ice (string_of_ty t ^ " not expanded") )
  in
  tyof

let lower _ = ""
