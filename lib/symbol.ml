type symbol = string * int

let nextsym = ref 0

let interner : (string, int) Hashtbl.t = Hashtbl.create 128

let symbol name =
  match Hashtbl.find_opt interner name with
  | Some i -> (name, i)
  | None ->
      let i = !nextsym in
      nextsym := i + 1;
      Hashtbl.add interner name i;
      (name, i)

let name (s, _) = s

module Table = struct
  module SymbolHashtbl = Hashtbl.Make (struct
    type t = symbol

    let equal (_, i) (_, j) = i = j

    let hash (_, i) = i
  end)

  type 'a t = {
    mutable tbls : 'a SymbolHashtbl.t list;
        (** Scoped list of symbol tables. First table
            corresponds to the current most inner scope. *)
  }

  let newtbl () = SymbolHashtbl.create 32

  let singleton () = { tbls = [ newtbl () ] }

  let enter t = t.tbls <- newtbl () :: t.tbls

  let exit t =
    t.tbls <-
      ( match t.tbls with
      | [] -> failwith "Inconsistent state: symbol table has no scopes"
      | _ :: rest -> rest )

  let add { tbls } sym v =
    match tbls with
    | [] -> failwith "Inconsistent state: symbol table has no scopes"
    | t :: _ -> SymbolHashtbl.add t sym v

  let find { tbls } sym =
    let rec walk = function
      | [] -> None
      | t :: rest -> (
          match SymbolHashtbl.find_opt t sym with
          | None -> walk rest
          | Some v -> Some v )
    in
    walk tbls

  let copy { tbls } = { tbls = List.map SymbolHashtbl.copy tbls }

  let string_of { tbls } fmtv =
    let fmtentry (k, v) = Printf.sprintf "%s=>%s" (name k) (fmtv v) in
    let fmttbl level tbl =
      Printf.sprintf "(%d) %s" level
        ( SymbolHashtbl.to_seq tbl |> Seq.map fmtentry |> List.of_seq
        |> String.concat ";" )
    in
    List.rev tbls |> List.mapi fmttbl |> String.concat "\n"
end
