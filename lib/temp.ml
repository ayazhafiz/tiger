type temp = int

let newtemp =
  let cnt = ref 0 in
  fun () ->
    cnt := !cnt + 1;
    !cnt

type label = string

let string_of_label l = l

(** TODO: figure out how to deal with externals better *)
let stringEqual = "stringEqual"

let initArray = "initArray"
let externals = [stringEqual; initArray]

let label_store =
  List.map (fun n -> (n, ())) externals |> List.to_seq |> Hashtbl.of_seq

let newlabel =
  let rec find name ext =
    let cand = if ext = 0 then name else name ^ string_of_int ext in
    match Hashtbl.find_opt label_store cand with
    | None ->
        Hashtbl.add label_store cand ();
        cand
    | Some _ -> find name (ext + 1)
  in
  fun name -> find name 0

let strlabel name =
  Hashtbl.add label_store name ();
  name

module TempSet = Set.Make (struct
  type t = temp

  let compare = compare
end)
