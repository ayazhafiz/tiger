open Symbol

type temp = int
type label = symbol

let label = symbol
let temp_cnt = ref 0
let label_store = Hashtbl.create 128
let string_of_temp t = "t" ^ string_of_int t
let tempeq t u = t = u

let newtemp () =
  temp_cnt := !temp_cnt + 1;
  !temp_cnt

let string_of_label = name

(** TODO: figure out how to deal with externals better *)
let stringEqual = label "stringEqual"

let initArray = label "initArray"
let externals = [stringEqual; initArray]

let init_label_store () =
  List.map (fun n -> (name n, ())) externals
  |> List.to_seq
  |> Hashtbl.add_seq label_store

let _ = init_label_store ()

let newlabel =
  let rec find name ext =
    let cand = if ext = 0 then name else name ^ string_of_int ext in
    match Hashtbl.find_opt label_store cand with
    | None ->
        Hashtbl.add label_store cand ();
        label cand
    | Some _ -> find name (ext + 1)
  in
  fun name -> find name 0

let strlabel name =
  Hashtbl.add label_store name ();
  label name

module TempSet = Set.Make (struct
  type t = temp

  let compare = compare
end)

module LabelSet = Set.Make (struct
  type t = label

  let compare = compare
end)

module LabelHashtbl = SymbolHashtbl

let reset reserved_temps reserved_labels =
  temp_cnt := List.fold_left max 0 reserved_temps;
  Hashtbl.clear label_store;
  init_label_store ();
  List.iter (fun l -> Hashtbl.add label_store (name l) ()) reserved_labels
