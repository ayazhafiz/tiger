type symbol = string * int

let nextsym = ref 0

let hashtable : (string, int) Hashtbl.t = Hashtbl.create 128

let symbol name =
  match Hashtbl.find_opt hashtable name with
  | Some i -> (name, i)
  | None ->
      let i = !nextsym in
      nextsym := i + 1;
      Hashtbl.add hashtable name i;
      (name, i)

let name (s, _) = s
