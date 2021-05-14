type temp = int

let newtemp =
  let cnt = ref 0 in
  fun () ->
    cnt := !cnt + 1;
    !cnt

type label = string

(** TODO: figure out how to deal with externals better *)
let externals = [ "stringEqual"; "initArray" ]

let newlabel =
  let store =
    List.map (fun n -> (n, ())) externals |> List.to_seq |> Hashtbl.of_seq
  in
  let rec find name ext =
    let cand = if ext = 0 then name else name ^ string_of_int ext in
    match Hashtbl.find_opt store cand with
    | None ->
        Hashtbl.add store cand ();
        cand
    | Some _ -> find name (ext + 1)
  in
  fun name -> find name 0
