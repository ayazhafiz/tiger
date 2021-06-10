open Tiger

let usage = "tigerfmt [-i] <file>"
let infile = ref None
let inplace = ref false

(* *)
let setinfile fi = infile := Some fi
let speclist = [("-i", Arg.Set inplace, "Format in place")]

let () =
  Arg.parse speclist setinfile usage;
  let fi =
    match !infile with
    | Some fi -> fi
    | None ->
        Printf.eprintf "Error: no file specified\n";
        Arg.usage speclist usage;
        exit 1
  in
  let expr = Driver.(readfi fi |> lex |> parse) in
  let pp_expr = Driver.pretty_print expr in
  match !inplace with
  | true -> Driver.writefi fi pp_expr
  | false -> Printf.printf "%s" pp_expr
