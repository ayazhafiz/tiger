open Tiger

let usage = "tigerc [-k asm|exe] [-t `machine] [-o file] <file>"

type kind = Asm | Exe

let infile = ref None
let kind = ref Asm
let target = ref None
let outfile = ref None

(* *)
let setinfile fi = infile := Some fi

let setkind k =
  kind :=
    match String.trim k with
    | "asm" -> Asm
    | "exe" -> Exe
    | k -> raise (Arg.Bad ("Invalid emit kind " ^ k))

let settarget t =
  target :=
    try Some (Driver.machine_of_triple t)
    with _ -> raise (Arg.Bad ("Invalid triple " ^ t))

let setoutfile fi = outfile := Some fi

let speclist =
  [ ("-k", Arg.String setkind, "Emit kind (asm|exe); default asm")
  ; ( "-t"
    , Arg.String settarget
    , "Target triple. One of:\n"
      ^ String.concat "\n  "
          (List.map Driver.triple_of_machine Driver.all_machines) )
  ; ("-o", Arg.String setoutfile, "Output file") ]

let emit kind target expr =
  match (kind, target) with
  | ( Asm
    , ( Driver.X86_64_apple_darwin20_1_0 | Driver.X86_64_apple_darwin20_6_0
      | Driver.X86_64_linux_gnu ) ) ->
      Backend_registry.X86_64_Backend.emit_assem expr
  | ( Exe
    , ( Driver.X86_64_apple_darwin20_1_0 | Driver.X86_64_apple_darwin20_6_0
      | Driver.X86_64_linux_gnu ) ) ->
      Backend_registry.X86_64_Backend.emit_exe target expr

let () =
  Arg.parse speclist setinfile usage;
  if Option.is_none !target then target := Some (Driver.current_machine ());
  let target = Option.get !target in
  let input =
    match !infile with
    | Some fi -> Driver.readfi fi
    | None -> Driver.read_in stdin
  in
  let lex = Lexing.from_string ~with_positions:true input in
  let expr = Front.Parser.toplevel Front.Lexer.read lex in
  ( match Front.Semantic.check_prog expr with
  | Ok () -> ()
  | Error e -> failwith e );
  let result = emit !kind target expr in
  match !outfile with
  | Some outfile -> Driver.writefi outfile result
  | None -> Printf.printf "%s" result
