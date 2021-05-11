open Tiger
open Tiger.Language
open Tiger.Print

let ext = ".tig"

let parse_ext = ".parse"

type fi = {
  name : string;
  path : string;
  content : string;
  syntax_error : bool;
  mutable lexed : Lexing.lexbuf option;
  mutable parse : expr option;
}

let readfi path =
  let ch = open_in path in
  let content = really_input_string ch (in_channel_length ch) in
  close_in ch;
  content

let writefi path content =
  let ch = open_out path in
  output_string ch content;
  close_out ch

let mkfi dir name =
  let path = Filename.concat dir name in
  let content = readfi path in
  let re_syntax_err = Str.regexp {|/\* error: syntax error|} in
  let syntax_error = Str.string_match re_syntax_err content 0 in
  {
    name = Filename.remove_extension name;
    path;
    content;
    syntax_error;
    lexed = Option.none;
    parse = Option.none;
  }

let get opt err = match opt with Some v -> v | None -> failwith err

let ensure_lexed fi =
  if Option.is_none fi.lexed then
    fi.lexed <- Option.some (Lexing.from_string fi.content ~with_positions:true)

let ensure_parsed fi =
  ensure_lexed fi;
  let lexed = get fi.lexed "Lexing incomplete" in
  if Option.is_none fi.parse then
    fi.parse <- Option.some (Parser.toplevel Lexer.read lexed)

let get_test_files =
  let open Cmdliner in
  let res =
    Arg.(value & opt string "testcases" & info [ "p" ] ~doc:"Path to tests")
  in
  Term.app
    (Term.const (fun dir ->
         Sys.readdir dir |> Array.to_list
         |> List.filter (fun file -> Filename.extension file = ext)
         |> List.map (mkfi dir)))
    res

let update_goldens =
  let open Cmdliner in
  Arg.(value & flag & info [ "u" ] ~doc:"Update goldens")

let cmd =
  let open Cmdliner in
  let join test_files update_goldens = (test_files, update_goldens) in
  let cmdline = Term.(const join $ get_test_files $ update_goldens) in
  let result = Term.eval (cmdline, Term.info "Test Options") in
  match result with `Ok r -> r | _ -> exit (Term.exit_status_of_result result)

let lextest fi =
  let test _ = ensure_lexed fi in
  (fi.name, `Quick, test)

let parsetest fi =
  let test _ =
    match fi.syntax_error with
    | true -> (
        try
          ensure_parsed fi;
          Alcotest.fail "Expected syntax error"
        with _ -> () )
    | false -> ensure_parsed fi
  in
  (fi.name, `Quick, test)

let printtest update fi =
  if fi.syntax_error then Option.none
  else
    Option.some
      ( ensure_parsed fi;
        let pr_path = Filename.remove_extension fi.path ^ parse_ext in
        let expr =
          get fi.parse (Printf.sprintf "Parsing %s incomplete" fi.name)
        in
        let pr_real = string_of_expr expr in
        let test _ =
          match update with
          | true -> writefi pr_path pr_real
          | false ->
              let pr_expect = readfi pr_path in
              Alcotest.(check string) fi.name pr_expect pr_real
        in
        (fi.name, `Quick, test) )

let () =
  let tests, update_goldens = cmd in
  let fakeargv = Array.make 1 "compiler_tests" in
  Alcotest.run "compiler_tests" ~argv:fakeargv
    [
      ("lexer tests", List.map lextest tests);
      ("parser tests", List.map parsetest tests);
      ("printer tests", List.filter_map (printtest update_goldens) tests);
    ]
