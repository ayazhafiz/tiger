open Tiger
open Tiger.Backend_registry
open Tiger.Desugar
open Tiger.Language
open Tiger.Print
open Tiger.Semantic

let ext = ".tig"

type fi =
  { name : string
  ; path : string
  ; content : string
  ; syntax_error : bool
  ; semantic_error : string option
  ; mutable lexed : Lexing.lexbuf option
  ; mutable parse : expr option
  ; mutable desugar : desugared_expr option }

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
  let re_semantic_err = Str.regexp {|/\* error: \(.*\) \*/|} in
  let syntax_error = Str.string_match re_syntax_err content 0 in
  let semantic_error = Str.string_match re_semantic_err content 0 in
  let semantic_error =
    if semantic_error && not syntax_error then
      Some (Str.matched_group 1 content |> String.trim)
    else None
  in
  { name = Filename.remove_extension name
  ; path
  ; content
  ; syntax_error
  ; semantic_error
  ; lexed = Option.none
  ; parse = Option.none
  ; desugar = Option.none }

let get opt err = match opt with Some v -> v | None -> failwith err
let lex = Lexing.from_string ~with_positions:true
let parse = Parser.toplevel Lexer.read
let testable_expr = Alcotest.testable Print.fmt_expr ( = )

let ensure_lexed fi =
  if Option.is_none fi.lexed then fi.lexed <- Option.some (lex fi.content)

let ensure_parsed fi =
  ensure_lexed fi;
  let lexed = get fi.lexed "Lexing incomplete" in
  if Option.is_none fi.parse then fi.parse <- Option.some (parse lexed)

let ensure_desugar fi =
  ensure_parsed fi;
  let parsed = get fi.parse "Parsing incomplete" in
  if Option.is_none fi.desugar then
    fi.desugar <- Option.some (desugar_expr parsed)

let get_test_files =
  let open Cmdliner in
  let res =
    Arg.(value & opt string "test/testcases" & info ["p"] ~doc:"Path to tests")
  in
  Term.app
    (Term.const (fun dir ->
         Sys.readdir dir |> Array.to_list
         |> List.filter (fun file -> Filename.extension file = ext)
         |> List.map (mkfi dir) ) )
    res

let update_goldens =
  let open Cmdliner in
  Arg.(value & flag & info ["u"] ~doc:"Update goldens")

let cmd =
  let open Cmdliner in
  let join test_files update_goldens = (test_files, update_goldens) in
  let cmdline = Term.(const join $ get_test_files $ update_goldens) in
  let result = Term.eval (cmdline, Term.info "Test Options") in
  match result with `Ok r -> r | _ -> exit (Term.exit_status_of_result result)

let wrap test =
  Printexc.record_backtrace true;
  Sys.catch_break true;
  try test
  with _ as e ->
    Printexc.record_backtrace false;
    Alcotest.fail (Printexc.to_string e ^ "\n" ^ Printexc.get_backtrace ())

let lextest fi =
  let test _ = ensure_lexed fi in
  ((fi.name, `Quick, wrap test), true)

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
  ((fi.name, `Quick, wrap test), not fi.syntax_error)

let printtest update fi =
  let test _ =
    ensure_parsed fi;
    let pr_path = Filename.remove_extension fi.path ^ ".parse" in
    let expr_real =
      get fi.parse (Printf.sprintf "Parsing %s incomplete" fi.name)
    in
    let pr_real = string_of_expr expr_real in
    let pr_expect = ref pr_real in
    match update with
    | true -> writefi pr_path pr_real
    | false ->
        pr_expect := readfi pr_path;
        Alcotest.(check string) fi.name !pr_expect pr_real
    (* TODO *)
    (* let expr_expect = parse @@ lex !pr_expect in
       Alcotest.(check testable_expr) fi.name expr_expect expr_real *)
  in
  ((fi.name, `Quick, wrap test), true)

let sematest fi =
  let test _ =
    ensure_parsed fi;
    let expr = get fi.parse (Printf.sprintf "Parsing %s incomplete" fi.name) in
    match fi.semantic_error with
    | Some expect_err -> (
      match check_prog expr with
      | Ok _ -> Alcotest.fail ("Expected semantic error: " ^ expect_err)
      | Error msg ->
          if not (Str.string_match (Str.regexp msg) expect_err 0) then
            Alcotest.(check string) fi.name expect_err msg )
    | None -> (
      match check_prog expr with
      | Ok _ -> ()
      | Error msg -> Alcotest.fail ("Unexpected semantic error: " ^ msg) )
  in
  ((fi.name, `Quick, test), Option.is_none fi.semantic_error)

let backend_golden update fi ext driver =
  ensure_parsed fi;
  let golden_path = Filename.remove_extension fi.path ^ ext in
  let expr = get fi.parse (Printf.sprintf "Parsing %s incomplete" fi.name) in
  let real = driver expr in
  match update with
  | true -> writefi golden_path real
  | false ->
      let expect = readfi golden_path in
      Alcotest.(check string) fi.name expect real

let irtest update fi =
  let test _ =
    (* x86 *)
    backend_golden update fi ".ir" X86_64_Backend.emit_ir
  in
  ((fi.name, `Quick, wrap test), true)

let exclude_asm = ["queens"; "test4"; "test30"; "merge"; "test42"]

let pseudo_asmtest update fi =
  let test _ =
    (* x86 *)
    backend_golden update fi ".pseudo_s" X86_64_Backend.Debug.emit_pseudo_assem
  in
  ((fi.name, `Quick, wrap test), List.mem fi.name exclude_asm |> not)

let asmtest update fi =
  let test _ =
    (* x86 *)
    backend_golden update fi ".s" X86_64_Backend.emit_assem
  in
  ((fi.name, `Quick, wrap test), true)

let exectest update fi =
  let exec handler expr =
    let stdout, stderr, exit = handler expr in
    let ekind, ecode =
      match exit with
      | Backend.Exit n -> ("exit", n)
      | Backend.Killed n -> ("killed", n)
    in
    String.concat "\n"
      [ "~~~stdout"; stdout; "~~~stdout"; ""; "~~~stderr"; stderr; "~~~stderr"
      ; ""; "---" ^ ekind; string_of_int ecode; "---" ^ ekind ]
  in
  let test _ =
    (* x86 *)
    backend_golden update fi ".exec" (exec X86_64_Backend.exec)
  in
  ((fi.name, `Slow, wrap test), true)

let mktests factory cases =
  List.fold_right
    (fun case (tests, cases) ->
      let test, keep = factory case in
      let tests' = test :: tests in
      match keep with
      | true -> (tests', case :: cases)
      | false -> (tests', cases) )
    cases ([], [])

let () =
  let cases, update_goldens = cmd in
  let lexer_tests, cases = mktests lextest cases in
  let parser_tests, cases = mktests parsetest cases in
  let printer_tests, cases = mktests (printtest update_goldens) cases in
  let semantic_tests, cases = mktests sematest cases in
  let ir_tests, cases = mktests (irtest update_goldens) cases in
  let pseudo_asm_tests, cases = mktests (pseudo_asmtest update_goldens) cases in
  let asm_tests, cases = mktests (asmtest update_goldens) cases in
  let exec_tests, _cases = mktests (exectest update_goldens) cases in
  let fakeargv = Array.make 1 "compiler_tests" in
  Alcotest.run "compiler_tests" ~argv:fakeargv
    [ ("lexer tests", lexer_tests); ("parser tests", parser_tests)
    ; ("printer tests", printer_tests); ("semantic tests", semantic_tests)
    ; ("lowering tests", ir_tests); ("pseudo-emit tests", pseudo_asm_tests)
    ; ("emit tests", asm_tests); ("execution tests", exec_tests) ]
