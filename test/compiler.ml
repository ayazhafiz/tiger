(* TODO: better encapsulation of "bless"ing with dune *)

open Tiger.Registry
open Tiger.Front
open Tiger.Front.Desugar
open Tiger.Front.Language
open Tiger.Front.Semantic
module Driver = Tiger.Driver

let readfi = Driver.readfi
let writefi = Driver.writefi

(** [dir_contents] returns the paths of all regular files that are contained
    in [dir]. Each file is a path starting with [dir].
    Source: https://gist.github.com/lindig/be55f453026c65e761f4e7012f8ab9b5 *)
let dir_contents dir =
  let rec loop result = function
    | f :: fs when Sys.is_directory f ->
        Sys.readdir f |> Array.to_list
        |> List.map (Filename.concat f)
        |> List.append fs |> loop result
    | f :: fs -> loop (f :: result) fs
    | [] -> result
  in
  loop [] [dir]

let rec mkdir_p = function
  | "." -> ()
  | p -> (
      mkdir_p (Filename.dirname p);
      try Sys.mkdir p 0o777 with _ -> () )

let dirify = Str.global_replace (Str.regexp_string "/") Filename.dir_sep
let cases_dir = dirify "test/cases/"
let baseline_local = dirify "test/baseline/local"
let baseline_golden = dirify "test/baseline/golden"
let do_bless = ref false
let fi_local = Filename.concat baseline_local
let fi_golden = Filename.concat baseline_golden
let write_local name content = writefi (fi_local name) content
let write_golden name content = writefi (fi_golden name) content

type fi =
  { name : string
  ; path : string
  ; content : string
  ; syntax_error : bool
  ; semantic_error : string option
  ; mutable lexed : Lexing.lexbuf option
  ; mutable parse : expr option
  ; mutable desugar : desugared_expr option }

let mkfi path =
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
  { name =
      List.hd (Str.split (Str.regexp_string cases_dir) path)
      |> Filename.chop_extension
  ; path
  ; content
  ; syntax_error
  ; semantic_error
  ; lexed = Option.none
  ; parse = Option.none
  ; desugar = Option.none }

let ensure_unique cases =
  let tbl = Hashtbl.create (List.length cases) in
  List.iter
    (fun {name; path; _} ->
      match Hashtbl.find_opt tbl name with
      | Some path1 ->
          Alcotest.fail
            (Printf.sprintf "Duplicate tests %s (%s and %s)" name path1 path)
      | None -> Hashtbl.add tbl name path )
    cases

let ensure_dirs cases =
  let dirs = Hashtbl.create 16 in
  Hashtbl.add dirs baseline_local ();
  Hashtbl.add dirs baseline_golden ();
  List.iter
    (fun {name; _} ->
      let local = Filename.dirname (fi_local name) in
      let golden = Filename.dirname (fi_golden name) in
      if not (Hashtbl.mem dirs local) then (
        Hashtbl.add dirs local ();
        Hashtbl.add dirs golden () ) )
    cases;
  Hashtbl.iter (fun dir () -> mkdir_p dir) dirs

let all_cases =
  let cases =
    dir_contents cases_dir
    |> List.filter (fun file -> Filename.extension file = ".tig")
    |> List.map mkfi
  in
  ensure_unique cases;
  ensure_dirs cases;
  cases

let get opt err = match opt with Some v -> v | None -> failwith err
let lex = Lexing.from_string ~with_positions:true
let parse = Parser.toplevel Lexer.read
let testable_expr = Alcotest.testable Language.fmt_expr ( = )

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

let cmp_golden name =
  if !do_bless then ()
  else
    let local = readfi (fi_local name) in
    let golden =
      try readfi (fi_golden name)
      with _ ->
        Alcotest.fail
          (Printf.sprintf
             {|Golden "%s" not present; run "--bless" to accept baselines first.|}
             (fi_golden name) )
    in
    if local <> golden then
      Alcotest.fail
        (Printf.sprintf "Local baseline %s differs from golden." name)

(* Option: Accept goldens *)
let bless =
  let open Cmdliner in
  Arg.(
    value & flag & info ["bless"] ~doc:"Accept local baselines as new goldens.")

(* Option: filter *)
let test_filter =
  let open Cmdliner in
  Arg.(value & opt string "" & info ["f"] ~doc:"Test filter")

let test_options =
  let open Cmdliner in
  let cmdline = Term.(const (fun a b -> (a, b)) $ test_filter $ bless) in
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

let printtest fi =
  let test _ =
    ensure_parsed fi;
    let expr_real =
      get fi.parse (Printf.sprintf "Parsing %s incomplete" fi.name)
    in
    let parsefi = fi.name ^ ".parse" in
    let pr_real = string_of_expr expr_real in
    write_local parsefi pr_real;
    cmp_golden parsefi
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

let backend_golden ?(compare_golden = true) fi ext driver =
  ensure_parsed fi;
  let name = fi.name ^ ext in
  let expr = get fi.parse (Printf.sprintf "Parsing %s incomplete" fi.name) in
  write_local name (driver expr);
  if compare_golden then cmp_golden name

(** Not a real test: writes an ir file in the local baselines for debug
    purposes. *)
let irtest fi =
  let test _ =
    (* x86 *)
    backend_golden ~compare_golden:false fi ".ir" X86_64_Backend.emit_ir
  in
  ((fi.name, `Quick, wrap test), true)

(** Not a real test: writes a pseudo_s file in the local baselines for debug
    purposes. *)
let pseudo_asmtest fi =
  let test _ =
    (* x86 *)
    backend_golden ~compare_golden:false fi ".pseudo_s"
      X86_64_Backend.Debug.emit_pseudo_assem
  in
  ((fi.name, `Quick, wrap test), true)

let asmtest fi =
  let test _ =
    (* x86 *)
    backend_golden fi ".nasm" X86_64_Backend.emit_assem
  in
  ((fi.name, `Quick, wrap test), true)

let lltest fi =
  let test _ = backend_golden fi ".ll" Llvm.lower in
  ((fi.name, `Quick, wrap test), true)

let exectest fi =
  let exec handler expr =
    let expect = fi_golden (fi.name ^ ".exec") in
    let stdin =
      let contents = try readfi expect with _ -> "" in
      let re_stdin = Str.regexp "===stdin\n" in
      if Str.string_match re_stdin contents 0 then
        let stdin = Str.split (Str.regexp "===stdin\n") contents |> List.hd in
        String.sub stdin 0 (String.length stdin - 1) |> Option.some
      else None
    in
    let stdout, stderr, exit = handler expr stdin in
    let ekind, ecode =
      match exit with
      | Driver.Exit n -> ("exit", n)
      | Driver.Killed n -> Alcotest.fail (Printf.sprintf "Killed (%d)" n)
    in
    let parts =
      match stdin with None -> [] | Some s -> ["===stdin"; s; "===stdin"; ""]
    in
    let parts =
      parts
      @ [ "~~~stdout"; stdout; "~~~stdout"; ""; "~~~stderr"; stderr; "~~~stderr"
        ; ""; "---" ^ ekind; string_of_int ecode; "---" ^ ekind ]
    in
    String.concat "\n" parts
  in
  let test _ =
    let machine = Driver.current_machine () in
    (* x86 *)
    backend_golden fi ".exec" (exec (X86_64_Backend.exec machine))
  in
  ((fi.name, `Slow, wrap test), true)

let blessing =
  let cmd = Printf.sprintf "cp -R %s/* %s" baseline_local baseline_golden in
  ("blessing", `Quick, fun _ -> ignore (Driver.sh cmd))

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
  let test_filter, bless = test_options in
  do_bless := bless;
  let cases =
    List.filter
      (fun fi ->
        try Str.search_forward (Str.regexp_string test_filter) fi.name 0 >= 0
        with Not_found -> false )
      all_cases
  in
  let lexer_tests, cases = mktests lextest cases in
  let parser_tests, cases = mktests parsetest cases in
  let printer_tests, cases = mktests printtest cases in
  let semantic_tests, cases = mktests sematest cases in
  let ir_tests, cases = mktests irtest cases in
  let pseudo_asm_tests, cases = mktests pseudo_asmtest cases in
  let asm_tests, cases = mktests asmtest cases in
  let ll_tests, cases = mktests lltest cases in
  let exec_tests, _cases = mktests exectest cases in
  let fakeargv = Array.make 1 "compiler_tests" in
  let compiler_tests =
    [ ("lexer tests", lexer_tests); ("parser tests", parser_tests)
    ; ("printer tests", printer_tests); ("semantic tests", semantic_tests)
    ; ("lowering tests", ir_tests); ("pseudo-emit tests", pseudo_asm_tests)
    ; ("asm emit tests", asm_tests); ("llvm emit tests", ll_tests)
    ; ("execution tests", exec_tests) ]
    @ if !do_bless then [("blessing", [blessing])] else []
  in
  Alcotest.run "compiler_tests" ~argv:fakeargv compiler_tests
