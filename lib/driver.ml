open Front.Desugar
open Back
open Back.Reg_alloc
open Back.Canon
open Back.Frame
open Back.Lower
module Escape = Front.Escape
module Print = Util.Print
module G = Data.Graph.Graph
module UDG = Data.Graph.UndirectedGraph
module TempSet = Back.Temp.TempSet

type version = {major : int; minor : int; patch : int option}

let parse_version version =
  let re_version = Str.regexp {|\([0-9]+\)\.\([0-9]+\)\(\.\([0-9]+\)\)?|} in
  assert (Str.string_match re_version version 0);
  let major = Str.matched_group 1 version |> int_of_string in
  let minor = Str.matched_group 2 version |> int_of_string in
  let patch =
    try Str.matched_group 4 version |> int_of_string |> Option.some
    with Not_found -> None
  in
  {major; minor; patch}

type machine = MacOS_11

let lkg_runtime = function MacOS_11 -> "runtime/lkg/runtime_macos_11.o"

let read_in ic =
  let lines = ref [] in
  try
    while true do
      lines := input_line ic :: !lines
    done;
    failwith "not terminated"
  with End_of_file -> List.rev !lines |> String.concat "\n"

let sh ?stdin:(stdin_str = None) cmd =
  let ((stdout, stdin, stderr) as exe) =
    Unix.open_process_full cmd (Array.of_list [])
  in
  Option.iter
    (fun s ->
      output_string stdin s;
      close_out stdin )
    stdin_str;
  let str_stdout = read_in stdout in
  let str_stderr = read_in stderr in
  let exit = Unix.close_process_full exe in
  (str_stdout, str_stderr, exit)

let assert_exit0 (_, _, exit) = assert (exit = Unix.WEXITED 0)

let execution_target () =
  let uname, _, _ = sh "uname" in
  let fail () =
    let unamea, _, _ = sh "uname -a" in
    failwith ("Unknown execution target " ^ unamea)
  in
  match uname with
  | "Darwin" -> (
      let version, _, _ =
        sh "defaults read loginwindow SystemVersionStampAsString"
      in
      let {major; _} = parse_version (String.trim version) in
      match major with m when m >= 11 -> MacOS_11 | _ -> fail () )
  | _ -> fail ()

type exit_status = Exit of int | Killed of int

let writefi path content =
  let ch = open_out path in
  output_string ch content;
  close_out ch

let exec_macos_11 prog stdin entry =
  let temp_asm = Filename.temp_file "main" ".asm" in
  let temp_o = Filename.temp_file "main" ".o" in
  let temp_exe = Filename.temp_file "main" ".exe" in
  let runtime_o = lkg_runtime MacOS_11 in
  let assemble = Printf.sprintf "nasm -f macho64 %s -o %s" temp_asm temp_o in
  let ld_lib_paths =
    ["-L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"]
  in
  let ld_libs = ["-lSystem"] in
  let ld_args = ld_lib_paths @ ld_libs |> String.concat " " in
  let link =
    Printf.sprintf
      "ld %s %s -o %s -e %s -arch x86_64 -macosx_version_min 11.0 %s" temp_o
      runtime_o temp_exe entry ld_args
  in
  writefi temp_asm prog;
  sh assemble |> assert_exit0;
  sh link |> assert_exit0;
  let stdout, stderr, exit = sh ~stdin temp_exe in
  let exit =
    match exit with
    | Unix.WEXITED n -> Exit n
    | Unix.WSIGNALED n | Unix.WSTOPPED n -> Killed n
  in
  (stdout, stderr, exit)

module Backend (F : FRAME) = struct
  module RA = RegisterAllocation (F)
  module TR = Translate (F)

  let fresh_compilation () = Temp.reset F.reserved_temps F.reserved_labels

  let string_of_temp t =
    match List.assoc_opt t F.temp_map with
    | Some r -> F.string_of_register r
    | None -> Temp.string_of_temp t

  let string_of_label = Temp.string_of_label

  let unzip_frags frags =
    List.fold_right
      (fun frag (frames, strings) ->
        match frag with
        | F.Proc (fr, body) -> ((fr, body) :: frames, strings)
        | F.String (lab, s) -> (frames, (lab, s) :: strings) )
      frags ([], [])

  let lower_canon expr =
    Escape.mark expr;
    let main, frags = expr |> desugar_expr |> TR.lower in
    let frames, strings = unzip_frags frags in
    let frames =
      frames
      |> List.map (fun (f, b) ->
             (f, b |> linearize |> basic_blocks |> trace_schedule |> simplify) )
    in
    (main, frames, strings)

  let codegen expr =
    let main, frames, strings = lower_canon expr in
    let frames =
      frames
      |> List.map (fun (f, b) ->
             (f, b |> List.concat_map (F.codegen f) |> F.proc_entry_exit2 f) )
    in
    (main, frames, strings)

  let compile expr =
    let main, frames, strings = codegen expr in
    let frames =
      frames
      |> List.map (fun (f, b) ->
             let b, a = RA.reg_alloc f b in
             (f, b, a) )
    in
    (main, frames, strings)

  let emit_assem expr =
    fresh_compilation ();
    let main, frames, strings = compile expr in
    F.emit strings frames main

  let emit_ir expr =
    fresh_compilation ();
    let _, frames, strings = lower_canon expr in
    let frames =
      frames
      |> List.map (fun (f, b) ->
             let b = Ir.string_of_irs string_of_temp b in
             Printf.sprintf "%s:\n%s"
               (F.name f |> string_of_label)
               (Print.reflow 2 b) )
      |> String.concat "\n\n"
    in
    let strings =
      strings
      |> List.map (fun (l, s) ->
             Printf.sprintf "%s:\n  \"%s\"" (string_of_label l)
               (String.escaped s) )
      |> String.concat "\n\n"
    in
    Printf.sprintf "%s\n\n%s" strings frames

  let exec expr stdin =
    fresh_compilation ();
    let main, frames, strings = compile expr in
    let prog = F.emit strings frames main in
    let entry = string_of_label main in
    match execution_target () with MacOS_11 -> exec_macos_11 prog stdin entry

  module Debug = struct
    let emit_pseudo_assem expr =
      fresh_compilation ();
      let main, frames, strings = codegen expr in
      let e = ref [] in
      (* Add liveness and intereference analysis. *)
      let interferences =
        List.concat_map
          (fun (_, instrs) ->
            let flowgraph, flownodes = Flow.flowgraph_of_instrs instrs in
            let ({graph = ifgraph; temp_of_ifnode; _} : Live.ifgraph), liveout =
              Live.interference_graph flowgraph
            in
            (* Add info on live registers for each instr. *)
            List.iter2
              (fun i n ->
                let live =
                  liveout n |> Temp.TempSet.to_seq |> List.of_seq
                  |> List.map string_of_temp |> String.concat " "
                in
                Assem.add_comment ("liveout: " ^ live) i )
              instrs flownodes;
            (* Determine interferences *)
            List.map
              (fun t ->
                let t, ns =
                  (temp_of_ifnode t, UDG.adj t |> List.map temp_of_ifnode)
                in
                e :=
                  Printf.sprintf "nbhd %s = %s" (string_of_temp t)
                    (String.concat " " (List.map string_of_temp ns))
                  :: !e;
                (t, ns) )
              (G.nodes ifgraph) )
          frames
      in
      (* Merge inteferences *)
      let alltemps = TempSet.of_list (List.map fst interferences) in
      let iftbl = Hashtbl.create (List.length interferences) in
      List.iter
        (fun (t, new_ifs) ->
          let old_ifs =
            match Hashtbl.find_opt iftbl t with
            | Some ifs ->
                Hashtbl.remove iftbl t;
                ifs
            | None -> TempSet.empty
          in
          Hashtbl.add iftbl t TempSet.(union old_ifs (of_list new_ifs)) )
        interferences;
      (* Format them *)
      let tolst s = TempSet.to_seq s |> List.of_seq in
      let fmt_interferences =
        tolst alltemps
        |> List.map (fun t ->
               Printf.sprintf "%s: %s" (string_of_temp t)
                 ( tolst (Hashtbl.find iftbl t)
                 |> List.map string_of_temp |> String.concat " " ) )
        |> String.concat "\n"
      in
      let asm_body = F.Debug.emit_debug strings frames string_of_temp main in
      String.concat "\n"
        ([asm_body; ""; "=== Interferences ==="; ""; fmt_interferences] @ !e)
  end
end
