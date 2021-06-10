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

let readfi path =
  let ch = open_in path in
  let content = really_input_string ch (in_channel_length ch) in
  close_in ch;
  content

let writefi path content =
  let ch = open_out path in
  output_string ch content;
  close_out ch

type machine =
  | X86_64_apple_darwin20_1_0
  | X86_64_apple_darwin20_6_0
  | X86_64_linux_gnu

let all_machines =
  [X86_64_apple_darwin20_1_0; X86_64_apple_darwin20_6_0; X86_64_linux_gnu]

let triple_machine_tbl =
  [ ("x86_64-apple-darwin20.1.0", X86_64_apple_darwin20_1_0)
  ; ("x86_64-apple-darwin20.6.0", X86_64_apple_darwin20_6_0)
  ; ("x86_64-linux-gnu", X86_64_linux_gnu) ]

let machine_triple_tbl = List.map (fun (a, b) -> (b, a)) triple_machine_tbl

let machine_of_triple t =
  let fail () = failwith ("Unknown execution target " ^ t) in
  match List.assoc_opt t triple_machine_tbl with Some m -> m | None -> fail ()

let triple_of_machine m = List.assoc m machine_triple_tbl
let lkg_runtime m = Printf.sprintf "runtime/lkg/%s.o" (triple_of_machine m)

type execspec =
  { assemble : string -> string -> string  (** [assemble asmfile outfile] *)
  ; link : string -> string -> string -> string -> string
        (** [link entry objfile objruntime outfile] *) }

let execspec : (machine * execspec) list =
  let apple_darwin_20 =
    { assemble =
        (fun asmfile outfile ->
          Printf.sprintf "nasm -f macho64 %s -o %s" asmfile outfile )
    ; link =
        (fun entry objfile objruntime outfile ->
          let ld_lib_paths =
            ["-L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"]
          in
          let ld_libs = ["-lSystem"] in
          let ld_args = ld_lib_paths @ ld_libs |> String.concat " " in
          Printf.sprintf
            "ld %s %s -o %s -e %s -arch x86_64 -macosx_version_min 11.0 %s"
            objfile objruntime outfile entry ld_args ) }
  in
  [ (X86_64_apple_darwin20_1_0, apple_darwin_20)
  ; (X86_64_apple_darwin20_6_0, apple_darwin_20)
  ; ( X86_64_linux_gnu
    , { assemble =
          (fun asmfile outfile ->
            Printf.sprintf "nasm -f elf64 %s -o %s" asmfile outfile )
      ; link =
          (fun entry objfile objruntime outfile ->
            Printf.sprintf
              "ld -e %s -m elf_x86_64 \
               --dynamic-linker=/lib64/ld-linux-x86-64.so.2 %s %s -lc -o %s"
              entry objfile objruntime outfile ) } ) ]

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

let current_machine () =
  let triple, _, _ = sh "gcc -dumpmachine" in
  machine_of_triple triple

type exit_status = Exit of int | Killed of int

let lex = Lexing.from_string ~with_positions:true
let parse = Front.Parser.toplevel Front.Lexer.read
let pretty_print = Front.Language.string_of_expr

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

  let emit_exe1 machine expr =
    fresh_compilation ();
    let main, frames, strings = compile expr in
    let prog = F.emit strings frames main in
    let entry = string_of_label main in
    let temp_asm = Filename.temp_file "main" ".asm" in
    let temp_o = Filename.temp_file "main" ".o" in
    let temp_exe = Filename.temp_file "main" ".exe" in
    let runtime_o = lkg_runtime machine in
    let {assemble; link} = List.assoc machine execspec in
    writefi temp_asm prog;
    sh (assemble temp_asm temp_o) |> assert_exit0;
    sh (link entry temp_o runtime_o temp_exe) |> assert_exit0;
    temp_exe

  let emit_exe machine expr = readfi (emit_exe1 machine expr)

  let exec machine expr stdin =
    let temp_exe = emit_exe1 machine expr in
    let stdout, stderr, exit = sh ~stdin temp_exe in
    let exit =
      match exit with
      | Unix.WEXITED n -> Exit n
      | Unix.WSIGNALED n | Unix.WSTOPPED n -> Killed n
    in
    (stdout, stderr, exit)

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
