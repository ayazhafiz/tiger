open Canon
open Codegen
open Desugar
open Escape
open Frame
open Lower
open Reg_alloc

module Backend (F : FRAME) (CG : CODEGEN with module F = F) = struct
  module RA = RegisterAllocation (F) (CG)
  module TR = Translate (F)

  let unzip_frags frags =
    List.fold_right
      (fun frag (frames, strings) ->
        match frag with
        | F.Proc (fr, body) -> ((fr, body) :: frames, strings)
        | F.String (lab, s) -> (frames, (lab, s) :: strings) )
      frags ([], [])

  let compile expr =
    mark_escapes expr;
    let frames, strings = expr |> desugar_expr |> TR.lower |> unzip_frags in
    let frames =
      frames
      |> List.map (fun (f, b) ->
             ( f
             , b |> linearize |> basic_blocks |> trace_schedule
               |> List.concat_map (CG.codegen f)
               |> F.proc_entry_exit2 f |> RA.reg_alloc f ) )
    in
    (frames, strings)

  let emit_frame (frame, (instrs, coloring)) =
    let open Assem in
    let ({prolog; body; epilog} : F.proc) = F.proc_entry_exit3 frame instrs in
    let string_of_temp t = t |> Hashtbl.find coloring |> F.string_of_register in
    let body =
      body
      |> List.map (string_of_instr string_of_temp)
      |> List.map (fun s -> F.assem_body_indent ^ s)
      |> String.concat "\n"
    in
    String.concat "\n" [prolog; body; epilog]

  let emit_assem expr =
    let frames, strings = compile expr in
    let asm_strings = List.map F.assem_of_string strings in
    let asm_frames = List.map emit_frame frames in
    F.assem_complete asm_strings asm_frames
end
