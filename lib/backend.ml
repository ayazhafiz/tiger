open Canon
open Desugar
open Escape
open Frame
open Lower
open Reg_alloc

module Backend (F : FRAME) = struct
  module RA = RegisterAllocation (F)
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
               |> List.concat_map (F.codegen f)
               |> F.proc_entry_exit2 f |> RA.reg_alloc f )
             |> fun (f, (i, a)) -> (f, i, a) )
    in
    (frames, strings)

  let emit_assem expr =
    let frames, strings = compile expr in
    F.emit strings frames
end
