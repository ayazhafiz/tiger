open Frame
open Temp

let ice why = failwith ("ICE (reg_alloc):" ^ why)

module RegisterAllocation (F : FRAME) = struct
  type allocation = (temp, F.register) Hashtbl.t

  (*******************************)
  (* Interference Graph Coloring *)
  (*******************************)

  (** [color ifgraph initial spill_cost registers] colors the temporaries in the
      interference graph [ifgraph] using [registers]. [initial] is a table of
      pre-colored registers, and [spill_cost] is used to prioritize what
      temporaries to spill, if any.
      Returns a pair [(coloring, spilled)] with the coloring of all temporaries,
      and any temporaries that must be spilled. *)
  let color (interference : Live.ifgraph) (precolored : allocation)
      (spill_cost : Live.ifnode -> int) (registers : F.register list) :
      allocation * temp list =
    let _ = interference in
    let _ = precolored in
    let _ = spill_cost in
    let _ = registers in
    ice "not implemented"

  let reg_alloc (instrs : Assem.instr list) (frame : F.frame) :
      Assem.instr list * allocation =
    let _ = instrs in
    let _ = frame in
    ice "not implemented"
end
