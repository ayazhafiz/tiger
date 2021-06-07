open Data
open Graph

type instr_data =
  { defs : Temp.temp list
        (** Temporaries defined by this instruction (i.e. destination registers) *)
  ; uses : Temp.temp list
        (** Uses of temporaries in this instruction (i.e. source registers) *)
  ; is_mov : bool  (** Is this a move instruction? *) }

(** A control flow graph. *)
type flowgraph = instr_data Graph.graph

(** A node in a [flowgraph] *)
type flownode = instr_data Graph.node

val flowgraph_of_instrs : Assem.instr list -> flowgraph * flownode list
(** Constructs a [flowgraph] from a list of instructions, returning the
    flowgraph and a list of nodes in the graph with horizontal 1-to-1
    correspondence with the input list of instructions. *)
