open Flow
open Graph
open Temp

(** A node in an [ifgraph]. *)
type ifnode = temp Graph.node

(** An interference graph. *)
type ifgraph =
  { graph : temp Graph.graph
        (** Graph with information of the temporary corresponding to that node. *)
  ; ifnode_of_temp : temp -> ifnode
  ; temp_of_ifnode : ifnode -> temp
  ; moves : (ifnode * ifnode) list }

(** A mapping of [flownode]s to the temporaries that are live-out at that
    [flownode]. *)
type liveout_of_node = flownode -> TempSet.t

val interference_graph : flowgraph -> ifgraph * liveout_of_node
(** Given a [flowgraph], produces a tuple [(ifgraph, liveout_of_node)]
    with the following properties:
      - [ifgraph] is a graph with temporary interference analysis, constructed
        from [lgraph].
      - [liveout_of_node] is a mapping of nodes N in [lgraph] to the temporaries
        that are live out of N. *)
