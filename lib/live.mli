open Flow
open Temp

(** A control flow graph, extended with liveness data. *)
type liveflowgraph

(** A node in a [liveflowgraph] *)
type liveflownode

(** An interference graph. *)
type ifgraph

(** A node in an [ifgraph]. *)
type ifnode

(** A mapping of [flownode]s to the temporaries that are live-out at that
    [flownode]. *)
type liveout_of_node = liveflownode -> TempSet.t

val interference_graph : flowgraph -> ifgraph * liveflowgraph * liveout_of_node
(** Given a [flowgraph], produces a tuple [(ifgraph, lgraph, liveout_of_node)]
    with the following properties:
      - [lgraph] is a graph with the same node mappings as [flowgraph], extended
        with liveness analysis.
      - [nodes lgraph = nodes flowgraph]
      - [ifgraph] is a graph with temporary interference analysis, constructed
        from [lgraph].
      - [liveout_of_node] is a mapping of nodes N in [lgraph] to the temporaries
        that are live out of N. *)
