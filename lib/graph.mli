(** A mutable graph with data presentations on nodes. *)
module Graph : sig
  type 'a graph
  type 'a node

  val new_graph : unit -> 'a graph
  val new_node : 'a graph -> 'a -> 'a node
  val nodes : 'a graph -> 'a node list
  val eq : 'a node -> 'a node -> bool
  val set_data : 'a node -> 'a -> unit
  val get_data : 'a node -> 'a

  val map : 'a graph -> ('a -> 'b) -> 'b graph * ('a node -> 'b node)
  (** Maps a graph from one type of data to another, returning also a mapping
      function to get from a node in the original graph to one in the new graph. *)
end

(** A mutable directed graph. *)
module DirectedGraph : sig
  val add_edge : 'a Graph.node -> 'a Graph.node -> unit
  val rm_edge : 'a Graph.node -> 'a Graph.node -> unit
  val succ : 'a Graph.node -> 'a Graph.node list
  val pred : 'a Graph.node -> 'a Graph.node list
end

(** A mutable undirected graph. *)
module UndirectedGraph : sig
  val add_edge : 'a Graph.node -> 'a Graph.node -> unit
  val rm_edge : 'a Graph.node -> 'a Graph.node -> unit
  val adj : 'a Graph.node -> 'a Graph.node list
end
