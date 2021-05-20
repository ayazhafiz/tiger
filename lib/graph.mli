module GraphCollection : sig
  type 'a graph
  type 'a node

  val new_graph : unit -> 'a graph
  val new_node : 'a graph -> 'a node
  val nodes : 'a graph -> 'a node list
  val eq : 'a node -> 'a node -> bool
  val add_data : 'a node -> 'a -> unit
  val get_data : 'a node -> 'a
  val map : 'a graph -> ('a -> 'b) -> 'b graph
end

(** A mutable directed graph. *)
module DirectedGraph : sig
  include module type of GraphCollection

  val add_edge : 'a node -> 'a node -> unit
  val rm_edge : 'a node -> 'a node -> unit
  val succ : 'a node -> 'a node list
  val pred : 'a node -> 'a node list
end

(** A mutable undirected graph. *)
module UndirectedGraph : sig
  include module type of GraphCollection

  val add_edge : 'a node -> 'a node -> unit
  val rm_edge : 'a node -> 'a node -> unit
  val adj : 'a node -> 'a node list
end
