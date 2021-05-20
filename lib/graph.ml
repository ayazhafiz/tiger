let ice why = failwith ("ICE (graph): " ^ why)

module GraphCollection = struct
  type node' = int

  module NodeSet = Set.Make (struct
    type t = node'

    let compare = compare
  end)

  type edges = {mutable pred : NodeSet.t; mutable succ : NodeSet.t}

  type 'a graph =
    { adjlist : (node', edges) Hashtbl.t
    ; mutable next_node : node'
    ; data : (node', 'a) Hashtbl.t }

  type 'a node = node' * 'a graph

  let new_graph () =
    {adjlist = Hashtbl.create 32; next_node = 0; data = Hashtbl.create 32}

  let new_node graph =
    let node = graph.next_node in
    Hashtbl.add graph.adjlist node {pred = NodeSet.empty; succ = NodeSet.empty};
    graph.next_node <- graph.next_node + 1;
    (node, graph)

  let check g1 g2 = if g1 == g2 then () else ice "assigned to different graphs"
  let into_node g n = (n, g)

  let nodes g =
    Hashtbl.to_seq_keys g.adjlist |> Seq.map (into_node g) |> List.of_seq

  let eq (n1, g1) (n2, g2) =
    check g1 g2;
    n1 = n2

  let add_data (n, {data; _}) = Hashtbl.add data n
  let get_data (n, {data; _}) = Hashtbl.find data n

  let map {adjlist; next_node; data} mapper =
    { adjlist
    ; next_node
    ; data =
        Hashtbl.to_seq data
        |> Seq.map (fun (k, v) -> (k, mapper v))
        |> Hashtbl.of_seq }

  let directed_add_edge (n1, g1) (n2, g2) =
    check g1 g2;
    let edges1 = Hashtbl.find g1.adjlist n1 in
    edges1.succ <- NodeSet.add n2 edges1.succ;
    let edges2 = Hashtbl.find g1.adjlist n2 in
    edges2.pred <- NodeSet.add n1 edges2.pred

  let directed_rm_edge (n1, g1) (n2, g2) =
    check g1 g2;
    let edges1 = Hashtbl.find g1.adjlist n1 in
    edges1.succ <- NodeSet.remove n2 edges1.succ;
    let edges2 = Hashtbl.find g1.adjlist n2 in
    edges2.pred <- NodeSet.remove n1 edges2.pred
end

module DirectedGraph = struct
  include GraphCollection

  let add_edge = directed_add_edge
  let rm_edge = directed_rm_edge

  let succ (n, g) =
    (Hashtbl.find g.adjlist n).succ |> NodeSet.to_seq
    |> Seq.map (into_node g)
    |> List.of_seq

  let pred (n, g) =
    (Hashtbl.find g.adjlist n).pred |> NodeSet.to_seq
    |> Seq.map (into_node g)
    |> List.of_seq
end

module UndirectedGraph = struct
  include GraphCollection

  let add_edge n1 n2 =
    directed_add_edge n1 n2;
    directed_add_edge n2 n1

  let rm_edge n1 n2 =
    directed_rm_edge n1 n2;
    directed_rm_edge n2 n1

  let adj (n, g) =
    let edges = Hashtbl.find g.adjlist n in
    NodeSet.union edges.pred edges.succ
    |> NodeSet.to_seq
    |> Seq.map (into_node g)
    |> List.of_seq
end
