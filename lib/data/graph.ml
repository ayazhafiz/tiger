let ice why = failwith ("ICE (graph): " ^ why)

module Graph = struct
  type node' = int

  module INodeSet = Set.Make (struct
    type t = node'

    let compare = compare
  end)

  type edges = {mutable pred : INodeSet.t; mutable succ : INodeSet.t}

  type 'a graph =
    { adjlist : (node', edges) Hashtbl.t
    ; mutable next_node : node'
    ; data : (node', 'a) Hashtbl.t }

  type 'a node = node' * 'a graph

  let new_graph () =
    {adjlist = Hashtbl.create 32; next_node = 0; data = Hashtbl.create 32}

  let new_node graph data =
    let node = graph.next_node in
    Hashtbl.add graph.adjlist node {pred = INodeSet.empty; succ = INodeSet.empty};
    Hashtbl.add graph.data node data;
    graph.next_node <- graph.next_node + 1;
    (node, graph)

  let check g1 g2 = if g1 == g2 then () else ice "assigned to different graphs"
  let into_node g n = (n, g)

  let nodes g =
    Hashtbl.to_seq_keys g.adjlist |> Seq.map (into_node g) |> List.of_seq

  let eq (n1, g1) (n2, g2) =
    check g1 g2;
    n1 = n2

  let set_data (n, {data; _}) = Hashtbl.add data n
  let get_data (n, {data; _}) = Hashtbl.find data n

  let map {adjlist; next_node; data} mapper =
    let newg =
      { adjlist = Hashtbl.copy adjlist
      ; next_node
      ; data =
          Hashtbl.to_seq data
          |> Seq.map (fun (k, v) -> (k, mapper v))
          |> Hashtbl.of_seq }
    in
    let nodemap (n, _) = (n, newg) in
    (newg, nodemap)

  let directed_add_edge (n1, g1) (n2, g2) =
    check g1 g2;
    let edges1 = Hashtbl.find g1.adjlist n1 in
    edges1.succ <- INodeSet.add n2 edges1.succ;
    let edges2 = Hashtbl.find g1.adjlist n2 in
    edges2.pred <- INodeSet.add n1 edges2.pred

  let directed_rm_edge (n1, g1) (n2, g2) =
    check g1 g2;
    let edges1 = Hashtbl.find g1.adjlist n1 in
    edges1.succ <- INodeSet.remove n2 edges1.succ;
    let edges2 = Hashtbl.find g1.adjlist n2 in
    edges2.pred <- INodeSet.remove n1 edges2.pred

  module NodeSet = struct
    module S = INodeSet

    type 'a t = S.t * 'a graph

    let of_list = function
      | (_, g) :: _ as nodes -> (List.map fst nodes |> S.of_list, g)
      | _ -> ice "cannot create set from empty list"

    let empty_with_graph g = (S.empty, g)
    let clear (_, g) = empty_with_graph g

    let add (n, g1) (s1, g2) =
      check g1 g2;
      (S.add n s1, g1)

    let remove (n, g1) (s1, g2) =
      check g1 g2;
      (S.remove n s1, g1)

    let inter (s1, g1) (s2, g2) =
      check g1 g2;
      (S.inter s1 s2, g1)

    let union (s1, g1) (s2, g2) =
      check g1 g2;
      (S.union s1 s2, g1)

    let difference (s1, g1) (s2, g2) =
      check g1 g2;
      (S.diff s1 s2, g1)

    let choose (s, g) = (S.choose s, g)

    let mem (n, g1) (s, g2) =
      check g1 g2;
      S.mem n s

    let ( ^^ ) = inter
    let ( ++ ) = union
    let ( // ) = difference
    let is_empty (s, _) = S.is_empty s
    let iter pred (set, g) = S.iter (fun n -> pred (n, g)) set
    let fold pred (set, g) init = S.fold (fun n v -> pred (n, g) v) set init
    let for_all pred (set, g) = S.for_all (fun n -> pred (n, g)) set
    let size (s, _) = S.cardinal s
  end

  module DirectedGraph = struct
    let add_edge = directed_add_edge
    let rm_edge = directed_rm_edge

    let succ (n, g) =
      (Hashtbl.find g.adjlist n).succ |> INodeSet.to_seq
      |> Seq.map (into_node g)
      |> List.of_seq

    let pred (n, g) =
      (Hashtbl.find g.adjlist n).pred |> INodeSet.to_seq
      |> Seq.map (into_node g)
      |> List.of_seq
  end

  module UndirectedGraph = struct
    let add_edge n1 n2 =
      directed_add_edge n1 n2;
      directed_add_edge n2 n1

    let rm_edge n1 n2 =
      directed_rm_edge n1 n2;
      directed_rm_edge n2 n1

    let adj (n, g) =
      let edges = Hashtbl.find g.adjlist n in
      INodeSet.union edges.pred edges.succ
      |> INodeSet.to_seq
      |> Seq.map (into_node g)
      |> List.of_seq
  end
end

module DirectedGraph = Graph.DirectedGraph
module UndirectedGraph = Graph.UndirectedGraph
