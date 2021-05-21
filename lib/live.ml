open Flow
open Graph
open Temp
module G = Graph
module DG = DirectedGraph
module UDG = DirectedGraph

let ice why = failwith ("ICE (live): " ^ why)

type ifnode = temp G.node

type ifgraph =
  { graph : temp G.graph
  ; ifnode_of_temp : temp -> ifnode
  ; temp_of_ifnode : ifnode -> temp
  ; moves : (ifnode * ifnode) list }

type liveflow_data =
  { mutable live_in : TempSet.t
        (** What is live at entry to each flowgraph node? *)
  ; mutable live_out : TempSet.t
        (** What is live at exit of each flowgraph node? *)
  ; defs : TempSet.t
  ; uses : TempSet.t
  ; is_mov : bool }

type liveout_of_node = flownode -> TempSet.t

let interference_graph (flowgraph : flowgraph) : ifgraph * liveout_of_node =
  (* TODO: this function should really be modularized... *)
  let lgraph, liveflownode_of_flownode =
    G.map flowgraph (fun ({defs; uses; is_mov} : instr_data) ->
        { live_in = TempSet.empty
        ; live_out = TempSet.empty
        ; defs = TempSet.of_list defs
        ; uses = TempSet.of_list uses
        ; is_mov })
  in
  (* page 214, Algorithm 10.4 *)
  let rec solve () =
    let fixedpoint = ref true in
    List.iter
      (fun node ->
        let data = G.get_data node in
        let live_in1 = data.live_in in
        let live_out1 = data.live_out in
        (* If a temporary
             - is used at this node
             - or is live-out at this node, and not defined by this node
           then it is live-in at this node. *)
        data.live_in <-
          TempSet.union data.uses (TempSet.diff data.live_out data.defs);
        (* If a temporary
             - is live-in at any successor to this node
           then it is live-in at this node. *)
        let successors = DG.succ node |> List.map G.get_data in
        data.live_out <-
          List.fold_left
            (fun live_out succ -> TempSet.union live_out succ.live_in)
            TempSet.empty successors;
        fixedpoint :=
          !fixedpoint
          && TempSet.equal live_in1 data.live_in
          && TempSet.equal live_out1 data.live_out)
      (G.nodes lgraph);
    if not !fixedpoint then solve ()
  in
  (* Solve dataflow equations *)
  solve ();
  (* Figure out all the temporaries in our program; allocate space in an
     interference graph for them, and construct the temp<=>node mappings. *)
  let alltemps =
    List.fold_left
      (fun all node ->
        let {defs; uses; _} = G.get_data node in
        TempSet.(union all (union defs uses)))
      TempSet.empty (G.nodes lgraph)
  in
  let ifgraph = G.new_graph () in
  let ifnode_of_temp = Hashtbl.create (TempSet.cardinal alltemps) in
  TempSet.iter
    (fun temp ->
      let ifnode = G.new_node ifgraph temp in
      Hashtbl.add ifnode_of_temp temp ifnode)
    alltemps;
  (* Time to populate the graph. *)
  let moves = ref [] in
  List.iter
    (fun node ->
      let data = G.get_data node in
      (* Is this a move, and if so what temp does it use? *)
      let move_use =
        match (data.is_mov, data.uses |> TempSet.to_seq |> List.of_seq) with
        | true, [lab] -> Some lab
        | true, _ -> ice "move doesn't have exactly one use"
        | _ -> None
      in
      (* When is there an intereference? If a temp d is defined at a node, it
         interferes with all temps {t_1, ..., t_n} that are live out of that
         node (i.e. we could not allocate them in the same concrete register,
         since {t_1, ..., t_n} still need to be used). *)
      data.defs
      |> TempSet.iter (fun d ->
             let d = Hashtbl.find ifnode_of_temp d in
             data.live_out
             |> TempSet.iter (fun t ->
                    let t' = Hashtbl.find ifnode_of_temp t in
                    (* Don't mark self-interference. *)
                    if t' <> d then
                      match move_use with
                      (* Unless the instruction we detect the interference in
                         is a move, and the source t of the move is live out of
                         this node. This is okay because d is now equal to t! *)
                      | Some source when source = t ->
                          moves := (d, t') :: !moves
                      | _ -> UDG.add_edge d t')))
    (G.nodes lgraph);
  let ifgraph =
    { graph = ifgraph
    ; ifnode_of_temp = Hashtbl.find ifnode_of_temp
    ; temp_of_ifnode = G.get_data
    ; moves = !moves }
  in
  let liveout_of_node node =
    let data = liveflownode_of_flownode node |> G.get_data in
    data.live_out
  in
  (ifgraph, liveout_of_node)
