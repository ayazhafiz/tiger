open Flow
open Graph
open Temp
module DG = DirectedGraph

let ice why = failwith ("ICE (live): " ^ why)

type ifnode = temp DG.node

type ifgraph =
  { graph : temp DG.graph
        (** DGraph with information of the temporary corresponding to that node. *)
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

type liveflownode = liveflow_data DG.node
type liveflowgraph = liveflow_data DG.graph
type liveout_of_node = liveflownode -> TempSet.t

let interference_graph (flowgraph : flowgraph) :
    ifgraph * liveflowgraph * liveout_of_node =
  (* TODO: this function should really be modularized... *)
  let lgraph =
    DG.map flowgraph (fun ({defs; uses; is_mov} : instr_data) ->
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
        let data = DG.get_data node in
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
        let successors = DG.succ node |> List.map DG.get_data in
        data.live_out <-
          List.fold_left
            (fun live_out succ -> TempSet.union live_out succ.live_in)
            TempSet.empty successors;
        fixedpoint :=
          !fixedpoint
          && TempSet.equal live_in1 data.live_in
          && TempSet.equal live_out1 data.live_out)
      (DG.nodes lgraph);
    if not !fixedpoint then solve ()
  in
  (* Solve dataflow equations *)
  solve ();
  (* Figure out all the temporaries in our program; allocate space in an
     interference graph for them, and construct the temp<=>node mappings. *)
  let alltemps =
    List.fold_left
      (fun all node ->
        let {defs; uses; _} = DG.get_data node in
        TempSet.(union all (union defs uses)))
      TempSet.empty (DG.nodes lgraph)
  in
  let ifgraph = DG.new_graph () in
  let ifnode_of_temp = Hashtbl.create (TempSet.cardinal alltemps) in
  TempSet.iter
    (fun temp ->
      let ifnode = DG.new_node ifgraph in
      DG.add_data ifnode temp;
      Hashtbl.add ifnode_of_temp temp ifnode)
    alltemps;
  (* Time to populate the graph. *)
  let moves = ref [] in
  List.iter
    (fun node ->
      let data = DG.get_data node in
      let movein_lab =
        match (data.is_mov, data.uses |> TempSet.to_seq |> List.of_seq) with
        | true, [lab] -> Some lab
        | true, _ -> ice "move doesn't have exactly one label"
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
             |> TempSet.iter (fun ttemp ->
                    let t = Hashtbl.find ifnode_of_temp ttemp in
                    match movein_lab with
                    (* Unless the instruction we detect the interference in is a
                       move, and the source t of the move is live out of this
                       node. This is okay because d is now equal to t! *)
                    | Some source when source = ttemp ->
                        moves := (d, t) :: !moves
                    | _ -> DG.add_edge d t)))
    (DG.nodes lgraph);
  let ifgraph =
    { graph = ifgraph
    ; ifnode_of_temp = Hashtbl.find ifnode_of_temp
    ; temp_of_ifnode = DG.get_data
    ; moves = !moves }
  in
  let liveout_of_node node = (DG.get_data node).live_out in
  (ifgraph, lgraph, liveout_of_node)
