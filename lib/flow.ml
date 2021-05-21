open Graph

let ice why = failwith ("ICE (graph): " ^ why)

type instr_data = {defs : Temp.temp list; uses : Temp.temp list; is_mov : bool}
type flowgraph = instr_data Graph.graph
type flownode = instr_data Graph.node

let flownode_of_instr = function
  | Assem.Oper {dst; src; _} -> {defs = dst; uses = src; is_mov = false}
  | Assem.Label _ -> {defs = []; uses = []; is_mov = false}
  | Assem.Mov {dst; src; _} -> {defs = [dst]; uses = [src]; is_mov = true}

let does_jump = function
  | Assem.Oper {jmp; _} -> Option.is_some jmp
  | Assem.Mov _ | Assem.Label _ -> false

let jumps_of_instr = function
  | Assem.Oper {jmp; _} -> (
    match jmp with
    | None | Some [] -> ice "attempted to unwrap empty jumps"
    | Some jumps -> jumps )
  | Assem.Mov _ | Assem.Label _ -> ice "moves, labels have no jumps"

let flowgraph_of_instrs instrs =
  let graph = Graph.new_graph () in
  (* 1. allocate all nodes *)
  let flownodes =
    List.map
      (fun i ->
        let node = Graph.new_node graph (flownode_of_instr i) in
        (i, node))
      instrs
  in
  (* 2. assemble table label->node *)
  let node_of_label = Hashtbl.create 128 in
  List.iter
    (function
      | Assem.Label {lab; _}, node -> Hashtbl.add node_of_label lab node
      | _ -> ())
    flownodes;
  (* 3. create flow graph. *)
  let rec sequence = function
    | [] -> ()
    | (i, node) :: rest when does_jump i ->
        jumps_of_instr i
        |> List.iter (fun lab ->
               let lab_node = Hashtbl.find node_of_label lab in
               DirectedGraph.add_edge node lab_node);
        sequence rest
    | [_last_no_jump] -> ()
    | (_, node1) :: ((_, node2) :: _ as rest) ->
        (* No direct jump from i1; mark node1 as going into node2. *)
        DirectedGraph.add_edge node1 node2;
        sequence rest
  in
  sequence flownodes;
  (graph, List.map snd flownodes)
