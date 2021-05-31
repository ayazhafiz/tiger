(* vim: foldmethod=marker
 *)
open Frame
open Temp
module LL = Linked_list
module G = Graph.Graph
module UDG = Graph.UndirectedGraph
module NS = G.NodeSet

let ice why = failwith ("ICE (reg_alloc):" ^ why)
let pair a b = (a, b)

let tbl_update tbl key default updatefn =
  let v = Hashtbl.find_opt tbl key |> Option.value ~default in
  Hashtbl.add tbl key (updatefn v)

module RegisterAllocation (F : FRAME) = struct
  (*******************************)
  (* Interference Graph Coloring *)
  (*******************************)
  let color (instrs : Assem.instr list) (spill_cost : Live.ifnode -> int) :
      F.allocation * temp list =
    (* Control flow graph and list of nodes in that graph. *)
    let flowgraph, flownodes = Flow.flowgraph_of_instrs instrs in
    (* Alist in 1-1 correspondence between [instrs] and nodes in [flowgraph]. *)
    let instrs_join_flownodes = List.map2 pair instrs flownodes in
    let instr_of_flownode =
      List.map2 pair flownodes instrs |> List.to_seq |> Hashtbl.of_seq
    in
    (* Interference graph and fn to retrieve live-out temps at a [flownode]. *)
    let ({graph = ifgraph; ifnode_of_temp; temp_of_ifnode; moves}, _)
          : Live.ifgraph * _ =
      Live.interference_graph flowgraph
    in
    let ifnodes = G.nodes ifgraph in
    let n_ifnodes = List.length ifnodes in
    let n_ifgraph_moves = List.length moves in
    (* Number of registers. *)
    let k = List.length F.registers in
    (* Registers that are pre-assigned a color. *)
    let precolored, precolored_nodes =
      let fixed = F.temp_map |> List.map fst in
      (TempSet.of_list fixed, List.map ifnode_of_temp fixed |> NS.of_list)
    in
    (* Temporary registers not precolored and not yet processed. *)
    let initial = ref NS.(of_list ifnodes // precolored_nodes) in
    (* Low-degree, non-move-related nodes. Available for simplification. *)
    let wl_simplify = ref (NS.empty_with_graph ifgraph) in
    (* Low-degree, move-related nodes. Available for possible coalescing, or
       freezing into [wl_simplify] if coalescing is not possible. *)
    let wl_freeze = ref (NS.empty_with_graph ifgraph) in
    (* High-degree nodes that cannot (yet) be used to simplify an interference
       graph. These nodes may have to be spilled. *)
    let wl_spill = ref (NS.empty_with_graph ifgraph) in
    (* Nodes marked for spilling in an allocation round. *)
    let spilled_nodes = ref [] in
    (* Nodes that have been coalesced. This only contains redundant nodes; that
       is, if the move [u<-v] is coalesced, then [v] is in this container, and
       [u] is in a worklist. *)
    let coalesced_nodes = ref (NS.empty_with_graph ifgraph) in
    (* Nodes successfully colored. *)
    let colored_nodes = ref (NS.empty_with_graph ifgraph) in
    (* Stack containing temporaries removed from the graph. *)
    let select_stack = ref ([], NS.empty_with_graph ifgraph) in
    let select_stack_push n =
      select_stack :=
        let stk, set = !select_stack in
        (n :: stk, NS.add n set)
    in
    (* Moves that have been coalesced. *)
    let coalesced_moves = ref [] in
    (* Moves that are constrained; i.e. cannot be coalesced b/c the source and
       target interfere. *)
    (* *)
    let constrained_moves = ref [] in
    (* Moves no longer considered for coalescing. *)
    let frozen_moves = ref [] in
    (* Moves enabled for possible coalescing. *)
    let worklist_moves = ref (NS.empty_with_graph flowgraph) in
    (* Moves not yet ready for coalescing. *)
    let active_moves = ref (NS.empty_with_graph flowgraph) in
    (* Set of interference edges between temporaries among [ifgraph]. *)
    let adj_set = Hashtbl.create n_ifnodes in
    (* Adjacency list of interference edges. *)
    let adj_list = Hashtbl.create n_ifnodes in
    (* Mapping of nodes to their current interference degree. *)
    let degree = Hashtbl.create n_ifnodes in
    let degree_of = Hashtbl.find degree in
    (* Mapping of nodes to the moves they are involved in. *)
    let move_list = Hashtbl.create (n_ifgraph_moves * 2) in
    (* Mapping of coalesced nodes to the target of the coalescing.
       For example when [u<-v] is coalesced, [v] is put in [coalesced_moves] and
       [alias{v}=u]. *)
    let alias = Hashtbl.create n_ifgraph_moves in
    (* Color chosen for a node. *)
    let color =
      let t = Hashtbl.create n_ifnodes in
      (* init precolored nodes *)
      List.to_seq F.temp_map |> Hashtbl.add_seq t;
      t
    in
    let color_of = Hashtbl.find color in
    (* Invariants that hold after build. {{{ *)
    let assert_build_invariants () =
      let degree_invariant () =
        (* (u ∈ wl_simplify ∪ wl_freeze ∪ wl_spill) =>
           degree(u) = |adjList(w) ∩ (precolored ∪ wl_simplify ∪
                                      wl_freeze ∪ wl_spill)| *)
        let sfs = NS.(!wl_simplify ++ !wl_freeze ++ !wl_spill) in
        let psfs = NS.(sfs ++ precolored_nodes) in
        NS.iter
          (fun u ->
            assert (degree_of u = NS.(Hashtbl.find adj_list u ^^ psfs |> size))
            )
          sfs
      in
      let wl_simplify_invariant () =
        (* u ∈ wl_simplify =>
           degree(u) < K
           ∧ move_list[u] ∩ (active_moves ∪ worklist_moves) = ∅ *)
        NS.iter
          (fun u ->
            assert (degree_of u < k);
            assert (
              NS.(
                Hashtbl.find move_list u ^^ (!active_moves ++ !worklist_moves)
                |> is_empty) ) )
          !wl_simplify
      in
      let wl_freeze_invariant () =
        (* u ∈ wl_freeze =>
           degree(u) < K
           ∧ move_list[u] ∩ (active_moves ∪ worklist_moves) ≠ ∅ *)
        NS.iter
          (fun u ->
            assert (degree_of u < k);
            assert (
              NS.(
                Hashtbl.find move_list u ^^ (!active_moves ++ !worklist_moves)
                |> is_empty |> not) ) )
          !wl_freeze
      in
      let wl_spill_invariant () =
        (* u ∈ wl_spill => degree(u) >= K *)
        NS.iter (fun u -> assert (degree_of u >= k)) !wl_spill
      in
      (* *)
      degree_invariant ();
      wl_simplify_invariant ();
      wl_freeze_invariant ();
      wl_spill_invariant ()
    in
    (* }}} *)
    let add_edge u v =
      if (not (Hashtbl.mem adj_set (u, v))) && not (G.eq u v) then (
        Hashtbl.add adj_set (u, v) ();
        Hashtbl.add adj_set (v, u) ();
        if not (TempSet.mem (temp_of_ifnode u) precolored) then (
          tbl_update adj_list u (NS.empty_with_graph ifgraph) (NS.add v);
          tbl_update degree u 0 succ );
        if not (TempSet.mem (temp_of_ifnode v) precolored) then (
          tbl_update adj_list v (NS.empty_with_graph ifgraph) (NS.add u);
          tbl_update degree v 0 succ ) )
    in
    (* *)
    let build () =
      let movelist_add temp move =
        let tnode = ifnode_of_temp temp in
        tbl_update move_list tnode (NS.empty_with_graph flowgraph) (NS.add move)
      in
      List.iter
        (fun (i, n) ->
          match i with
          | Assem.Mov {src; dst; _} ->
              movelist_add src n;
              movelist_add dst n;
              worklist_moves := NS.add n !worklist_moves
          | _ -> () )
        instrs_join_flownodes;
      List.iter (fun u -> List.iter (fun v -> add_edge u v) (UDG.adj u)) ifnodes
    in
    (* *)
    let node_moves n =
      NS.(Hashtbl.find move_list n ^^ (!active_moves ++ !worklist_moves))
    in
    (* *)
    let move_related n = not (node_moves n |> NS.is_empty) in
    (* *)
    let make_worklist () =
      NS.iter
        (fun n ->
          if degree_of n >= k then wl_spill := NS.add n !wl_spill
          else if move_related n then wl_freeze := NS.add n !wl_freeze
          else wl_simplify := NS.add n !wl_simplify )
        !initial;
      initial := NS.clear !initial
    in
    (* *)
    let adjacent n =
      let _, select_set = !select_stack in
      let adj = Hashtbl.find adj_list n in
      NS.(adj // (select_set ++ !coalesced_nodes))
    in
    (* *)
    let enable_moves =
      NS.iter (fun n ->
          NS.iter
            (fun m ->
              if NS.mem m !active_moves then (
                (* Transition this move instruction as potentially eligible for
                   coalescing, rather than being completely unprocessed as in
                   [active_moves]. *)
                active_moves := NS.remove m !active_moves;
                worklist_moves := NS.add m !worklist_moves ) )
            (node_moves n) )
    in
    (* *)
    let decrement_degree m =
      let d = degree_of m in
      Hashtbl.add degree m (d - 1);
      if d = k then (
        (* Just transitioned from degree k to k-1. This means we may be eligible
           for coalescing, and so may our neighbors. *)
        enable_moves NS.(of_list [m] ++ adjacent m);
        wl_spill := NS.remove m !wl_spill;
        if move_related m then wl_freeze := NS.add m !wl_freeze
        else wl_simplify := NS.add m !wl_simplify )
    in
    (* *)
    let simplify () =
      let n = NS.choose !wl_simplify in
      wl_simplify := NS.remove n !wl_simplify;
      select_stack_push n;
      NS.iter (fun m -> decrement_degree m) (adjacent n)
    in
    (* *)
    let rec get_alias n =
      match Hashtbl.find_opt alias n with
      | Some a ->
          assert (NS.mem n !coalesced_nodes);
          get_alias a
      | None ->
          assert (not (NS.mem n !coalesced_nodes));
          n
    in
    (* Briggs test for coalescability (page 232, [Conservative] on page 247). *)
    let briggs u v =
      let neighbors_uv = NS.(adjacent u ++ adjacent v) in
      let uv_sig_neighbors =
        NS.fold
          (fun n cnt -> if degree_of n >= k then cnt + 1 else cnt)
          neighbors_uv 0
      in
      uv_sig_neighbors < k
    in
    (* George test for coalescability (page 232, [OK] on page 247). *)
    let george u v =
      adjacent u
      |> NS.for_all (fun t ->
             degree_of t < k
             || NS.mem t precolored_nodes
             || Hashtbl.mem adj_set (t, v) )
    in
    (* *)
    let add_work_list u =
      if
        (not (NS.mem u precolored_nodes))
        && (not (move_related u))
        && degree_of u < k
      then (
        wl_freeze := NS.remove u !wl_freeze;
        wl_simplify := NS.add u !wl_simplify )
    in
    (* *)
    let combine u v =
      if NS.mem v !wl_freeze then wl_freeze := NS.remove v !wl_freeze
      else (
        assert (NS.mem v !wl_spill);
        wl_spill := NS.remove v !wl_spill );
      coalesced_nodes := NS.add v !coalesced_nodes;
      Hashtbl.add alias v u;
      Hashtbl.add move_list u
        NS.(Hashtbl.find move_list u ++ Hashtbl.find move_list v);
      NS.iter
        (fun t ->
          add_edge t u;
          decrement_degree t )
        (adjacent v);
      if degree_of u >= k && NS.mem u !wl_freeze then (
        wl_freeze := NS.remove u !wl_freeze;
        wl_spill := NS.add u !wl_spill )
    in
    (* *)
    let coalesce () =
      let m = NS.choose !worklist_moves in
      let x, y =
        match Hashtbl.find instr_of_flownode m with
        | Assem.Mov {dst = x; src = y; _} -> (x, y)
        | _ -> ice "instr in worklist_moves is not a move"
      in
      let x = get_alias (ifnode_of_temp x) in
      let y = get_alias (ifnode_of_temp y) in
      (* Coalescing with precolored nodes should keep the precolored node
         register as the "real" one. *)
      let u, v = if NS.mem y precolored_nodes then (y, x) else (x, y) in
      worklist_moves := NS.remove m !worklist_moves;
      if G.eq u v then (
        (* Trivial case - this is a redundant copy, u<-u. *)
        coalesced_moves := m :: !coalesced_moves;
        add_work_list u )
      else if NS.mem v precolored_nodes || Hashtbl.mem adj_set (u, v) then (
        (* There are two cases in which the src/dst of a move interfere, and
           hence we cannot coalesce the temporaries:
             1. The src is a precolored register. We have to keep that register
                that way, and cannot alias the precolored node as the dst.
             2. There is an explicit interference edge between src/dst. *)
        constrained_moves := m :: !constrained_moves;
        add_work_list u;
        add_work_list v )
      else if
        (NS.mem u precolored_nodes && george u v)
        || ((not (NS.mem u precolored_nodes)) && briggs u v)
      then (
        (* This move is properly coalescable. *)
        coalesced_moves := m :: !coalesced_moves;
        combine u v;
        add_work_list u )
      else
        (* We do not yet know if this move can be coalesced or not. Put it back
           into the pre-processing pile. *)
        active_moves := NS.add m !active_moves
    in
    (* *)
    let freeze_moves u =
      NS.iter
        (fun m ->
          let x, y =
            match Hashtbl.find instr_of_flownode m with
            | Assem.Mov {dst = x; src = y; _} ->
                (ifnode_of_temp x, ifnode_of_temp y)
            | _ -> ice "instr in worklist_moves is not a move"
          in
          active_moves := NS.remove m !active_moves;
          frozen_moves := m :: !frozen_moves;
          (* Find the other temp [v] involved in this move with [u]. If [v] is
             longer involved in any moves that are available for potential
             coalescing, and it is of insignificant degree, we can mark it for
             simplification. *)
          let v =
            if get_alias y = get_alias u then get_alias x else get_alias y
          in
          if NS.is_empty (node_moves v) && degree_of v < k then (
            wl_freeze := NS.remove v !wl_freeze;
            wl_simplify := NS.add v !wl_simplify ) )
        (node_moves u)
    in
    (* *)
    let freeze () =
      let u = NS.choose !wl_freeze in
      wl_freeze := NS.remove u !wl_freeze;
      wl_simplify := NS.add u !wl_simplify;
      freeze_moves u
    in
    (* *)
    let select_spill () =
      let m, _ =
        NS.fold
          (fun u v ->
            let ucost = spill_cost u in
            match v with
            | None -> Some (u, ucost)
            | Some (_, vcost) when ucost < vcost -> Some (u, ucost)
            | Some (v, vcost) -> Some (v, vcost) )
          !wl_spill None
        |> Option.get
      in
      wl_spill := NS.remove m !wl_spill;
      wl_simplify := NS.add m !wl_simplify;
      freeze_moves m
    in
    (* *)
    let assign_colors () =
      let rec iter = function
        | [] -> ()
        | n :: rest ->
            ((* NB: this should only be registers that we are actually permitted
                to use. Prior passes of the compiler should have made sure that
                registers that are always reserved always force an interference,
                and so we will we find during assignment that those registers
                are not available for any uncolored temporary.
                TODO: we still may up end up using registers that we shouldn't
                (or is a bad idea to), for example if we spill a node and have
                to use [rsp] for a short interval. Resolve this by making the
                cost for this very high or eliminating its ability to be used
                entirely. *)
             let ok_colors =
               NS.fold
                 (fun w colors ->
                   let w = get_alias w in
                   if NS.(mem w (!colored_nodes ++ precolored_nodes)) then
                     F.RegisterSet.remove (color_of (temp_of_ifnode w)) colors
                   else colors )
                 (adjacent n)
                 (F.RegisterSet.of_list F.registers)
             in
             match F.RegisterSet.choose_opt ok_colors with
             | None -> spilled_nodes := temp_of_ifnode n :: !spilled_nodes
             | Some c ->
                 colored_nodes := NS.add n !colored_nodes;
                 Hashtbl.add color (temp_of_ifnode n) c );
            iter rest
      in
      iter (fst !select_stack);
      NS.iter
        (fun n ->
          let a = temp_of_ifnode (get_alias n) in
          let n = temp_of_ifnode n in
          Hashtbl.add color n (Hashtbl.find color a) )
        !coalesced_nodes
    in
    (*************)
    (* Main loop *)
    (*************)
    build ();
    assert_build_invariants ();
    make_worklist ();
    let rec solve () =
      if
        not
          NS.(is_empty !wl_simplify &&
              is_empty !worklist_moves &&
              is_empty !wl_freeze &&
              is_empty !wl_spill) [@ocamlformat "disable"]
      then (
        NS.(
          if not (is_empty !wl_simplify) then simplify ()
          else if not (is_empty !worklist_moves) then coalesce ()
          else if not (is_empty !wl_freeze) then freeze ()
          else if not (is_empty !wl_spill) then select_spill ();
          solve ()) )
    in
    solve ();
    assign_colors ();
    (color, !spilled_nodes)

  let rewrite_spills (spills : temp list) (instrs : Assem.instr list)
      (fr : F.frame) : Assem.instr list =
    let open Assem in
    let mem_of_temp =
      List.map
        (fun v ->
          let vmem = F.alloc_local fr (* escapes: put on stack *) true in
          (v, vmem) )
        spills
    in
    let gen_instr gen =
      List.fold_left
        (fun fetches t ->
          match List.assoc_opt t mem_of_temp with
          | Some mem -> gen t mem @ fetches
          | None -> fetches )
        []
    in
    let mov dst src = Ir.Mov (dst, src) |> F.codegen fr in
    let me mem = F.expr_of_access (mem, Ir.Temp F.fp) in
    let te t = Ir.Temp t in
    let fetch src = gen_instr (fun t mem -> mov (te t) (me mem)) src in
    let store src = gen_instr (fun t mem -> mov (me mem) (te t)) src in
    let rec iter = function
      | [] -> []
      | Oper {assem; dst; src; jmp} :: rest ->
          fetch src @ [Oper {assem; dst; src; jmp}] @ store dst @ iter rest
      | Mov {assem; dst; src} :: rest ->
          fetch [src] @ [Mov {assem; dst; src}] @ store [dst] @ iter rest
      | (Label _ as lab) :: rest -> lab :: iter rest
    in
    iter instrs

  let rec reg_alloc fr instrs =
    (* TODO: real spill cost *)
    let select_spill _ = 1 in
    let coloring, spills = color instrs select_spill in
    match spills with
    | [] -> (instrs, coloring)
    | _ -> reg_alloc fr (rewrite_spills spills instrs fr)
end
