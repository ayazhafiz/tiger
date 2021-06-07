(* vim: foldmethod=marker
 *)
open Frame
open Temp
module LL = Linked_list
module G = Graph.Graph
module UDG = Graph.UndirectedGraph
module NS = G.NodeSet

let ice why = failwith ("ICE (reg_alloc): " ^ why)

module MoveSet = Set.Make (struct
  type t = temp * temp

  let compare = compare
end)

module TempPairSet = MoveSet

module TempMap = Map.Make (struct
  type t = temp

  let compare = compare
end)

module RegisterAllocation (F : ALLOCATION_FRAME) = struct
  (*******************************)
  (* Interference Graph Coloring *)
  (*******************************)
  let color (instrs : Assem.instr list) (_spill_cost : Live.ifnode -> int) :
      F.allocation * temp list =
    (* Number of registers. *)
    let k = List.length F.registers in
    (* Registers that are pre-assigned a color. *)
    let precolored = F.temp_map |> List.map fst |> TempSet.of_list in
    let move_list : MoveSet.t TempMap.t ref = ref TempMap.empty in
    let worklist_moves : MoveSet.t ref = ref MoveSet.empty in
    let active_moves : MoveSet.t ref = ref MoveSet.empty in
    let coalesced_moves : MoveSet.t ref = ref MoveSet.empty in
    let constrained_moves : MoveSet.t ref = ref MoveSet.empty in
    let frozen_moves : MoveSet.t ref = ref MoveSet.empty in
    let initial : TempSet.t ref = ref TempSet.empty in
    let adj_set : TempPairSet.t ref = ref TempPairSet.empty in
    let adj_list : TempSet.t TempMap.t ref = ref TempMap.empty in
    let degree : int TempMap.t ref = ref TempMap.empty in
    let _find_or_assert_precolored tbl n default =
      match Hashtbl.find_opt tbl n with
      | Some v -> v
      | None ->
          assert (TempSet.mem n precolored);
          default
    in
    let spill_worklist : TempSet.t ref = ref TempSet.empty in
    let freeze_worklist : TempSet.t ref = ref TempSet.empty in
    let simplify_worklist : TempSet.t ref = ref TempSet.empty in
    let select_stack : temp list ref = ref [] in
    let select_stack_set : TempSet.t ref = ref TempSet.empty in
    let coalesced_nodes : TempSet.t ref = ref TempSet.empty in
    let alias : temp TempMap.t ref = ref TempMap.empty in
    let spilled_nodes = ref [] in
    let colored_nodes : TempSet.t ref = ref TempSet.empty in
    let color : F.register TempMap.t ref = ref TempMap.empty in
    (* Start *)
    let get_degree n = TempMap.find_opt n !degree |> Option.value ~default:0 in
    let get_adj n =
      TempMap.find_opt n !adj_list |> Option.value ~default:TempSet.empty
    in
    let init () =
      TempSet.iter
        (fun n -> color := TempMap.add n (List.assoc n F.temp_map) !color)
        precolored
    in
    let add_edge u v =
      if (not (TempPairSet.mem (u, v) !adj_set)) && not (Temp.tempeq u v) then (
        adj_set := TempPairSet.add (u, v) !adj_set;
        adj_set := TempPairSet.add (v, u) !adj_set;
        if not (TempSet.mem u precolored) then (
          adj_list :=
            TempMap.add u
              TempSet.(union (get_adj u) (TempSet.singleton v))
              !adj_list;
          degree := TempMap.add u (get_degree u + 1) !degree );
        if not (TempSet.mem v precolored) then (
          adj_list :=
            TempMap.add v
              TempSet.(union (get_adj v) (TempSet.singleton u))
              !adj_list;
          degree := TempMap.add v (get_degree v + 1) !degree ) )
    in
    let build () =
      let fg, fgnodes = Flow.flowgraph_of_instrs instrs in
      let _, liveout = Live.interference_graph fg in
      let buildnode (n : Flow.flownode) =
        let live_n = liveout n in
        let use_n = (G.get_data n).uses in
        let def_n = (G.get_data n).defs in
        if (G.get_data n).is_mov then (
          (* Since this is a move, there should be exactly one in each set. *)
          assert (List.length def_n = 1);
          assert (List.length use_n = 1);
          let mv = (List.hd def_n, List.hd use_n) in
          List.iter
            (fun t ->
              let moves =
                TempMap.find_opt t !move_list
                |> Option.value ~default:MoveSet.empty
              in
              assert (not (MoveSet.mem mv moves));
              let moves = MoveSet.add mv moves in
              move_list := TempMap.add t moves !move_list )
            (def_n @ use_n);
          assert (not (MoveSet.mem mv !worklist_moves));
          worklist_moves := MoveSet.add mv !worklist_moves )
        else ();
        List.iter (fun t -> initial := TempSet.add t !initial) (def_n @ use_n);
        List.iter (fun d -> TempSet.iter (fun l -> add_edge l d) live_n) def_n
      in
      List.iter buildnode fgnodes;
      TempSet.iter (fun r -> initial := TempSet.remove r !initial) precolored
    in
    let node_moves (n : temp) =
      MoveSet.inter
        (TempMap.find_opt n !move_list |> Option.value ~default:MoveSet.empty)
        (MoveSet.union !active_moves !worklist_moves)
    in
    let move_related n = not (MoveSet.is_empty (node_moves n)) in
    let make_worklist () =
      TempSet.iter
        (fun (n : temp) ->
          initial := TempSet.remove n !initial;
          if get_degree n >= k then (
            assert (not (TempSet.mem n !spill_worklist));
            spill_worklist := TempSet.add n !spill_worklist )
          else if move_related n then (
            assert (not (TempSet.mem n !freeze_worklist));
            freeze_worklist := TempSet.add n !freeze_worklist )
          else (
            assert (not (TempSet.mem n !simplify_worklist));
            simplify_worklist := TempSet.add n !simplify_worklist ) )
        !initial
    in
    let adjacent (n : temp) =
      TempSet.diff (get_adj n)
        (TempSet.union !select_stack_set !coalesced_nodes)
    in
    let enable_moves (s : TempSet.t) =
      TempSet.iter
        (fun n ->
          MoveSet.iter
            (fun m ->
              if MoveSet.mem m !active_moves then (
                active_moves := MoveSet.remove m !active_moves;
                worklist_moves := MoveSet.add m !worklist_moves ) )
            (node_moves n) )
        s
    in
    let decrement_degree (n : temp) =
      let d = get_degree n in
      degree := TempMap.add n (d - 1) !degree;
      if d = k then (
        enable_moves (TempSet.union (adjacent n) (TempSet.singleton n));
        spill_worklist := TempSet.remove n !spill_worklist;
        if move_related n then
          (* assert (not (TempSet.mem freeze_worklist n)); *)
          freeze_worklist := TempSet.add n !freeze_worklist
        else (
          assert (not (TempSet.mem n !simplify_worklist));
          simplify_worklist := TempSet.add n !simplify_worklist ) )
    in
    let simplify () =
      let n = TempSet.choose !simplify_worklist in
      simplify_worklist := TempSet.remove n !simplify_worklist;
      (* assert (not (List.mem n !select_stack));
         assert (not (TempSet.mem n !select_stack_set)); *)
      if not (List.mem n !select_stack) then (
        select_stack := n :: !select_stack;
        select_stack_set := TempSet.add n !select_stack_set );
      TempSet.iter decrement_degree (adjacent n)
    in
    let rec get_alias (n : temp) =
      if TempSet.mem n !coalesced_nodes then get_alias (TempMap.find n !alias)
      else n
    in
    let add_work_list (n : temp) =
      if
        (not (TempSet.mem n precolored))
        && (not (move_related n))
        && get_degree n < k
      then (
        freeze_worklist := TempSet.remove n !freeze_worklist;
        assert (not (TempSet.mem n !simplify_worklist));
        simplify_worklist := TempSet.add n !simplify_worklist )
    in
    let briggs (n : temp) (m : temp) =
      let s = TempSet.union (adjacent n) (adjacent m) in
      let j =
        TempSet.fold (fun n i -> if get_degree n >= k then i + 1 else i) s 0
      in
      j < k
    in
    let george (n : temp) (m : temp) =
      let s = adjacent m in
      let ok n m =
        get_degree n < k
        || TempSet.mem n precolored
        || TempPairSet.mem (n, m) !adj_set
      in
      TempSet.fold (fun t p -> p && ok t n) s true
    in
    let combine (u : temp) (v : temp) =
      if TempSet.mem v !freeze_worklist then
        freeze_worklist := TempSet.remove v !freeze_worklist
      else
        (* assert (Hashtbl.mem spill_worklist v); *)
        spill_worklist := TempSet.remove v !spill_worklist;
      coalesced_nodes := TempSet.add v !coalesced_nodes;
      assert (not (TempMap.mem v !alias));
      alias := TempMap.add v u !alias;
      move_list :=
        TempMap.add u
          (MoveSet.union
             (TempMap.find u !move_list)
             (TempMap.find v !move_list) )
          !move_list;
      enable_moves (TempSet.singleton v);
      TempSet.iter
        (fun t ->
          add_edge t u;
          decrement_degree t )
        (adjacent v);
      if get_degree u >= k && TempSet.mem u !freeze_worklist then (
        freeze_worklist := TempSet.remove u !freeze_worklist;
        assert (not (TempSet.mem u !spill_worklist));
        spill_worklist := TempSet.add u !spill_worklist )
    in
    let coalesce () =
      let m = MoveSet.choose !worklist_moves in
      let p, q = m in
      let x = get_alias p in
      let y = get_alias q in
      let u, v = if TempSet.mem y precolored then (y, x) else (x, y) in
      worklist_moves := MoveSet.remove m !worklist_moves;
      if tempeq u v then (
        (* Case 1: already coalesced *)
        assert (not (MoveSet.mem m !coalesced_moves));
        coalesced_moves := MoveSet.add m !coalesced_moves;
        add_work_list u )
      else if TempSet.mem v precolored || TempPairSet.mem (u, v) !adj_set then (
        (* Case 2: impossible to be coalesced - both precolored or constrained *)
        assert (not (MoveSet.mem m !constrained_moves));
        constrained_moves := MoveSet.add m !constrained_moves;
        add_work_list u;
        add_work_list v )
      else if
        (TempSet.mem u precolored && george u v)
        || ((not (TempSet.mem u precolored)) && briggs u v)
      then (
        (* Case 3: coalesce! - George test if u is precolored, Briggs otherwise *)
        assert (not (MoveSet.mem m !coalesced_moves));
        coalesced_moves := MoveSet.add m !coalesced_moves;
        combine u v;
        add_work_list u )
      else (
        (* Case 4: not coalesceables now *)
        assert (not (MoveSet.mem m !active_moves));
        active_moves := MoveSet.add m !active_moves )
    in
    let freeze_moves (u : temp) =
      let node_moves_u = node_moves u in
      let freeze_move m =
        let x, y = m in
        let v =
          if get_alias y = get_alias u then get_alias x else get_alias y
        in
        active_moves := MoveSet.remove m !active_moves;
        frozen_moves := MoveSet.add m !frozen_moves;
        if MoveSet.is_empty (node_moves v) && get_degree v < k then (
          freeze_worklist := TempSet.remove v !freeze_worklist;
          assert (not (TempSet.mem v !simplify_worklist));
          simplify_worklist := TempSet.add v !simplify_worklist )
      in
      MoveSet.iter freeze_move node_moves_u
    in
    let freeze () =
      let u = TempSet.choose !freeze_worklist in
      freeze_worklist := TempSet.remove u !freeze_worklist;
      simplify_worklist := TempSet.add u !simplify_worklist;
      freeze_moves u
    in
    let select_spill () =
      let n = TempSet.choose !spill_worklist in
      spill_worklist := TempSet.remove n !spill_worklist;
      simplify_worklist := TempSet.add n !simplify_worklist;
      freeze_moves n
    in
    let assign_colors () =
      (* only color when the other node is not being spilled! *)
      let color_coalesced n =
        if not (List.mem (get_alias n) !spilled_nodes) then
          (* assert (not (TempMap.mem n !color)); *)
          color := TempMap.add n (TempMap.find (get_alias n) !color) !color
      in
      while not (List.length !select_stack = 0) do
        let n = List.hd !select_stack in
        let adj_n = get_adj n in
        let ok_colors = F.registers |> F.RegisterSet.of_list in
        select_stack := List.tl !select_stack;
        let ok_colors =
          TempSet.fold
            (fun w ok_colors ->
              if
                TempSet.mem (get_alias w)
                  (TempSet.union !colored_nodes precolored)
              then
                F.RegisterSet.remove
                  (TempMap.find (get_alias w) !color)
                  ok_colors
              else ok_colors )
            adj_n ok_colors
        in
        if F.RegisterSet.is_empty ok_colors then (
          assert (not (List.mem n !spilled_nodes));
          spilled_nodes := n :: !spilled_nodes )
        else (
          assert (not (TempSet.mem n !colored_nodes));
          colored_nodes := TempSet.add n !colored_nodes;
          (* assert (not (TempMap.mem n !color)); *)
          color := TempMap.add n (F.RegisterSet.choose ok_colors) !color )
      done;
      TempSet.iter color_coalesced !coalesced_nodes
    in
    (*************)
    (* Main loop *)
    (*************)
    init ();
    build ();
    make_worklist ();
    while
      (not (TempSet.is_empty !simplify_worklist))
      || (not (MoveSet.is_empty !worklist_moves))
      || (not (TempSet.is_empty !freeze_worklist))
      || not (TempSet.is_empty !spill_worklist)
    do
      if not (TempSet.is_empty !simplify_worklist) then simplify ()
      else if not (MoveSet.is_empty !worklist_moves) then coalesce ()
      else if not (TempSet.is_empty !freeze_worklist) then freeze ()
      else if not (TempSet.is_empty !spill_worklist) then select_spill ()
      else ice "nothing to do"
    done;
    assign_colors ();
    (TempMap.to_seq !color |> Hashtbl.of_seq, !spilled_nodes)

  let rewrite_spills (spills : temp list) (instrs : Assem.instr list)
      (fr : F.frame) : Assem.instr list =
    let open Assem in
    (* Allocate memory locations for each v e spilledNodes *)
    let mem_of_spill =
      let map =
        List.map
          (fun v ->
            let vmem = F.alloc_local fr None (* put on stack *) true in
            (v, vmem) )
          spills
      in
      fun spilltemp -> List.assoc spilltemp map
    in
    let fetch spilled_temp newtemp =
      let cmt = "fetch spilled " ^ string_of_temp spilled_temp in
      F.fetch_from_access fr newtemp (mem_of_spill spilled_temp) cmt
    in
    let store spilled_temp newtemp =
      let cmt = "store spilled " ^ string_of_temp spilled_temp in
      F.store_to_access fr (mem_of_spill spilled_temp) newtemp cmt
    in
    let process dsts srcs =
      let dstsrc = TempSet.(union (of_list dsts) (of_list srcs)) in
      (* Create a new temporary v_i for each definition and each use. *)
      let newtemps =
        TempSet.to_seq dstsrc |> List.of_seq
        |> List.map (fun t ->
               if List.mem t spills then (t, newtemp ()) else (t, t) )
      in
      let handle_spill handler spilled_temp =
        match List.mem spilled_temp spills with
        | false -> []
        | true ->
            let newtemp = List.assoc spilled_temp newtemps in
            handler spilled_temp newtemp
      in
      let newdsts = List.map (fun s -> List.assoc s newtemps) dsts in
      let newsrcs = List.map (fun s -> List.assoc s newtemps) srcs in
      (* Insert a store after each definition of a v_i. *)
      let store_spilled_dst = List.concat_map (handle_spill store) dsts in
      (* Insert a fetch before each use of a v_i. *)
      let fetch_spilled_src = List.concat_map (handle_spill fetch) srcs in
      (newdsts, store_spilled_dst, newsrcs, fetch_spilled_src)
    in
    (* Insert fetch/stores for all instructions. *)
    let transform_instr = function
      | Oper ({dst; src; _} as oper) ->
          let newdsts, store_spilled_dst, newsrcs, fetch_spilled_src =
            process dst src
          in
          let new_oper = Oper {oper with dst = newdsts; src = newsrcs} in
          fetch_spilled_src @ [new_oper] @ store_spilled_dst
      | Mov ({dst; src; _} as mov) ->
          let newdsts, store_spilled_dst, newsrcs, fetch_spilled_src =
            process [dst] [src]
          in
          let newsrc, newdst =
            match (newsrcs, newdsts) with
            | [s], [d] -> (s, d)
            | _ -> ice "impossible"
          in
          let new_mov = Mov {mov with dst = newdst; src = newsrc} in
          fetch_spilled_src @ [new_mov] @ store_spilled_dst
      | Label _ as lab -> [lab]
    in
    List.concat_map transform_instr instrs

  let rec reg_alloc1 fr instrs n =
    (* TODO: real spill cost *)
    let select_spill _ = 1 in
    let coloring, spills = color instrs select_spill in
    match spills with
    | [] -> (instrs, coloring)
    | _ -> reg_alloc1 fr (rewrite_spills spills instrs fr) (n + 1)

  let reg_alloc fr instrs = reg_alloc1 fr instrs 1
end
