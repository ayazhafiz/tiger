(** Tests [Tiger.Flow] and [Tiger.Live]. *)

open Tiger.Assem
open Tiger.Flow
open Tiger.Live
open Tiger.Temp
module DG = Tiger.Graph.DirectedGraph
module UDG = Tiger.Graph.UndirectedGraph

let pair l1 l2 = List.map2 (fun k v -> (k, v)) l1 l2
let revpair l = List.map (fun (k, v) -> (v, k)) l

let assem_of_instr = function
  | Oper {assem; _} | Label {assem; _} | Mov {assem; _} -> assem

let temp_of_var = ["a"; "b"; "c"; "d"] |> List.map (fun v -> (v, newtemp ()))

let var_of_temp temp =
  let map = revpair temp_of_var in
  List.assoc temp map

let a = List.assoc "a" temp_of_var
let b = List.assoc "b" temp_of_var
let c = List.assoc "c" temp_of_var
let d = List.assoc "d" temp_of_var

let string_of_temp = function
  | t when t = a -> "a"
  | t when t = b -> "b"
  | t when t = c -> "c"
  | t when t = d -> "d"
  | _ -> failwith "bad temp"

let l1 = strlabel "L1"
let l2 = strlabel "L2"

(* Graph 10.1, 10.2 with some amendment.

   1    a:=0
         ↓
L1:      ↓    <---\
   2   b:=a+1     |
         ↓        |
   3   c:=c+b     |
         ↓        |
   4    d:=c      |
         ↓        |
   5   a:=b*2     |
         ↓        |
   6    a<N ------/
         ↓
L2:      ↓
   7  return d
 *)
let program =
  [ Oper {assem = "I1 a:=0"; dst = []; src = []; jmp = None}
  ; Label {assem = "L1"; lab = l1}
  ; Oper {assem = "I2 b:=a+1"; dst = [b]; src = [a]; jmp = None}
  ; Oper {assem = "I3 c:=c+b"; dst = [c]; src = [b; c]; jmp = None}
  ; Mov {assem = "I4 d:=c"; dst = d; src = c}
  ; Oper {assem = "I5 a:=b*2"; dst = [a]; src = [b]; jmp = None}
  ; Oper {assem = "I6 a<N"; dst = []; src = [a]; jmp = Some [l1; l2]}
  ; Label {assem = "L2"; lab = l2}
  ; Oper {assem = "I7 return d"; dst = []; src = [d]; jmp = None} ]

let program_as_str =
  List.map assem_of_instr program |> List.map (fun s -> String.sub s 0 2)

let check = Alcotest.(check (list string))
let mktest name what = (name, `Quick, what)

let flow_tests =
  let _, nodes = flowgraph_of_instrs program in
  let i_of_node = pair nodes program_as_str in
  let node_of_i = revpair i_of_node in
  let succ_of instr =
    let node = List.assoc instr node_of_i in
    DG.succ node |> List.map (fun n -> List.assoc n i_of_node)
  in
  let cases =
    [
    ("I1", ["L1"]);
    ("L1", ["I2"]);
    ("I2", ["I3"]);
    ("I3", ["I4"]);
    ("I4", ["I5"]);
    ("I5", ["I6"]);
    ("I6", ["L1"; "L2"]);
    ("L2", ["I7"]);
    ("I7", []);
    ] [@ocamlformat "disable"]
  in
  List.map
    (fun (i, succ) ->
      let test _ = check i succ (succ_of i) in
      mktest ("succ: " ^ i) test)
    cases

let live_tests =
  let g, nodes = flowgraph_of_instrs program in
  let i_of_node = pair nodes program_as_str in
  let node_of_i = revpair i_of_node in
  let ifgraph, liveout_of_node = interference_graph g in
  let liveout_of instr =
    let node = List.assoc instr node_of_i in
    liveout_of_node node |> TempSet.to_seq |> List.of_seq
    |> List.map string_of_temp
  in
  (* Liveout of instrs *)
  let liveout_tests =
    let cases =
      [
      ("I1", [a; c]);
      ("L1", [a; c]);
      ("I2", [b; c]);
      ("I3", [b; c]);
      ("I4", [b; c; d]);
      ("I5", [a; c; d]);
      ("I6", [a; c; d]);
      ("L2", [d]);
      ("I7", []);
      ] [@ocamlformat "disable"]
    in
    List.map
      (fun (i, liveout) ->
        let expc = List.map string_of_temp liveout in
        let test _ = check i expc (liveout_of i) in
        mktest ("liveout: " ^ i) test)
      cases
  in
  let ifs_of_temp temp =
    let node = ifgraph.ifnode_of_temp temp in
    UDG.adj node |> List.map ifgraph.temp_of_ifnode |> List.map string_of_temp
  in
  (* Interferences *)
  let ifs_tests =
    let cases =
      [
      (a, [c; d]);
      (b, [c; d]);
      (c, [a; b]);
      (d, [a; b]);
      ] [@ocamlformat "disable"]
    in
    List.map
      (fun (t, ifs) ->
        let t' = string_of_temp t in
        let expc = List.map string_of_temp ifs in
        let real = ifs_of_temp t in
        let test _ = check t' expc real in
        mktest ("interferences: " ^ t') test)
      cases
  in
  (* Moves *)
  let moves =
    List.map
      (fun (i1, i2) ->
        let get t = ifgraph.temp_of_ifnode t |> string_of_temp in
        (get i1, get i2))
      ifgraph.moves
  in
  let moves_test =
    let test _ =
      Alcotest.(check (list (pair string string))) "moves" moves [("d", "c")]
    in
    mktest "moves" test
  in
  liveout_tests @ ifs_tests @ [moves_test]

let () =
  Alcotest.run "liveness tests"
    [("flow tests", flow_tests); ("live tests", live_tests)]
