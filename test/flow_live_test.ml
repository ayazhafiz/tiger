(** Tests [Tiger.Flow] and [Tiger.Live]. *)

open Tiger.Assem
open Tiger.Flow
open Tiger.Frame
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
  [ Oper {assem = "I1 a:=0"; dst = []; src = []; jmp = None; comments = []}
  ; Label {assem = "L1"; lab = l1; comments = []}
  ; Oper {assem = "I2 b:=a+1"; dst = [b]; src = [a]; jmp = None; comments = []}
  ; Oper
      {assem = "I3 c:=c+b"; dst = [c]; src = [b; c]; jmp = None; comments = []}
  ; Mov {assem = "I4 d:=c"; dst = d; src = c; comments = []}
  ; Oper {assem = "I5 a:=b*2"; dst = [a]; src = [b]; jmp = None; comments = []}
  ; Oper
      {assem = "I6 a<N"; dst = []; src = [a]; jmp = Some [l1; l2]; comments = []}
  ; Label {assem = "L2"; lab = l2; comments = []}
  ; Oper {assem = "I7 return d"; dst = []; src = [d]; jmp = None; comments = []}
  ]

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
      mktest ("succ: " ^ i) test )
    cases

let mk_if_test cases ifgraph string_of_temp =
  let ifs_of_temp temp =
    let node = ifgraph.ifnode_of_temp temp in
    UDG.adj node |> List.map ifgraph.temp_of_ifnode |> List.map string_of_temp
  in
  List.map
    (fun (t, ifs) ->
      let t' = string_of_temp t in
      let expc = List.map string_of_temp ifs in
      let real = ifs_of_temp t in
      let test () = check t' expc real in
      mktest ("interferences: " ^ t') test )
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
        mktest ("liveout: " ^ i) test )
      cases
  in
  let ifs_cases =
    [
    (a, [c; d]);
    (b, [c; d]);
    (c, [a; b]);
    (d, [a; b]);
    ] [@ocamlformat "disable"]
  in
  let ifs_tests = mk_if_test ifs_cases ifgraph string_of_temp in
  (* Moves *)
  let moves =
    List.map
      (fun (i1, i2) ->
        let get t = ifgraph.temp_of_ifnode t |> string_of_temp in
        (get i1, get i2) )
      ifgraph.moves
  in
  let moves_test =
    let test _ =
      Alcotest.(check (list (pair string string))) "moves" moves [("d", "c")]
    in
    mktest "moves" test
  in
  liveout_tests @ ifs_tests @ [moves_test]

(****************************)
(* REGISTER ALLOCATION TEST *)
(****************************)
(* Example from page 237-241 *)

let r1 = newtemp ()
let r2 = newtemp ()
let r3 = newtemp ()

module TestMachine : ALLOCATION_FRAME = struct
  type register = string

  let string_of_register r = r

  module RegisterSet = Set.Make (struct
    type t = register

    let compare = compare
  end)

  type allocation = (temp, register) Hashtbl.t

  let registers = ["r1"; "r2"; "r3"]
  let t_of_r = [("r1", r1); ("r2", r2); ("r3", r3)]
  let temp_map = List.map (fun (r, t) -> (t, r)) t_of_r

  type frame = unit
  type access = unit

  let alloc_local _ _ _ = ()

  let fetch_from_access _ _ _ _ =
    [Oper {assem = "FETCH"; dst = []; src = []; jmp = None; comments = []}]

  let store_to_access _ _ _ _ =
    [Oper {assem = "FETCH"; dst = []; src = []; jmp = None; comments = []}]
end

let a = newtemp ()
let b = newtemp ()
let c = newtemp ()
let d = newtemp ()
let e = newtemp ()
let loop = newlabel "loop"

let string_of_temp = function
  | t when t = r1 -> "r1"
  | t when t = r2 -> "r2"
  | t when t = r3 -> "r3"
  | t when t = a -> "a"
  | t when t = b -> "b"
  | t when t = c -> "c"
  | t when t = d -> "d"
  | t when t = e -> "e"
  | _ -> failwith "bad temp"

let program =
  [ Mov {assem = "c<-r3"; dst = c; src = r3; comments = []}
  ; Mov {assem = "a<-r1"; dst = a; src = r1; comments = []}
  ; Mov {assem = "b<-r2"; dst = b; src = r2; comments = []}
  ; Oper {assem = "d<-0"; dst = [d]; src = []; jmp = None; comments = []}
  ; Mov {assem = "e<-a"; dst = e; src = a; comments = []}
  ; Label {assem = "loop"; lab = loop; comments = []}
  ; Oper {assem = "d<-d+b"; dst = [d]; src = [d; b]; jmp = None; comments = []}
  ; Oper {assem = "e<-e-1"; dst = [e]; src = [e]; jmp = None; comments = []}
  ; Oper
      { assem = "if e>0 goto loop"
      ; dst = []
      ; src = [e]
      ; jmp = Some [loop]
      ; comments = [] }; (* *)
    Mov {assem = "r1<-d"; dst = r1; src = d; comments = []}
  ; Mov {assem = "r3<-c"; dst = r3; src = c; comments = []}
  ; Oper {assem = "return"; dst = []; src = [r1; r3]; jmp = None; comments = []}
  ]

let reg_alloc_tests =
  let flowgraph, _ = flowgraph_of_instrs program in
  let ifgraph, _ = interference_graph flowgraph in
  let if_cases =
    [
    (r1, [r2; r3]);
    (r2, [r1; r3; a; c]);
    (r3, [r1; r3; a; c]);
    (a,  [r2; c;  d]);
    (b,  [a;  c;  d; e]);
    (c,  [r2; a;  b; d; e]);
    (d,  [a;  b;  c; e]);
    (e,  [b;  c;  d]);
    ] [@ocamlformat "disable"]
  in
  let if_tests = mk_if_test if_cases ifgraph string_of_temp in
  if_tests

let () =
  Alcotest.run "liveness tests"
    [ ("flow tests", flow_tests); ("live tests", live_tests)
    ; ("register allocation tests", reg_alloc_tests) ]
