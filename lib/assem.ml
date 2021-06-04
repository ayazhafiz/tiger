open Temp

type instr =
  | Oper of
      { assem : string
      ; dst : temp list
      ; src : temp list
      ; jmp : label list option
      ; mutable comments : string list }
  | Label of {assem : string; lab : label; mutable comments : string list}
  | Mov of
      {assem : string; dst : temp; src : temp; mutable comments : string list}

let add_comment cmt = function
  | Oper i -> i.comments <- cmt :: i.comments
  | Label i -> i.comments <- cmt :: i.comments
  | Mov i -> i.comments <- cmt :: i.comments

let string_of_instr ?(string_of_temp = Temp.string_of_temp) instr =
  let fmt_tlist lst = List.map string_of_temp lst |> String.concat ";" in
  let fmt_llist lst = List.map string_of_label lst |> String.concat ";" in
  let fmt_comments lst = String.concat "\t" lst in
  match instr with
  | Oper {assem; dst; src; jmp; comments} ->
      Printf.sprintf "%s (dst=%s, src=%s, jmp=%s)\t\t%s" assem (fmt_tlist dst)
        (fmt_tlist src)
        (Option.value jmp ~default:[] |> fmt_llist)
        (fmt_comments comments)
  | Label {assem; lab; comments} ->
      Printf.sprintf "%s (lab=%s)\t\t%s" assem (string_of_label lab)
        (fmt_comments comments)
  | Mov {assem; dst; src; comments} ->
      Printf.sprintf "%s (dst=%s, src=%s)\t\t%s" assem (string_of_temp dst)
        (string_of_temp src) (fmt_comments comments)

let rec uniq = function
  | [] -> []
  | a :: rest when List.mem a rest -> uniq rest
  | a :: rest -> a :: uniq rest

let fmt_instr1 string_of_temp comment_prefix eliminate_moves instr =
  let explode s =
    let rec expl i l = if i < 0 then l else expl (i - 1) (s.[i] :: l) in
    expl (String.length s - 1) []
  in
  let fmt assem dst src jmp comments =
    let pick lst n = List.nth lst (String.make 1 n |> int_of_string) in
    let rec walk = function
      | [] -> ""
      | '`' :: 'd' :: n :: rst -> pick dst n ^ walk rst
      | '`' :: 's' :: n :: rst -> pick src n ^ walk rst
      | '`' :: 'j' :: n :: rst -> pick jmp n ^ walk rst
      | c :: rst -> String.make 1 c ^ walk rst
    in
    let base = walk (explode assem) in
    let comments =
      uniq comments |> String.concat "\n" |> Print.lines
      |> List.map (fun cmt ->
             if String.length (String.trim cmt) <> 0 then
               Printf.sprintf "%s %s" comment_prefix cmt
             else "" )
      |> String.concat "\n"
    in
    Print.annotate comments base
  in
  let assem, dst, src, jmp, comments =
    match instr with
    | Oper {assem; dst; src; jmp; comments} ->
        (assem, dst, src, Option.value jmp ~default:[], comments)
    | Label {assem; comments; _} -> (assem, [], [], [], comments)
    | Mov {assem; dst; src; comments} -> (assem, [dst], [src], [], comments)
  in
  let dst = List.map string_of_temp dst in
  let src = List.map string_of_temp src in
  match (instr, assem, dst, src) with
  | Mov _, _, dst, src when eliminate_moves && List.equal ( = ) dst src -> None
  | _, assem, _, _ when assem = "" -> None
  | _ ->
      fmt assem dst src (List.map string_of_label jmp) comments |> Option.some

let fmt_instrs string_of_temp comment_prefix eliminate_moves instrs =
  let do1 instr =
    try fmt_instr1 string_of_temp comment_prefix eliminate_moves instr
    with _ ->
      failwith
        ( "ICE (string_of_instr): "
        ^ string_of_instr ~string_of_temp instr
        ^ " malformed" )
  in
  let fmted = List.filter_map do1 instrs in
  String.concat "\n" fmted |> Print.prettify
