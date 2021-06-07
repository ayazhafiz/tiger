let lines = String.split_on_char '\n'

let reflow indent what =
  let pre = String.make indent ' ' in
  String.split_on_char '\n' what |> List.map (( ^ ) pre) |> String.concat "\n"

let reflown1 indent what =
  match lines what with
  | [] -> ""
  | [f] -> f
  | f :: rst -> f ^ "\n" ^ reflow indent (String.concat "\n" rst)

let cmt_space = "  "
let ncmt_space = String.length cmt_space

let rstrip_none lst =
  let rec walk = function
    | [] -> []
    | None :: rst -> walk rst
    | Some _ :: _ as l -> l
  in
  List.rev (walk (List.rev lst))

let rec merge_cmts = function
  | [], [] -> []
  | None :: rest_old, cmt :: rest_new -> cmt :: merge_cmts (rest_old, rest_new)
  | Some old :: rest_old, cmt :: rest_new ->
      (old ^ cmt_space ^ cmt) :: merge_cmts (rest_old, rest_new)
  | None :: rest_old, [] -> "" :: merge_cmts (rest_old, [])
  | Some old :: rest_old, [] -> old :: merge_cmts (rest_old, [])
  | [], cmt :: rest_new -> cmt :: merge_cmts ([], rest_new)

let cut_cmts body =
  let re_body_cmt = Str.regexp {|\(.*\)Φ\(.*\)φ +Ξ\(.+\)ξ|} in
  List.map
    (fun l ->
      match Str.string_match re_body_cmt l 0 with
      | false -> (l, None)
      | true ->
          let line = Str.matched_group 1 l ^ Str.matched_group 2 l in
          let cmt = Str.matched_group 3 l in
          (line, Some cmt) )
    (lines body)

let fix_annotate cmts body =
  let body, existing_cmts = cut_cmts body |> List.split in
  let existing_cmts = rstrip_none existing_cmts in
  let cmts = merge_cmts (existing_cmts, cmts) in
  let indent =
    List.map (fun l -> String.length l + ncmt_space) body
    |> List.fold_left max 40
  in
  let rec fmt = function
    | [], [] -> []
    | body :: rbody, [] -> body :: fmt (rbody, [])
    | [], cmt :: rcmt ->
        ("Φφ" ^ String.make indent ' ' ^ "Ξ" ^ cmt ^ "ξ") :: fmt ([], rcmt)
    | body :: rbody, cmt :: rcmt ->
        ( "Φ" ^ body ^ "φ"
        ^ String.make (indent - String.length body) ' '
        ^ "Ξ" ^ cmt ^ "ξ" )
        :: fmt (rbody, rcmt)
  in
  fmt (body, cmts) |> String.concat "\n"

let annotate cmt body =
  let cmts = if cmt = "" then [] else lines cmt in
  fix_annotate cmts body

let prettify body =
  body |> fix_annotate [] |> Str.global_replace (Str.regexp {|[ΦφΞξ]|}) ""
