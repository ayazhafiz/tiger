open Temp

type instr =
  | Oper of
      { assem : string
      ; dst : temp list
      ; src : temp list
      ; jmp : label list option
            (** [None] if [assem] always falls through; otherwise, must be a list
              with any fall-through made explicit. *)
      }
  | Label of {assem : string; lab : label}
  | Mov of {assem : string; dst : temp; src : temp}

let string_of_instr string_of_temp instr =
  let explode s =
    let rec expl i l = if i < 0 then l else expl (i - 1) (s.[i] :: l) in
    expl (String.length s - 1) []
  in
  let fmt assem dst src jmp =
    let pick lst n = List.nth lst (int_of_char n) in
    let rec walk = function
      | [] -> ""
      | '`' :: 'd' :: n :: rst -> pick dst n ^ walk rst
      | '`' :: 's' :: n :: rst -> pick src n ^ walk rst
      | '`' :: 'j' :: n :: rst -> pick jmp n ^ walk rst
      | c :: rst -> String.make 1 c ^ walk rst
    in
    walk (explode assem)
  in
  let assem, dst, src, jmp =
    match instr with
    | Oper {assem; dst; src; jmp} ->
        (assem, dst, src, Option.value jmp ~default:[])
    | Label {assem; _} -> (assem, [], [], [])
    | Mov {assem; dst; src} -> (assem, [dst], [src], [])
  in
  fmt assem
    (List.map string_of_temp dst)
    (List.map string_of_temp src)
    (List.map string_of_label jmp)
