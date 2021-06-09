let sanitize_string_for_label str =
  let rec walk i n rest =
    if n >= 10 || i >= String.length str then ""
    else
      match str.[i] with
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as c ->
          String.make 1 c ^ walk (i + 1) (n + 1) rest
      | _ -> walk (i + 1) n rest
  in
  walk 0 0 str
