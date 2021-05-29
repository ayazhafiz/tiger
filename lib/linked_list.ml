let ice why = failwith ("ICE (linked_list): " ^ why)

(** An immutable doubly-linked list. *)
type 'a dllist = Empty | Lst of 'a list * 'a * 'a list  (** prev, cur, next *)

let of_list = function hd :: tl -> Lst ([], hd, tl) | [] -> Empty

let to_list = function
  | Empty -> []
  | Lst (prevs, cur, rest) -> List.rev prevs @ [cur] @ rest

let empty = Empty

let cur = function
  | Empty -> ice "attempted to read empty list"
  | Lst (_, cur, _) -> cur

let prev = function
  | Lst (p :: prevs, cur, nexts) -> (Lst (prevs, p, cur :: nexts), p)
  | Lst _ -> ice "front of list reached"
  | Empty -> ice "list is empty"

let next = function
  | Lst (prevs, cur, n :: nexts) -> (Lst (cur :: prevs, n, nexts), n)
  | Lst _ -> ice "front of list reached"
  | Empty -> ice "list is empty"

(** Adds an item to the prev of the current element, and returns a list pointing
    to the added element. *)
let add_prev item = function
  | Empty -> of_list [item]
  | Lst (prevs, cur, nexts) -> Lst (prevs, item, cur :: nexts)

(** Adds an item to the next of the current element, and returns a list pointing
    to the added element. *)
let add_next item = function
  | Empty -> of_list [item]
  | Lst (prevs, cur, nexts) -> Lst (cur :: prevs, item, nexts)

(** Appends two lists. If exactly one is empty, the list returned is the
    non-empty list. If both are non empty, the returned list points to the
    existing item pointed to by the first list with the entirety of the second
    list appended. *)
let append l1 l2 =
  match (l1, l2) with
  | Empty, Empty -> Empty
  | Empty, l | l, Empty -> l
  | Lst (prevs, cur, nexts), l2 -> Lst (prevs, cur, nexts @ to_list l2)

let is_empty = function Empty -> true | Lst _ -> false
