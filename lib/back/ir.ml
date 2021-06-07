(** The Tiger intermediate representation. *)

open Temp

type expr =
  | Const of int
  | Name of label
  | Temp of temp
  | BinOp of binop * expr * expr
  | Mem of expr * string
      (** Memory address. As an rvalue, this refers to a fetch from the address;
          as an lvalue, it is a store into the memory address. *)
  | Call of expr * expr list
  | ESeq of stmt * expr  (** [Seq] followed by [expr] *)

and stmt =
  | Expr of expr
  | Mov of expr * expr * string
  | Jmp of expr * label list  (** to where, possible landing sites *)
  | CJmp of relop * expr * expr * label * label * string
  | Seq of stmt * stmt
  | Label of label  (** Define value of [label] as current machine addr *)

and binop =
  | Plus
  | Minus
  | Mul
  | Div
  | And
  | Or
  | Shl  (** left shift *)
  | Shr  (** right shift *)
  | Sar  (** arithmetic right shift *)
  | Xor

and relop = Eq | Neq | Lt | Gt | Leq | Geq | Ult | Ule | Ugt | Uge

let cmt_of_expr = function Mem (_, cmt) -> cmt | _ -> ""

let cmt_of_stmt = function
  | Mov (_, _, cmt) | CJmp (_, _, _, _, _, cmt) -> cmt
  | _ -> ""

let not_relop = function
  | Eq -> Neq
  | Neq -> Eq
  | Lt -> Geq
  | Gt -> Leq
  | Leq -> Gt
  | Geq -> Lt
  | Ult -> Uge
  | Ule -> Ugt
  | Ugt -> Ule
  | Uge -> Ult

let seq lst =
  match lst with
  | [] -> Expr (Const 0)
  | fst :: rest -> List.fold_left (fun sq next -> Seq (sq, next)) fst rest

let string_of_irs string_of_temp stmts =
  let open Util.Print in
  let binop = function
    | Plus -> "+"
    | Minus -> "-"
    | Mul -> "*"
    | Div -> "/"
    | And -> "&"
    | Or -> "|"
    | Shl -> "<<"
    | Shr -> ">>"
    | Sar -> ">>>"
    | Xor -> "^"
  in
  let relop = function
    | Eq -> "="
    | Neq -> "!="
    | Lt -> "<"
    | Gt -> ">"
    | Leq -> "<="
    | Geq -> ">="
    | Ult -> "<u"
    | Ule -> "<=u"
    | Ugt -> ">u"
    | Uge -> ">=u"
  in
  let rec expr = function
    | Const n -> string_of_int n
    | Name lab -> string_of_label lab
    | Temp t -> string_of_temp t
    | BinOp (op, e1, e2) ->
        let op = binop op in
        let indent = String.length op + 1 in
        Printf.sprintf "%s(%s,\n%s)" op
          (reflown1 indent (expr e1))
          (reflow indent (expr e2))
    | Mem (e, cmt) ->
        Printf.sprintf "Mem(%s)" (reflown1 4 (expr e)) |> annotate cmt
    | Call (fn, args) ->
        Printf.sprintf "Call(%s,\n%s)"
          (reflown1 5 (expr fn))
          (List.map (fun a -> reflow 5 (expr a)) args |> String.concat ",\n")
    | ESeq (s, e) ->
        Printf.sprintf "ESeq(%s,\n%s)" (reflown1 5 (stmt s)) (reflow 5 (expr e))
  and stmt = function
    | Expr e -> expr e
    | Mov (e1, e2, cmt) ->
        Printf.sprintf "Mov(%s,\n%s)"
          (reflown1 4 (expr e1))
          (reflow 4 (expr e2))
        |> annotate cmt
    | Jmp (e1, where) ->
        Printf.sprintf "Jmp(%s,\n%s)"
          (reflown1 4 (expr e1))
          (reflow 4
             ( List.map string_of_label where
             |> String.concat ", " |> Printf.sprintf "[%s]" ) )
    | CJmp (op, e1, e2, t, f, cmt) ->
        Printf.sprintf "CJmp(%s,\n%s,\n%s,\n%s,\n%s)"
          (reflown1 4 (relop op))
          (reflow 4 (expr e1))
          (reflow 4 (expr e2))
          (reflow 4 (string_of_label t))
          (reflow 4 (string_of_label f))
        |> annotate cmt
    | Seq (s1, s2) ->
        Printf.sprintf "Seq(%s,\n%s)"
          (reflown1 3 (stmt s1))
          (reflow 3 (stmt s2))
    | Label lab -> string_of_label lab ^ ":"
  in
  List.map stmt stmts |> String.concat "\n" |> prettify

let string_of_ir string_of_temp st = string_of_irs string_of_temp [st]
