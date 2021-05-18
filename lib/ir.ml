(** The Tiger intermediate representation. *)

open Temp

type expr =
  | Const of int
  | Name of label
  | Temp of temp
  | BinOp of binop * expr * expr
  | Mem of expr
      (** Memory address. As an rvalue, this refers to a fetch from the address;
          as an lvalue, it is a store into the memory address. *)
  | Call of expr * expr list
  | ESeq of stmt * expr  (** [Seq] followed by [expr] *)

and stmt =
  | Expr of expr
  | Mov of expr * expr
  | Jmp of expr * label list  (** to where, possible landing sites *)
  | CJmp of relop * expr * expr * label * label
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
