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

val cmt_of_expr : expr -> string
val cmt_of_stmt : stmt -> string
val not_relop : relop -> relop
val seq : stmt list -> stmt
val string_of_ir : (temp -> string) -> stmt -> string
val string_of_irs : (temp -> string) -> stmt list -> string
