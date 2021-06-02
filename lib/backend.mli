open Frame

type exit_status = Exit of int | Killed of int

module Backend (F : FRAME) : sig
  val emit_ir : Language.expr -> string
  (** [emit_ir expr] emits a canonicalized IR of all function frames in the
      program, and any strings. *)

  val emit_assem : Language.expr -> string
  (** [emit_assem expr] compiles an expression to assembly.
      [expr] must be checked by [Semantic] before compilation. *)

  val exec : Language.expr -> string * string * exit_status
  (** [exec expr] compiles and executes an expression on the present machine.
      Returns a tuple [(stdout, stderr, exit_status)]. *)

  module Debug : sig
    val emit_pseudo_assem : Language.expr -> string
    (** Like [emit_assem], but without register allocation. *)
  end
end
