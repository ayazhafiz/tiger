open Frame

module Backend (F : FRAME) : sig
  val emit_assem : Language.expr -> string
  (** [emit_assem expr] compiles an expression to assembly.
      [expr] must be checked by [Semantic] before compilation. *)
end
