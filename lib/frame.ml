(** An architecture-specific representation of a stack frame. *)
module type FRAME = sig
  type frame

  type access
  (** Denotes the access location of a variable in this frame. *)

  (** Fragment types. *)
  type frag =
    | Proc of frame * Ir.stmt  (** A procedure *)
    | String of Temp.label * string  (** A string literal *)

  val fp : Temp.temp
  (** The frame pointer of the present frame.
      Should be stored in a constant location. *)

  val rv : Temp.temp
  (** Return value location; should be constant. *)

  val wordsize : int
  (** Native word size of the architecture this frame represents. *)

  val expr_of_access : access * Ir.expr -> Ir.expr
  (** [expr (a, addr_orig_fp)] returns an [Ir.expr] for a variable access [a],
      given the address [addr_orig_fp] of the frame the access was originally
      created in. *)

  val new_frame : Temp.label -> bool list -> frame
  (** [new_frame name formals] creates a new frame for a function [name] with
      [formals], each of whose entry indicates whether the formal escapes the
      frame. *)

  val name : frame -> Temp.label
  (** [name frame] retrieves the named label of a frame. *)

  val formals : frame -> access list
  (** [formals frame] retrieves the access location of formal arguments. *)

  val alloc_local : frame -> bool -> access
  (** [allocLocal frame escape] allocates a local variable with a given [escape]
      qualifier in [frame]. *)

  val external_call : Temp.label -> Ir.expr list -> Ir.expr
  (** [external_call name args] performs a call to an external procedure. *)

  val proc_entry_exit1 : frame -> Ir.stmt -> Ir.stmt
end
