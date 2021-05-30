(** An architecture-specific representation of a stack frame. *)
module type FRAME = sig
  type frame

  (** Denotes the access location of a variable in this frame. *)
  type access

  (** Fragment types. *)
  type frag =
    | Proc of frame * Ir.stmt  (** A procedure *)
    | String of Temp.label * string  (** A string literal *)

  type proc = {prolog : string; body : Assem.instr list; epilog : string}

  (** The type of a register. *)
  type register

  module RegisterSet : Set.S with type elt = register

  val registers : register list
  (** All registers on this machine. *)

  val string_of_register : register -> string

  val fp : Temp.temp
  (** The frame pointer of the present frame.
      Should be stored in a constant location. *)

  val rv : Temp.temp
  (** Return value location; should be constant. *)

  val arg_regs : Temp.temp list
  (** Registers available for arguments. *)

  val special_regs : Temp.temp list
  (** "Special-purpose" or reserved registers that should be constantly preserved. *)

  val callee_saves : Temp.temp list
  (** Registers the callee of a function is required to save. *)

  val caller_saves : Temp.temp list
  (** Registers the caller of a function is required to save, as the callee is
      free to use them arbitrarily. *)

  val calldefs : Temp.temp list
  (** Registers trashed (definitely used) by a function inside a call. *)

  val temp_map : (Temp.temp * register) list
  (** A mapping of a register to its machine name. *)

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
      qualifier in [frame]. If [escape] is true, the local variable is
      guaranteed to be put on the stack. *)

  val external_call : Temp.label -> Ir.expr list -> Ir.expr
  (** [external_call name args] performs a call to an external procedure. *)

  val assem_of_string : Temp.label * string -> string
  (** Generates an assembly string for a [String]. *)

  val assem_body_indent : string
  (** Indentation of emitted instructions in a block body. *)

  val assem_complete : string list -> string list -> string
  (** [assem_complete strings frames] produces a final assembly file content for
      the target machine. *)

  val proc_entry_exit1 : frame -> Ir.stmt -> Ir.stmt
  val proc_entry_exit2 : frame -> Assem.instr list -> Assem.instr list
  val proc_entry_exit3 : frame -> Assem.instr list -> proc
end
