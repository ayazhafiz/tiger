(** A representation of a machine and its stack frames for register allocation. *)
module type ALLOCATION_FRAME = sig
  (** The type of a register. *)
  type register

  val string_of_register : register -> string

  module RegisterSet : Set.S with type elt = register

  val registers : register list
  (** All registers on this machine. *)

  (** Allocation of temporaries in a program to registers. *)
  type allocation = (Temp.temp, register) Hashtbl.t

  val temp_map : (Temp.temp * register) list
  (** A mapping of a register to its machine name. *)

  (** Type of a stack frame on this machine. *)
  type frame

  (** Denotes the access location of a variable in this frame. *)
  type access

  val alloc_local : frame -> string option -> bool -> access
  (** [alloc_local frame name escapes] allocates a local variable with a given
      [escape] qualifier in [frame]. If [escape] is true, the local variable is
      guaranteed to be put on the stack. If [name] is [None], a fresh name is
      given. *)

  val fetch_from_access :
    frame -> Temp.temp -> access -> string -> Assem.instr list
  (** [fetch fr t access comment] is equivalent to generating assembly for
      Mov(t, access, comment). *)

  val store_to_access :
    frame -> access -> Temp.temp -> string -> Assem.instr list
  (** [store fr access t comment] is equivalent to generating assembly for
      Mov(access, t, comment). *)
end

(* TODO: rename FRAME *)

(** An architecture-specific representation of a stack frame. *)
module type FRAME = sig
  include ALLOCATION_FRAME

  (** Fragment types. *)
  type frag =
    | Proc of frame * Ir.stmt  (** A procedure *)
    | String of Temp.label * string  (** A string literal *)

  type proc

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

  val wordsize : int
  (** Native word size of the architecture this frame represents. *)

  val expr_of_access : access * Ir.expr -> Ir.expr
  (** [expr_of_access (a, addr_orig_fp)] returns an [Ir.expr] for a variable access [a],
      given the address [addr_orig_fp] of the frame the access was originally
      created in. *)

  val address_of_access : access * Ir.expr -> Ir.expr
  (** Like [expr_of_access], but retrieves a memory address pointing to an
      [access] rather than the value at that access.
      @raises ice if the access is not on the stack *)

  val new_frame : Temp.label -> string list -> bool list -> frame
  (** [new_frame name formal_names formals] creates a new frame for a function
      [name] with [formals], each of whose entry indicates whether the formal
      escapes the frame. *)

  val alloc_stack : frame -> string option -> int -> access
  (** [alloc_stack frame name size] allocates a local variable of given [size]
      on the stack in a [frame]. *)

  val name : frame -> Temp.label
  (** [name frame] retrieves the named label of a frame. *)

  val formals : frame -> access list
  (** [formals frame] retrieves the access location of formal arguments. *)

  val external_call : frame -> Temp.label -> Ir.expr list -> Ir.expr
  (** [external_call calling_frame name args] performs a call to an external procedure. *)

  val codegen : frame -> Ir.stmt -> Assem.instr list
  (** Generates [Assem]-style assembly instructions from the [Ir]. *)

  val proc_entry_exit1 : frame -> Ir.stmt -> Ir.stmt
  val proc_entry_exit2 : frame -> Assem.instr list -> Assem.instr list
  val proc_entry_exit3 : frame -> Assem.instr list -> proc

  val emit :
       (Temp.label * string) list
    -> (frame * Assem.instr list * allocation) list
    -> Temp.label
    -> string
  (** [emit strings blocks main] generates assembly for a program consisting of
      strings [strings] and procedure blocks [blocks]. *)

  val reserved_temps : Temp.temp list
  val reserved_labels : Temp.label list

  module Debug : sig
    val emit_debug :
         (Temp.label * string) list
      -> (frame * Assem.instr list) list
      -> (Temp.temp -> string)
      -> Temp.label
      -> string
    (** Like [emit], but does not require a register [allocation]. *)
  end
end
