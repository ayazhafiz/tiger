open Front
open Back.Frame

type exit_status = Exit of int | Killed of int
type machine = X86_64_apple_darwin20_1_0 | X86_64_linux_gnu

val lkg_runtime : machine -> string
(** Maps a machine to a last-known-good object file containing the runtime. *)

val machine_of_triple : string -> machine
(** Translates a string triple to a [machine]. *)

val triple_of_machine : machine -> string
(** Translates a [machine] to a string triple. *)

val all_machines : machine list
(** All [machine]s targetable by the driver.  *)

val current_machine : unit -> machine
(** Discovers the [machine] this executable is running on. *)

val readfi : string -> string
val writefi : string -> string -> unit
val read_in : in_channel -> string
val sh : ?stdin:string option -> string -> string * string * Unix.process_status

module Backend (F : FRAME) : sig
  val emit_ir : Language.expr -> string
  (** [emit_ir expr] emits a canonicalized IR of all function frames in the
      program, and any strings. *)

  val emit_assem : Language.expr -> string
  (** [emit_assem expr] compiles an expression to assembly.
      [expr] must be checked by [Semantic] before compilation. *)

  val emit_exe : machine -> Language.expr -> string
  (** [emit_exe machine expr] compiles an expression to assembly.
      [expr] must be checked by [Semantic] before compilation. *)

  val exec :
    machine -> Language.expr -> string option -> string * string * exit_status
  (** [exec machine expr stdin] compiles and executes an expression on [machine].
      Returns a tuple [(stdout, stderr, exit_status)]. *)

  module Debug : sig
    val emit_pseudo_assem : Language.expr -> string
    (** Like [emit_assem], but without register allocation. *)
  end
end
