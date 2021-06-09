open Front
open Back.Frame

type exit_status = Exit of int | Killed of int
type machine = X86_64_apple_darwin20_1_0 | X86_64_linux_gnu

val sh : ?stdin:string option -> string -> string * string * Unix.process_status
val lkg_runtime : machine -> string
(* Maps a machine to a last-known-good object file containing the runtime. *)

module Backend (F : FRAME) : sig
  val emit_ir : Language.expr -> string
  (** [emit_ir expr] emits a canonicalized IR of all function frames in the
      program, and any strings. *)

  val emit_assem : Language.expr -> string
  (** [emit_assem expr] compiles an expression to assembly.
      [expr] must be checked by [Semantic] before compilation. *)

  val exec : Language.expr -> string option -> string * string * exit_status
  (** [exec expr stdin] compiles and executes an expression on the present
      machine. Returns a tuple [(stdout, stderr, exit_status)]. *)

  module Debug : sig
    val emit_pseudo_assem : Language.expr -> string
    (** Like [emit_assem], but without register allocation. *)
  end
end
