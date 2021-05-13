type symbol = string * int

val nextsym : int ref

val symbol : string -> symbol

val name : symbol -> string

(** A mutable symbol table. To make copies of the table, use [Table.copy]. *)
module Table : sig
  type 'a t
  (** The type of a symbol table. *)

  val singleton : unit -> 'a t
  (** Creates an empty symbol table with one scope. *)

  val enter : 'a t -> unit
  (** Enters a scope. *)

  val exit : 'a t -> unit
  (** Exits a scope. *)

  val add : 'a t -> symbol -> 'a -> unit
  (** Adds a value to the table. *)

  val find : 'a t -> symbol -> 'a option
  (** Looks up a value in the table. *)

  val copy : 'a t -> 'a t
  (** Deep copy of the symbol table. *)

  val string_of : 'a t -> ('a -> string) -> string
  (** String view of the symbol table, with formatted scopes. *)
end
