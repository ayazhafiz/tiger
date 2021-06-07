type symbol = string * int

val nextsym : int ref
val symbol : string -> symbol
val symeq : symbol -> symbol -> bool
val name : symbol -> string

module SymbolHashtbl : Hashtbl.S with type key = symbol

(** A mutable symbol table. To make copies of the table, use [Table.copy]. *)
module Table : sig
  (** The type of a symbol table. *)
  type 'a t

  val singleton : unit -> 'a t
  (** Creates an empty symbol table with one scope. *)

  val scoped : 'a t -> ('a t -> 'b) -> 'b
  (** [scoped tbl go] evaluates [go tbl] in a new scope. *)

  val add : 'a t -> symbol -> 'a -> unit
  (** Adds a value to the table. *)

  val find : 'a t -> symbol -> 'a
  (** Looks up a key in the table.
      Raises [Not_found] if the key does not exist. *)

  val find_opt : 'a t -> symbol -> 'a option
  (** Looks up a key in the table. *)

  val keys : 'a t -> symbol list
  (** Retrieves the keys in the table, unique but in no particular order. *)

  val copy : 'a t -> 'a t
  (** Deep copy of the symbol table. *)

  val string_of : 'a t -> ('a -> string) -> string
  (** String view of the symbol table, with formatted scopes. *)
end
