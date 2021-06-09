(** Module [Temp] provides a notion of temporary lifetimes for variables, and
    temporary labels for functions. *)

type temp

val string_of_temp : temp -> string
val newtemp : unit -> temp
val tempeq : temp -> temp -> bool

type label

val string_of_label : label -> string
val stringEqual : label
val initArray : label
val ttexit : label
val newlabel : string -> label

val strlabel : string -> label
(** [strlabel mand] returns a label corresponding exactly to [mand]. *)

module TempSet : Set.S with type elt = temp
module LabelSet : Set.S with type elt = label
module LabelHashtbl : Hashtbl.S with type key = label

val reset : temp list -> label list -> unit
(** Resets temps and labels for a fresh compilation. *)
