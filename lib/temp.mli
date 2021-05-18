(** Module [Temp] provides a notion of temporary lifetimes for variables, and
    temporary labels for functions. *)

type temp

val newtemp : unit -> temp

type label

val string_of_label : label -> string

val stringEqual : label

val initArray : label

val newlabel : string -> label

val strlabel : string -> label
(** [strlabel mand] returns a label corresponding exactly to [mand]. *)
