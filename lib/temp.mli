(** Module [Temp] provides a notion of temporary lifetimes for variables, and
    temporary labels for functions. *)

type temp

val newtemp : unit -> temp

type label

val stringEqual : label

val initArray : label

val newlabel : string -> label
