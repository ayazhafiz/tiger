open Language

val mark_escapes : expr -> unit
(** [mark_escapes expr] finds and marks variables that escape their scopes. *)
