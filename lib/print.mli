open Language

val lines : string -> string list

val reflow : int -> string -> string
(** [reflow indent s] re-formats [s] with whitespace indentation [indent]. *)

val reflown1 : int -> string -> string
(** Like [reflow], but does not indent the first line. *)

val string_of_var : var -> string
val string_of_expr : expr -> string
val string_of_decl : decl -> string
val string_of_ty : ty -> string
val fmt_expr : Format.formatter -> expr -> unit

(* TODO: hide result of annotate behind [annotated_string] type. *)

val annotate : string -> string -> string
(** [annotate cmt body] vertically adds [cmt] to the right side of [body].
    The result of [annotate] is an intermediate representation that can be
    reused in subsequent calls to [annotate], even across inclusion of the
    returned string in other strings.
    However, the returned string should *not* be modified.
    Before using the returned string for display, [prettify] should be called. *)

val prettify : string -> string
(** Realigns a string consisting of one or more results of [annotate] and
    prepares it for end-user display. *)
