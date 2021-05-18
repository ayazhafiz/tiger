open Language

val string_of_var : var -> string

val string_of_expr : expr -> string

val string_of_decl : decl -> string

val string_of_ty : ty -> string

val fmt_expr : Format.formatter -> expr -> unit
