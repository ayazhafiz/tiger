(** Module [Desugar] provides processes for desugarring the language before
    conversion to [Ir]. *)

open Language

type desugared_expr

val desugar_expr : expr -> desugared_expr

val expr_of_desugared : desugared_expr -> expr
