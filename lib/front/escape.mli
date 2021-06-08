open Language

val mark : expr -> unit
(** [mark exprt] analyzes and records the following markers in subexpressions of
    [expr]:
    1. Variables that escape (i.e. are used in nested scopes)
    2. Variables that not "naked rvalues" (are not return values or assigned to
       other variables)
       Examples:
         let ... in a end
         b := a
       Non-examples:
         a[0]
         a.b *)
