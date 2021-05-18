(** Module [Canon] canonicalizes the Tiger [Ir] for later phases like isel. *)

val linearize : Ir.stmt -> Ir.stmt list
(** [linearize stmt] produces a list of [Ir.stmt]s based on [stmt] with the
    property that
      - all [Ir.Seq], [Ir.ESeq]s are eliminated
      - [Ir.Call]s are on the toplevel, inside [Ir.Mov] or [Ir.Expr] *)

val basic_blocks : Ir.stmt list -> Ir.stmt list list * Temp.label
(** [basic_blocks stmts] partitions a sequence of a statements into a sequence
    of basic blocks, where each basic block
      1. has a [Ir.Label] as its first statement
      2. has a [Ir.Jmp] or [Ir.CJmp] as its last statement
      3. The presence of 1. and 2. are unique in this basic block.

    It is expected that the input to [basic_blocks] will have no terminating
    [Ir.Jmp] or [Ir.CJmp]; instead, a fresh label to which control flow will be
    passed to after the last statement will be generated and added to the
    generated basic blocks. This is the second return value.

    Precondition: [basic_blocks] must be called with the output of [linearize]. *)

val trace_schedule : Ir.stmt list list * Temp.label -> Ir.stmt list
(** [trace_schedule basic_blocks] reorders a series of basic blocks retrieved
    from [basic_blocks] so that
      - Each [Ir.CJmp (op, e1, e2, t, f)] is followed by its false label [f].
      - Trivial jumps [Ir.Jmp (lab)] are eliminated by falling through to the
        block corresponding to [lab].

    The final block [F] returned from [basic_blocks] is also cleaned up by
    attaching an [Ir.Label] of fresh label jumped to at the end of [F] to the
    scheduled sequence of statements. *)
