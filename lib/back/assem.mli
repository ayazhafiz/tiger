open Temp

(** NB: Comments should be added in reverse order; i.e. prepended to the
    existing list. *)
type instr =
  | Oper of
      { assem : string
      ; dst : temp list
      ; src : temp list
      ; jmp : label list option
            (** [None] if [assem] always falls through; otherwise, must be a list
              with any fall-through made explicit. *)
      ; mutable comments : string list }
  | Label of {assem : string; lab : label; mutable comments : string list}
  | Mov of
      {assem : string; dst : temp; src : temp; mutable comments : string list}

val add_comment : string -> instr -> unit

val fmt_instrs : (temp -> string) -> string -> bool -> instr list -> string
(** [fmt_instrs string_of_temp comment_prefix eliminate_moves instrs] formats
    [instrs] for assembly emission by a backend.
    An instruction will be eliminated when
      - It is empty.
      - [eliminate_moves] is true and dst=src for a move. *)

val string_of_instr : ?string_of_temp:(temp -> string) -> instr -> string
(** [string_of_instr instr] provides a debug view of an [instr]. A procedure to
    pretty-print temporaries may optionally be provided. *)
