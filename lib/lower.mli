(** Lowers the Tiger [Language] to an [Ir]. *)

open Frame
open Desugar

module Translate (F : FRAME) : sig
  val lower : desugared_expr -> Temp.label * F.frag list
  (** Lowers a program in the high-level language to a series of frame-dependent
      fragments. Also returns the label corresponding to the entry point to the
      program. *)
end
