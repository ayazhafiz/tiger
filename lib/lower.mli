open Frame
open Desugar

type expr

module LOWER (F : FRAME) : sig
  val lower : desugared_expr -> F.frag list
  (** Lowers a program in the high-level language to a series of frame-dependent
      fragments. *)
end
