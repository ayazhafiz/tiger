open Frame

module type CODEGEN = sig
  module F : FRAME

  val codegen : F.frame -> Ir.stmt -> Assem.instr list
end
