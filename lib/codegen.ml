open Frame

module type CODEGEN = sig
  module Frame : FRAME

  val codegen : Frame.frame -> Ir.stmt -> Assem.instr list
end
