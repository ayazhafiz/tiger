module RegisterAllocation
    (F : Frame.FRAME)
    (CG : Codegen.CODEGEN with module F = F) : sig
  type allocation = (Temp.temp, F.register) Hashtbl.t

  val reg_alloc : F.frame -> Assem.instr list -> Assem.instr list * allocation
  (** Allocates registers for temporaries in an [Assem] program. Returns an
      updated version of the [Assem] program with any instructions introduced
      for spilled registers, and the allocation itself. *)
end
