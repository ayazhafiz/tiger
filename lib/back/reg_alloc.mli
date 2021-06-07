module RegisterAllocation (F : Frame.FRAME) : sig
  val reg_alloc : F.frame -> Assem.instr list -> Assem.instr list * F.allocation
  (** Allocates registers for temporaries in an [Assem] program. Returns an
      updated version of the [Assem] program with any instructions introduced
      for spilled registers, and the allocation itself. *)
end
