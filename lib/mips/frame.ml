open Tiger.Frame

module MipsFrame : Frame = struct
  type access =
    | InReg of Temp.temp
    | InFrame of int  (** offset from frame pointer *)

  type frame = {
    name : Temp.label;
    formals : access list;
    mutable sp_offset : int;
        (** current offset from frame pointer, where all memory addresses
            between (fp+sp_offset) and (fp) are being used for variables
            in the frame. *)
  }
  (** See page 127 for Appel's suggested stack layout.
                             ...higher addr
               |-----------|
               |argument n |
               |     .     |
      incoming |     .     | previous
        args   |     .     | frame
               |argument 2 |
               |argument 1 |
               |static link|            (actually seen as an argument)
      FP ----> |-----------|-----------
               |local vars | current
               |           | frame
               |-----------|
               |return addr|
               |           |
               |   temps   |
               |           |
               |   saved   |
               | registers |
               |-----------|
               |argument n |
               |     .     |
      outgoing |     .     |
        args   |     .     |
               |argument 2 |
               |argument 1 |
               |static link|            (actually seen as an argument)
      SP ----> |-----------|-----------
                             next frame
                             ...lower addr
   *)

  let fp = Temp.newtemp ()

  let wordsize = 4

  let expr_of_access = function
    | InReg reg, _ -> Ir.Temp reg (* just return the register access *)
    | InFrame offset, addr_orig_fp ->
        (* fetch the variable its true address *)
        Ir.Mem (Ir.BinOp (Ir.Plus, addr_orig_fp, Ir.Const offset))

  let max_formals_registers = 4

  let new_frame name formals =
    let rec alloc formals nfregs arg_offset =
      match (formals, nfregs, arg_offset) with
      | [], _, _ -> []
      (* formal escapes, we need it on stack *)
      | true :: rest, nfregs, offset ->
          InFrame offset :: alloc rest nfregs (offset + wordsize)
      (* no more registers left for this argument, put on stack *)
      | false :: rest, 0, offset ->
          InFrame offset :: alloc rest 0 (offset + wordsize)
      (* doesn't escape, we can put it in a fresh register *)
      | false :: rest, nfregs, offset ->
          InReg (Temp.newtemp ()) :: alloc rest (nfregs - 1) offset
    in
    { name; formals = alloc formals max_formals_registers 0; sp_offset = 0 }

  let name { name; _ } = name

  let formals { formals; _ } = formals

  let alloc_local f = function
    | false -> InReg (Temp.newtemp ()) (* doesn't escape *)
    | true ->
        (* Escapes; allocate on the stack.
           SP          old SP               FP
           | new local | ...existing locals | ...incoming args
            -----------
            [wordsize]
        *)
        f.sp_offset <- f.sp_offset - wordsize;
        InFrame f.sp_offset
end
