module MipsFrame : Frame.FRAME = struct
  type access =
    | InReg of Temp.temp
    | InFrame of int  (** offset from frame pointer *)

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
  type frame =
    { name : Temp.label
    ; formals : access list
    ; mutable sp_offset : int
          (** current offset from frame pointer, where all memory addresses
            between (fp+sp_offset) and (fp) are being used for variables
            in the frame. *)
    }

  type frag = Proc of frame * Ir.stmt | String of Temp.label * string
  type proc = {prolog : string; body : Assem.instr list; epilog : string}
  type register = string

  module RegisterSet = Set.Make (struct
    type t = register

    let compare = compare
  end)

  (** https://en.wikipedia.org/wiki/MIPS_architecture#Calling_conventions
      Name     Number   Use                           Callee must preserve?
      $zero    $0       constant 0                    N/A
      $at      $1       assembler temporary           No
      $v0–$v1  $2–$3    values for function returns   No
                        and expression evaluation
      $a0–$a3  $4–$7    function arguments            No
      $t0–$t7  $8–$15	  temporaries                   No
      $s0–$s7  $16–$23  saved temporaries             Yes
      $t8–$t9  $24–$25  temporaries                   No
      $k0–$k1  $26–$27  reserved for OS kernel        N/A
      $gp      $28      global pointer                Yes
      $sp	     $29      stack pointer                 Yes
      $fp	     $30      frame pointer                 Yes
      $ra	     $31      return address                N/A *)
  let registers =
    [
    "$zero"; "$at";
    "$v0"; "$v1"; (* returns, expr evals *)
    "$a0"; "$a1"; "$a2"; "$a3"; (* fn args *)
    "$t0"; "$t1"; "$t2"; "$t3"; "$t4"; "$t5"; "$t6"; "$t7"; "$t8"; "$t9"; (* caller-save temps *)
    "$s0"; "$s1"; "$s2"; "$s3"; "$s4"; "$s5"; "$s6"; "$s7"; (* callee-save temps *)
    "$k0"; "$k1"; (* kernel reserved *)
    "$gp"; "$sp"; "$fp"; "$ra" (* etc globals *)
    ] [@ocamlformat "disable"]

  let reg_temps = List.map (fun reg -> (reg, Temp.newtemp ())) registers
  let temp_map = List.map (fun (r, t) -> (t, r)) reg_temps
  let temp_of_reg reg = List.assoc reg reg_temps
  let fp = List.assoc "$fp" reg_temps
  let rv = List.assoc "$v0" reg_temps
  let ra = List.assoc "$ra" reg_temps
  let sp = List.assoc "$sp" reg_temps
  let zero = List.assoc "$zero" reg_temps
  let special_regs = [fp; rv; ra; sp; zero]
  let arg_regs = ["$a0"; "$a1"; "$a2"; "$a3"] |> List.map temp_of_reg

  let callee_saves =
    List.filter (fun (r, _) -> String.sub r 0 2 = "$s") reg_temps
    |> List.map snd

  let caller_saves =
    ( List.filter (fun (r, _) -> String.sub r 0 2 = "$t") reg_temps
    |> List.map snd )
    @ arg_regs

  let calldefs = rv :: ra :: caller_saves
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
    {name; formals = alloc formals max_formals_registers 0; sp_offset = 0}

  let name {name; _} = name
  let formals {formals; _} = formals

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

  let external_call fn args = Ir.Call (Ir.Name fn, args)
  let proc_entry_exit1 _ body = body

  let proc_entry_exit2 _ body =
    body
    @ [ Assem.Oper
          { assem = ""
          ; (* Mark registers as still being live (necessary) at procedure exit. *)
            src = [zero; ra; sp] @ callee_saves
          ; dst = []
          ; jmp = None } ]

  let proc_entry_exit3 {name; _} body =
    { prolog = "PROCEDURE " ^ Temp.string_of_label name ^ "\n"
    ; body
    ; epilog = "END " ^ Temp.string_of_label name ^ "\n" }
end
