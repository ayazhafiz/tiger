module X86_64_Frame : sig
  include Frame.FRAME

  val rax : Temp.temp
  val rdx : Temp.temp
  val rsp : Temp.temp
end = struct
  type access =
    | InReg of Temp.temp
    | InFrame of int
        (** Offset from frame pointer, in downward direction of stack.
            E.g. if this is 8, the access is located at $fp-8. *)

  (** Stack layout for x86 64 bit. See also page 127.
      Note: in internal functions, argument 1 is always a static link to the
      statically-enclosing frame.
                             ...higher addr
               |-----------|
               |argument n |                                -
               |     .     |                                |
      incoming |     .     | previous                       | generated
        args   |     .     | frame                          | during call
               |argument 8 |                                |
               |argument 7 | (args 1-6 passed in registers) -
               |    RA     |                                - side effect of call 
               |  old ebp  |                                - gen during prolog
      FP ----> |-----------|-----------
               |  local 1  | current
               |  local 2  | frame
               |     .     |
               |     .     |
               |     .     |
               |  local n  |
      SP ----> |-----------|
                             ...lower addr
   *)
  type frame =
    { name : Temp.label
    ; formals : access list
    ; sp_offset : int ref
          (** current offset from frame pointer, where all memory addresses
            between (fp-sp_offset) and (fp) are being used for variables
            in the frame. Like [InFrame], this grows in the negative direction. *)
    }

  type frag = Proc of frame * Ir.stmt | String of Temp.label * string
  type proc = {prolog : string; body : Assem.instr list; epilog : string}
  type register = string

  module RegisterSet = Set.Make (struct
    type t = register

    let compare = compare
  end)

  (** https://wiki.cdot.senecacollege.ca/wiki/X86_64_Register_and_Instruction_Quick_Start *)
  let registers =
    [
    "rax"; "rbx"; "rcx"; "rdx";
    "rbp"; (* base pointer (fp) *)
    "rsp"; (* stack pointer *)
    "rsi"; (* register source index (source for data copies) *)
    "rdi"; (* register destination index (destination for data copies) *)
    "r8";  "r9";  "r10"; "r11";
    "r12"; "r13"; "r14"; "r15";
    ] [@ocamlformat "disable"]

  let reg_temps = List.map (fun reg -> (reg, Temp.newtemp ())) registers
  let temp_map = List.map (fun (r, t) -> (t, r)) reg_temps
  let temp_of_reg reg = List.assoc reg reg_temps
  let rax = List.assoc "rax" reg_temps
  let rdx = List.assoc "rdx" reg_temps
  let rsp = List.assoc "rsp" reg_temps
  let rbp = List.assoc "rbp" reg_temps
  let fp = rbp
  let rv = rax
  let sp = rsp
  let special_regs = [fp; rv; sp]
  (* Registers live at procedure exit. Mentioned to avoid register allocation
     here. *)

  let arg_regs =
    ["rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9"] |> List.map temp_of_reg

  let callee_saves = ["rbx"; "r12"; "r13"; "r14"; "r15"] |> List.map temp_of_reg
  let caller_saves = arg_regs @ (["rax"; "r10"; "r11"] |> List.map temp_of_reg)
  let calldefs = rsp :: rbp :: caller_saves
  (* registers modified/defined by a [call] instr.
     [rsp] - RA is pushed on.
     [rbp] - Updated by function prolog.
     [caller_saves] - mark as being re-defined on call entry, so that we do not
       attempt to allocate temps used across procedure calls to these registers,
       or if they are allocated, that a spill to save in memory will be done. *)

  let wordsize = 8 (* 64-bit *)

  let expr_of_access = function
    | InReg reg, _ -> Ir.Temp reg (* just return the register access *)
    | InFrame offset, addr_orig_fp ->
        (* fetch the variable its true address *)
        Ir.Mem (Ir.BinOp (Ir.Minus, addr_orig_fp, Ir.Const offset))

  let alloc_local1 sp_offset = function
    | false -> InReg (Temp.newtemp ()) (* doesn't escape *)
    | true ->
        (* Escapes; allocate on the stack.
           SP          old SP               FP
           | new local | ...existing locals | ...incoming args
            -----------
            [wordsize]
        *)
        sp_offset := !sp_offset + wordsize;
        InFrame !sp_offset

  let alloc_local fr = alloc_local1 fr.sp_offset

  let new_frame name formals =
    let sp_offset = ref 0 in
    {name; formals = List.map (alloc_local1 sp_offset) formals; sp_offset}

  let name {name; _} = name
  let formals {formals; _} = formals
  let external_call fn args = Ir.Call (Ir.Name fn, args)
  let proc_entry_exit1 _ body = body

  let proc_entry_exit2 _ body =
    body
    @ [ Assem.Oper
          { assem = ""
          ; (* Mark special/reserved registers as still being live (necessary)
               at procedure exit. This means that the registers are live
               throughout, which prevents the register allocator from trying to
               use them for any other purpose (page 209). *)
            src = special_regs @ callee_saves
          ; dst = []
          ; jmp = None } ]

  let proc_entry_exit3 {name; _} body =
    { prolog = "PROCEDURE " ^ Temp.string_of_label name ^ "\n"
    ; body
    ; epilog = "END " ^ Temp.string_of_label name ^ "\n" }
end
