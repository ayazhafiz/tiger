open Driver

(* MIPS *)
open Back.Mips_frame
module MipsBackend = Backend (MipsFrame)

(* X86 64 Bit *)
open Back.X86_64_frame
module X86_64_Backend = Backend (X86_64_Frame)
