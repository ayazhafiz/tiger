open Backend
open Lower

(* MIPS *)
open Mips_frame
module MipsTranslate = Translate (MipsFrame)
module MipsBackend = Backend (MipsFrame)

(* X86 64 Bit *)
open X86_64_frame
module X86_64_Translate = Translate (X86_64_Frame)
module X86_64_Backend = Backend (X86_64_Frame)
