open Backend
open Lower

(* MIPS *)
open Mips_frame
open Mips_codegen
module MipsTranslate = Translate (MipsFrame)
module MipsBackend = Backend (MipsFrame) (MipsCodegen)

(* X86 64 Bit *)
open X86_64_frame
open X86_64_codegen
module X86_64_Translate = Translate (X86_64_Frame)
module X86_64_Backend = Backend (X86_64_Frame) (X86_64_Codegen)
