open Temp

type instr =
  | Oper of
      { assem : string
      ; dst : temp list
      ; src : temp list
      ; jmp : label list option
            (** [None] if [assem] always falls through; otherwise, must be a list
              with any fall-through made explicit. *)
      }
  | Label of {assem : string; lab : label}
  | Mov of {assem : string; dst : temp; src : temp}

let string_of_instr _string_of_temp = function
  | Oper _ -> failwith ""
  | Label _ -> failwith ""
  | Mov _ -> failwith ""
