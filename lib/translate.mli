open Frame
open Symbol

type expr

module Translate (F : Frame) : sig
  type level
  (** A nesting level in a program, corresponding to the scope of a function. *)

  type access
  (** Access kind of a variable. *)

  val fragments : F.frag list ref

  val toplevel : level

  val newlevel : level -> Temp.label -> bool list -> level
  (** [newlevel parent name formals] creates a new nesting level with [formals]
    specifying the escape status of each formal variable introduced to the
    level. *)

  val formals : level -> access list
  (** Retrieves the access kinds of formal parameters into a level. *)

  val alloc_local : level -> bool -> access
  (** [alloc_local lvl escape] allocates a variable of some access kind at a
    level given whether it escapes. *)

  (******************)
  (* IR translation *)
  (******************)

  val ir_basic_var : access -> level -> expr
  (** [ir_basic_var access at_level] returns an expression to access a variable
      of a particular [access] kind at a present [leve]. *)

  val ir_subscript_var : expr -> expr -> expr
  (** [ir_subscript_var base_arr idx] returns an expression to access an
      array [base_arr] at index [idx]. *)

  val ir_field_var : expr -> symbol list -> symbol -> expr
  (** [ir_field_var base_rcd fields field] returns an expression to access a
      record [base_rcd] with [fields] at [field]. *)

  val ir_binop : Language.oper -> expr -> expr -> expr
  (** [ir_binop op left right] translates a binary operation. *)

  val ir_ifthen : expr -> expr -> expr
  (** [ir_ifthen test then'] translates an [if test then then'] expression. *)

  val ir_strlit : string -> expr
  (** Creates a string literal. *)

  val ir_stringeq : expr -> expr -> expr
  (** String equality. *)

  val ir_stringneq : expr -> expr -> expr
  (** String inequality. *)

  val ir_array : expr -> expr -> expr
  (** [ir_array size init] creates an array with [size] elements each
      initialized to [init], and returns an address to the array. *)

  val ir_record : expr list -> expr
  (** [ir_record fields] creates a record with [fields]. *)

  val ir_while : expr -> expr -> Temp.label -> expr
  (** [ir_while test body break] creates a while loop with [test], [body], and
      break label [break]. *)

  val ir_call : level -> level -> expr list -> expr
  (** [ir_call caller target args] calls a function at [level target] with
      [args] from [level caller]. *)
end
