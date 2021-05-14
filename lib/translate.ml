open Frame
open Temp
module Ast = Language

type expr =
  | Ex of Ir.expr  (** a true expression *)
  | Nx of Ir.stmt  (** valueless expression, for example [Ast.WhileExpr] *)
  | Cx of (label -> label -> Ir.stmt)
      (** Formulates a conditional jump expression.
          Given two labels (true-dest, false-dest), formulates a statement
          that evaluates a conditional and then jumps to the appropriate
          destination.
          I.e. is a translation for some conditional expression [a < b] to
          the result of that conditional, whether it be a value or branches
          in a conditional statement.
          E.g. [a>b|c<d] ~> [Cx(fn (t, f) => Seq(CJump(Gt, a, b, t, z)
                                                 Seq(Label z,
                                                     CJump(Lt, c, d, t, f))))]
               given a fresh label [z]. *)

let seq lst =
  let fst = List.hd lst in
  List.fold_left (fun sq next -> Ir.Seq (sq, next)) fst (List.tl lst)

(** [asEx expr] "unwraps" a lowered expression into a pure [Ir.expr],
    regardless of its true form. *)
let asEx = function
  | Ex e -> e
  | Nx stmt ->
      (* Evaluate stmt, give effectively nothing back *)
      Ir.Eseq (stmt, Ir.Const 0)
  | Cx mk_jmp ->
      (* Need to convert conditional jump into evaluation of the underlying
         conditional expression.
         Create a fresh variable [v] and fresh jump locations. In the true
         branch set [v = 1]; in the false branch set [v = 0]. Then evaluate
         [v]; this amounts exactly to evaluation of the conditional. *)
      let t = newlabel "true" in
      let f = newlabel "false" in
      let r = Ir.Temp (newtemp ()) in
      Ir.Eseq
        ( seq
            [
              Ir.Mov (r, Ir.Const 1);
              mk_jmp t f;
              (* false -> r = 0 *)
              Ir.Label f;
              Ir.Mov (r, Ir.Const 0);
              (* true -> fallthrough *)
              Ir.Label t;
            ],
          r )

(** [asNx expr] "unwraps" a lowered expression into a pure [Ir.stmt],
    regardless of its true form. *)
let rec asNx = function
  | Ex e -> Ir.Expr e
  | Nx stmt -> stmt
  | Cx _ as cx -> asNx (Ex (asEx cx))

(** [asCx expr] "unwraps" a lowered expression into a pure
    [label -> label -> Ir.stmt], regardless of its true form. *)
let asCx = function
  | Ex (Ir.Const 0) -> fun _ f -> Ir.Jmp (Ir.Name f, [ f ])
  | Ex (Ir.Const 1) -> fun t _ -> Ir.Jmp (Ir.Name t, [ t ])
  | Ex cond -> fun t f -> Ir.CJmp (Ir.Neq, cond, Ir.Const 0, t, f)
  | Nx _ ->
      failwith
        "Internal error (lowering): valueless expression treated as conditional"
  | Cx f -> f

let memadd base off = Ir.Mem (Ir.BinOp (Ir.Plus, base, off))

let sanitize_label str =
  let rec walk i n rest =
    if n >= 10 || i >= String.length str then ""
    else
      match str.[i] with
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as c ->
          String.make 1 c ^ walk (i + 1) (n + 1) rest
      | _ -> walk (i + 1) n rest
  in
  walk 0 0 str

module Translate (F : Frame) = struct
  type level =
    | Toplevel
    | Nest of { frame : F.frame; parent : level; uuid : int }

  type access = level * F.access (* level declared, underlying access *)

  let fragments = ref []

  let toplevel = Toplevel

  let newlevel =
    let uuid = ref 0 in
    fun parent name formals ->
      let real_formals = true :: formals (* add static link *) in
      let frame = F.new_frame name real_formals in
      uuid := !uuid + 1;
      Nest { frame; parent; uuid = !uuid }

  let formals = function
    | Toplevel ->
        failwith "Domain error (translate): cannot get formals of toplevel"
    | Nest { frame; _ } as lvl -> List.map (fun a -> (lvl, a)) (F.formals frame)

  let alloc_local = function
    | Toplevel, _ ->
        failwith "Domain error (translate): cannot allocate locals on toplevel "
    | (Nest { frame; _ } as lvl), escapes -> (lvl, F.alloc_local frame escapes)

  let leveleq = function
    | Toplevel, Toplevel -> true
    | Nest { uuid = u1; _ }, Nest { uuid = u2; _ } -> u1 = u2
    | _ -> false

  (** [sl_of_from target base] retrieves a read of the static link (frame
      pointer address) of [target] from the function frame of [base]. *)
  let sl_of_from target base =
    let rec walk curlvl curfp_expr =
      match (curlvl, target) with
      | Toplevel, _ | _, Toplevel ->
          failwith
            "Domain error (translate): cannot walk static links to toplevel"
      | Nest { uuid = u1; _ }, Nest { uuid = u2; _ } when u1 = u2 ->
          (* We're here, so the built-up expression should already be a derefence
             resulting in a static link to [target]. *)
          curfp_expr
      | Nest { frame; parent; _ }, Nest _ ->
          (* Not yet at the level the variable was declared; we want to walk up
             to the parent. The static link to the parent is located at the
             first formal argument into the present frame, and [curfp_expr]
             currently represents the address of present frame (the present
             frame pointer). So just read the first formal to get the frame
             pointer of the parent. *)
          let parent_sl = List.hd (F.formals frame) in
          let parentfp_expr = F.expr_of_access (parent_sl, curfp_expr) in
          walk parent parentfp_expr
    in
    (* Start at our current frame pointer *)
    walk base (Ir.Temp F.fp)

  let ir_basic_var (level_decl, varaccess) level_used =
    let fp_decl = sl_of_from level_decl level_used in
    F.expr_of_access (varaccess, fp_decl)

  let ir_subscript_var base_arr idx =
    (* See page 159: The base address is the contents of pointer variable;
       indeed arrays are not structured/"large" in Tiger. Instead, we fetch
       the data in the memory location at [&base_arr], derefence it to a
       pointer that is the address of the actual array (this is the contents of
       [base_arr] below). Then, calculate the item in the array we want, and
       derefence that. *)
    let base_arr = asEx base_arr in
    let offset = Ir.BinOp (Ir.Mul, asEx idx, Ir.Const F.wordsize) in
    Ex (memadd base_arr offset)

  let ir_field_var base_rcd fields field =
    (* records decay into arrays; once we find the offset of the field, we can
       defer our work. *)
    let field_idx = List.nth fields field in
    ir_subscript_var base_rcd (Ex (Ir.Const field_idx))

  let ir_binop op left right =
    let left = asEx left in
    let right = asEx right in
    let arith op = Ex (Ir.BinOp (op, left, right)) in
    let cond op = Cx (fun t f -> Ir.CJmp (op, left, right, t, f)) in
    match op with
    | Ast.PlusOp -> arith Ir.Plus
    | Ast.MinusOp -> arith Ir.Minus
    | Ast.TimesOp -> arith Ir.Mul
    | Ast.DivideOp -> arith Ir.Div
    | Ast.EqOp -> cond Ir.Eq
    | Ast.NeqOp -> cond Ir.Neq
    | Ast.LtOp -> cond Ir.Lt
    | Ast.LeOp -> cond Ir.Leq
    | Ast.GtOp -> cond Ir.Gt
    | Ast.GeOp -> cond Ir.Geq

  let ir_ifthen test then' =
    let test = asCx test in
    let then' = asNx then' in
    let t = newlabel "true" in
    let f = newlabel "false" in
    Nx (seq [ test t f; Ir.Label t; then'; Ir.Label f ])

  let ir_ifthenelse test then' else' =
    (* TODO: smarter lowering for cases when then', else' are Nx, Cx (page 162) *)
    let test = asCx test in
    let then' = asEx then' in
    let else' = asEx else' in
    let t = newlabel "true" in
    let f = newlabel "false" in
    let r = newtemp () in
    let join = newlabel "join" in
    Ex
      (Ir.Eseq
         ( seq
             [
               test t f;
               (* true *)
               Ir.Label t;
               Ir.Mov (Ir.Temp r, then');
               Ir.Jmp (Ir.Name join, [ join ]);
               (* false *)
               Ir.Label f;
               Ir.Mov (Ir.Temp r, else');
               Ir.Jmp (Ir.Name join, [ join ]);
               (* join *)
               Ir.Label join;
             ],
           Ir.Temp r ))

  let ir_strlit str =
    let lab = newlabel ("str__" ^ sanitize_label str) in
    let frag = F.frag_of_string lab str in
    fragments := frag :: !fragments;
    Ex (Ir.Name lab)

  let ir_stringeq s1 s2 =
    Ex (F.external_call "stringEqual" [ asEx s1; asEx s2 ])

  let ir_stringneq s1 s2 =
    Ex (Ir.BinOp (Ir.Xor, asEx (ir_stringeq s1 s2), Ir.Const 1))

  let ir_array size init =
    let size = Ir.BinOp (Ir.Mul, asEx size, Ir.Const F.wordsize) in
    Ex (F.external_call "initArray" [ size; asEx init ])

  let ir_record fields =
    let fields = List.map asEx fields in
    let nfields = List.length fields in
    (* Records decay to arrays; just dummy init with 0s first. *)
    let rcd_alloc = ir_array (Ex (Ir.Const nfields)) (Ex (Ir.Const 0)) in
    let rcd = Ir.Temp (Temp.newtemp ()) in
    let rcd_init = Ir.Mov (rcd, asEx rcd_alloc) in
    let init_field idx field =
      let rcdf =
        memadd rcd (Ir.BinOp (Ir.Mul, Ir.Const idx, Ir.Const F.wordsize))
      in
      Ir.Mov (rcdf, field)
    in
    let init_fields = List.mapi init_field fields in
    Ex (Ir.Eseq (seq (rcd_init :: init_fields), rcd))

  let ir_while test body finish =
    let ltest = newlabel "test" in
    let lbody = newlabel "body" in
    let test = asEx test in
    let body = asNx body in
    Nx
      (seq
         [
           Ir.Label ltest;
           Ir.CJmp (Ir.Eq, test, Ir.Const 0, finish, lbody);
           Ir.Label lbody;
           body;
           Ir.Jmp (Ir.Name ltest, [ ltest ]);
           Ir.Label finish;
         ])

  let ir_call caller target args =
    let args = List.map asEx args in
    match target with
    | Toplevel -> failwith "Inconsistent state (lower): callee has no level"
    | Nest { parent = Toplevel; _ } ->
        failwith "Domain error (lower): cannot call function on toplevel"
    | Nest { parent; frame; _ } ->
        let sl = sl_of_from parent caller in
        Ex (Ir.Call (Ir.Name (F.name frame), sl :: args))
end
