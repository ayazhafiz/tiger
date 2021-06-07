open Front.Symbol
open Frame
open Temp
module Tbl = Front.Symbol.Table
module Env = Front.Env
module Ty = Front.Type
module Ast = Front.Language
module Print = Util.Print

let ice why = failwith ("ICE (lower): " ^ why)

(** Expression representation for use during lowering.
    Characterizes expressions in different ways so as to ease flexibility and
    efficiency of lowered code. See also [unEx], [unNx], [unCx]. *)
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

(** [unEx expr] "unwraps" a lowered expression into a pure [Ir.expr],
    regardless of its true form. *)
let unEx = function
  | Ex e -> e
  | Nx stmt ->
      (* Evaluate stmt, give effectively nothing back *)
      Ir.ESeq (stmt, Ir.Const 0)
  | Cx mk_jmp ->
      (* Need to convert conditional jump into evaluation of the underlying
         conditional expression.
         Create a fresh variable [v] and fresh jump locations. In the true
         branch set [v = 1]; in the false branch set [v = 0]. Then evaluate
         [v]; this amounts exactly to evaluation of the conditional. *)
      let t = newlabel "true" in
      let f = newlabel "false" in
      let r = Ir.Temp (newtemp ()) in
      Ir.ESeq
        ( Ir.seq
        [ Ir.Mov (r, Ir.Const 1, "true")
        ; mk_jmp t f
        ; Ir.Label f (* false -> r = 0 *)
        ; Ir.Mov (r, Ir.Const 0, "false")
        ; Ir.Label t (* true -> fallthrough *)
        ][@ocamlformat "disable"]
        , r )

(** [unNx expr] "unwraps" a lowered expression into a pure [Ir.stmt],
    regardless of its true form. *)
let rec unNx = function
  | Ex e -> Ir.Expr e
  | Nx stmt -> stmt
  | Cx _ as cx -> unNx (Ex (unEx cx))

(** [unCx expr] "unwraps" a lowered expression into a pure
    [label -> label -> Ir.stmt], regardless of its true form. *)
let unCx = function
  | Ex (Ir.Const 0) -> fun _ f -> Ir.Jmp (Ir.Name f, [f])
  | Ex (Ir.Const 1) -> fun t _ -> Ir.Jmp (Ir.Name t, [t])
  | Ex cond ->
      fun t f -> Ir.CJmp (Ir.Neq, cond, Ir.Const 0, t, f, Ir.cmt_of_expr cond)
  | Nx _ -> ice "valueless expression treated as conditional"
  | Cx f -> f

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

let unravel ty what =
  match !ty with
  | Some ty -> ty
  | None -> ice ("type of " ^ what ^ " not checked")

let ty_of_var v =
  let open Ast in
  let vs = string_of_var v in
  match v with
  | SimpleVar (_, ty) -> unravel ty vs
  | FieldVar (_, _, ty) -> unravel ty vs
  | SubscriptVar (_, _, ty) -> unravel ty vs

let ty_of_expr expr =
  let open Ast in
  let es = string_of_expr expr in
  match expr with
  | NilExpr -> Ty.Nil
  | VarExpr (_, ty) -> unravel ty es
  | IntExpr _ -> Ty.Int
  | StringExpr _ -> Ty.String
  | CallExpr {ty; _} -> unravel ty es
  | OpExpr {ty; _} -> unravel ty es
  | RecordExpr {ty; _} -> unravel ty es
  | SeqExpr (_, ty) -> unravel ty es
  | AssignExpr _ -> Ty.Unit
  | IfExpr {ty; _} -> unravel ty es
  | WhileExpr _ -> Ty.Unit
  | ForExpr _ -> Ty.Unit
  | BreakExpr -> Ty.Unit
  | LetExpr {ty; _} -> unravel ty es
  | ArrayExpr {ty; _} -> unravel ty es

let is_val expr = ty_of_expr expr <> Ty.Unit

module Translate (F : FRAME) = struct
  type level = Toplevel | Nest of {frame : F.frame; parent : level; uuid : int}

  let fragments = ref []
  let clear_frags _ = fragments := []
  let get_frags _ = !fragments

  let newlevel =
    let uuid = ref 0 in
    fun parent name formal_names formals ->
      let real_formals = true :: formals (* add static link *) in
      let real_formal_names = "static link" :: formal_names in
      let frame = F.new_frame name real_formal_names real_formals in
      uuid := !uuid + 1;
      Nest {frame; parent; uuid = !uuid}

  let formals = function
    | Toplevel -> ice "cannot get formals of toplevel"
    | Nest {frame; _} as lvl ->
        List.map (fun a -> (lvl, a)) (F.formals frame)
        |> (* drop static link *) List.tl

  (** Allocates a fresh local variable in a level. *)
  let alloc_local level name escapes =
    match (level, escapes) with
    | Toplevel, _ -> ice "cannot allocate locals on toplevel "
    | (Nest {frame; _} as lvl), escapes ->
        (lvl, F.alloc_local frame name escapes)

  (** [sl_of_from target base] retrieves a read of the static link (frame
      pointer address) of [target] from the function frame of [base]. *)
  let sl_of_from target base =
    let rec walk curlvl curfp_expr =
      match (curlvl, target) with
      | Toplevel, _ | _, Toplevel -> ice "cannot walk static links to toplevel"
      | Nest {uuid = u1; _}, Nest {uuid = u2; _} when u1 = u2 ->
          (* We're here, so the built-up expression should already be a derefence
             resulting in a static link to [target]. *)
          curfp_expr
      | Nest {frame; parent; _}, Nest _ ->
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

  let frame_of = function
    | Toplevel -> ice "toplevel has no frame"
    | Nest {frame; _} -> frame

  (******************)
  (* IR translation *)
  (******************)

  (** [ir_basic_var decl_lvl access usage_lvl] returns an expression to access a
      variable declared at [decl_lvl] with a particular [access], used at a
      present [usage_lvl]. *)
  let ir_basic_var decl_lvl access usage_lvl =
    let fp_decl = sl_of_from decl_lvl usage_lvl in
    Ex (F.expr_of_access (access, fp_decl))

  (** [ir_subscript_var base_arr idx] returns an expression to access an
      array [base_arr] at index [idx]. *)
  let ir_subscript_var base_arr idx access_fmt =
    (* See page 159: The base address is the contents of pointer variable;
       indeed arrays are not structured/"large" in Tiger. Instead, we fetch
       the data in the memory location at [&base_arr], derefence it to a
       pointer that is the address of the actual array (this is the contents of
       [base_arr] below). Then, calculate the item in the array we want, and
       derefence that. *)
    let base_arr = unEx base_arr in
    let offset = Ir.BinOp (Ir.Mul, unEx idx, Ir.Const F.wordsize) in
    let access = Ir.Mem (Ir.BinOp (Ir.Plus, base_arr, offset), access_fmt) in
    Ex access

  (** [ir_field_var base_rcd fields field] returns an expression to access a
      record [base_rcd] with [fields] at [field]. *)
  let ir_field_var base_rcd fields field access_fmt =
    (* records decay into arrays; once we find the offset of the field, we can
       defer our work. *)
    let rec walk i = function
      | [] -> ice "field not in fields"
      | cur :: _ when symeq cur field -> i
      | _ :: rest -> walk (i + 1) rest
    in
    let field_idx = walk 0 fields in
    ir_subscript_var base_rcd (Ex (Ir.Const field_idx)) access_fmt

  (** [ir_binop op left right] translates a binary operation on integers
      (or pointers). For strings, see [ir_stringeq]. *)
  let ir_binop op left right binop_fmt =
    let left = unEx left in
    let right = unEx right in
    let arith op = Ex (Ir.BinOp (op, left, right)) in
    let cond op = Cx (fun t f -> Ir.CJmp (op, left, right, t, f, binop_fmt)) in
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

  (** [ir_ifthen test then'] translates an [if test then then'] expression. *)
  let ir_ifthen test then' =
    let test = unCx test in
    let then' = unNx then' in
    let t = newlabel "true" in
    let f = newlabel "false" in
    Nx (Ir.seq [test t f; Ir.Label t; then'; Ir.Label f])

  (** [ir_ifthen test then' else'] translates an [if test then then' else else']
      expression. *)
  let ir_ifthenelse test then' else' =
    let test = unCx test in
    match (then', else') with
    | Nx then', Nx else' ->
        let t = newlabel "true" in
        let f = newlabel "false" in
        let join = newlabel "join" in
        Nx
          (Ir.seq
             [ test t f; (* true *) Ir.Label t; then'
             ; Ir.Jmp (Ir.Name join, [join]); (* false *) Ir.Label f; else'
             ; Ir.Jmp (Ir.Name join, [join]); (* join *) Ir.Label join ] )
    | (Cx _ as then'), else' | then', (Cx _ as else') ->
        (* If either branch is itself a test, attempt to produce nicer code by
           yielding a Cx that jumps to a "true" label if the whole expression is
           true, and to a "false" label otherwise.
           E.g.: if test' then a>b else c<d *)
        let then_test = unCx then' in
        let else_test = unCx else' in
        Cx
          (fun t f ->
            let if_t = newlabel "if_t" in
            let if_f = newlabel "if_f" in
            Ir.seq
              [ test if_t if_f; (* true branch *) Ir.Label if_t; then_test t f
              ; (* false branch *) Ir.Label if_f; else_test t f ] )
    | then', else' ->
        let then' = unEx then' in
        let else' = unEx else' in
        let t = newlabel "true" in
        let f = newlabel "false" in
        let r = newtemp () in
        let join = newlabel "join" in
        Ex
          (Ir.ESeq
             ( Ir.seq
                 [ test t f; (* true *) Ir.Label t
                 ; Ir.Mov (Ir.Temp r, then', "then")
                 ; Ir.Jmp (Ir.Name join, [join]); (* false *) Ir.Label f
                 ; Ir.Mov (Ir.Temp r, else', "else")
                 ; Ir.Jmp (Ir.Name join, [join]); (* join *) Ir.Label join ]
             , Ir.Temp r ) )

  (** Creates a string literal. *)
  let ir_strlit str =
    match
      List.find_opt
        (function F.String (_, s) when s = str -> true | _ -> false)
        !fragments
    with
    | Some (F.String (lab, _)) -> Ex (Ir.Name lab)
    | Some _ -> ice "unreachanble"
    | None ->
        let lab = newlabel ("str__" ^ sanitize_label str) in
        let frag = F.String (lab, str) in
        fragments := frag :: !fragments;
        Ex (Ir.Name lab)

  (** String equality. *)
  let ir_stringeq calling_lvl s1 s2 =
    Ex (F.external_call (frame_of calling_lvl) stringEqual [unEx s1; unEx s2])

  (** String inequality. *)
  let ir_stringneq calling_lvl s1 s2 =
    Ex (Ir.BinOp (Ir.Xor, unEx (ir_stringeq calling_lvl s1 s2), Ir.Const 1))

  (** [ir_array size init] creates an array with [size] elements each
      initialized to [init], and returns an address to the array. *)
  let ir_array calling_lvl size init =
    let size = Ir.BinOp (Ir.Mul, unEx size, Ir.Const F.wordsize) in
    Ex (F.external_call (frame_of calling_lvl) initArray [size; unEx init])

  (** [ir_record fields creator] creates a record with [fields]. *)
  let ir_record calling_lvl fields field_names field_exprs =
    let fields = List.map unEx fields in
    let nfields = List.length fields in
    (* Records decay to arrays; just dummy init with 0s first. *)
    let rcd_alloc =
      ir_array calling_lvl (Ex (Ir.Const nfields)) (Ex (Ir.Const 0))
    in
    let rcd = Ir.Temp (Temp.newtemp ()) in
    let rcd_init = Ir.Mov (rcd, unEx rcd_alloc, "" (* TODO: add info? *)) in
    let init_field idx field =
      let offset = Ir.BinOp (Ir.Mul, Ir.Const idx, Ir.Const F.wordsize) in
      let fname = "." ^ (List.nth field_names idx |> name) in
      let fexpr = Ast.string_of_expr (List.nth field_exprs idx) in
      let access = Ir.Mem (Ir.BinOp (Ir.Plus, rcd, offset), fname) in
      Ir.Mov (access, field, Printf.sprintf "%s=%s" fname fexpr)
    in
    let init_fields = List.mapi init_field fields in
    Ex (Ir.ESeq (Ir.seq (rcd_init :: init_fields), rcd))

  (** [ir_while test body break] creates a while loop with [test], [body], and
      break label [break]. *)
  let ir_while test test_expr body finish =
    let testcmt = Ast.string_of_expr test_expr in
    let ltest = newlabel "test" in
    let lbody = newlabel "body" in
    let test = unEx test in
    let body = unNx body in
    Nx
      (Ir.seq
         [ Ir.Label ltest
         ; Ir.CJmp (Ir.Eq, test, Ir.Const 0, finish, lbody, testcmt)
         ; Ir.Label lbody; body; Ir.Jmp (Ir.Name ltest, [ltest])
         ; Ir.Label finish ] )

  (** [ir_call caller target target_label args has_ret] calls a function at
      level [target] with [args] from level [caller]. If [has_ret] is true the
      called function has a return value. *)
  let ir_call caller target target_label args has_ret =
    let args = List.map unEx args in
    let calling_frame =
      match caller with
      | Toplevel -> ice "caller of function is not toplevel?"
      | Nest {frame; _} -> frame
    in
    let args, is_extern =
      match target with
      | Toplevel -> (args, true)
      | Nest {parent = Toplevel; _} -> ice "cannot call function on toplevel"
      | Nest {parent; _} ->
          (* Need to pass static link to the parent level of the function level
             we are about to call, per our calling convention (page 133) *)
          let sl = sl_of_from parent caller in
          (sl :: args, false)
    in
    let arg_moves, args' =
      List.mapi
        (fun i a ->
          let a' = Ir.Temp (newtemp ()) in
          let argkind =
            if is_extern then string_of_int (i + 1)
            else if i = 0 then "(static_link)"
            else string_of_int i
          in
          let cmt =
            Printf.sprintf "%%arg%s:%s" argkind (string_of_label target_label)
          in
          (Ir.Mov (a', a, cmt), a') )
        args
      |> List.split
    in
    let call =
      if is_extern then F.external_call calling_frame target_label args'
      else Ir.Call (Ir.Name target_label, args')
    in
    match has_ret with
    | true ->
        let rvtmp = Ir.Temp (Temp.newtemp ()) in
        Ex
          (Ir.ESeq
             ( Ir.seq
                 (arg_moves @ [Ir.Expr call; Ir.Mov (rvtmp, Ir.Temp F.rv, "")])
             , rvtmp ) )
    | false -> Nx (Ir.seq (arg_moves @ [Ir.Expr call]))

  (** [ir_seq exprs has_val] translates a sequence of [expr]s, conditioned on
      whether the last expr returns a value or not. The sequence may also be
      empty, in which case [has_val] is irrelevant. *)
  let ir_seq exprs has_val =
    match exprs with
    | [] -> Nx (Ir.Expr (Ir.Const 0))
    | exprs -> (
      match has_val with
      | false -> Nx (Ir.seq (List.map unNx exprs))
      | true ->
          let rec split = function
            | [] -> ice "impossible state"
            | [last] -> ([], last)
            | hd :: rest ->
                let stmts, last = split rest in
                (hd :: stmts, last)
          in
          let stmts, fin = split exprs in
          Ex (Ir.ESeq (Ir.seq (List.map unNx stmts), unEx fin)) )

  let ir_assign var_access expr assign_fmt =
    Nx (Ir.Mov (unEx var_access, unEx expr, assign_fmt))

  (** [proc_entry_exit lvl body has_ret] stores a procedure fragment. *)
  let proc_entry_exit level body has_ret =
    match level with
    | Toplevel -> ice "cannot save toplevel fn"
    | Nest {frame; _} ->
        let body' =
          if has_ret then
            let ret_expr = unEx body in
            let ret_cmt =
              Printf.sprintf "return (%s)" (Ir.cmt_of_expr ret_expr)
            in
            Ir.Mov (Ir.Temp F.rv, ret_expr, ret_cmt)
          else unNx body
        in
        let body'' = F.proc_entry_exit1 frame body' in
        fragments := F.Proc (frame, body'') :: !fragments

  (****************)
  (* Lowering Env *)
  (****************)

  type ventry =
    | VarEntry of level * F.access  (** level declared in, underlying access *)
    | FunEntry of level * label

  let getvar venv sym =
    match Tbl.find_opt venv sym with
    | Some (VarEntry (lvl, a)) -> (lvl, a)
    | None -> ice ("no entry " ^ name sym)
    | _ -> ice (name sym ^ " not a VarEntry")

  let getfn venv sym =
    match Tbl.find_opt venv sym with
    | Some (FunEntry (lvl, lab)) -> (lvl, lab)
    | None -> ice ("no entry " ^ name sym)
    | _ -> ice (name sym ^ " not a FunEntry")

  let base_venv =
    let t = Tbl.singleton () in
    List.iter
      (fun extern ->
        Tbl.add t extern (FunEntry (Toplevel, strlabel (name extern))) )
      (Tbl.keys (Env.base_venv ()));
    fun () -> Tbl.copy t

  let br = function
    | Some break -> break
    | None -> ice "attempting to access break in bad scope"

  (**************************)
  (* Lowering: Main Drivers *)
  (**************************)

  let rec lower_var ctx =
    let venv, usage_lvl, _break = ctx in
    let open Ast in
    function
    | SimpleVar (v, _) ->
        let decl_lvl, access = getvar venv v in
        ir_basic_var decl_lvl access usage_lvl
    | FieldVar (v, f, _) as va -> (
      match ty_of_var v with
      | Ty.Record (fields, _) ->
          ir_field_var (lower_var ctx v) (List.map fst fields) f
            (Ast.string_of_var va)
      | t ->
          ice ("field receiver checked as " ^ Ty.string_of_ty t ^ ", not record")
      )
    | SubscriptVar (v, idx, _) as va ->
        ir_subscript_var (lower_var ctx v) (lower_expr ctx idx)
          (Ast.string_of_var va)

  and lower_expr ctx =
    let venv, usage_lvl, break = ctx in
    let open Ast in
    function
    | NilExpr -> Ex (Ir.Const 0)
    | VarExpr (v, _) -> lower_var ctx v
    | IntExpr n -> Ex (Ir.Const n)
    | StringExpr s -> ir_strlit s
    | CallExpr {func; args; _} as e ->
        let target_lvl, target_label = getfn venv func in
        let args = List.map (lower_expr ctx) args in
        ir_call usage_lvl target_lvl target_label args (is_val e)
    | OpExpr {left; oper; right; _} as op -> (
        let left' = lower_expr ctx left in
        let right' = lower_expr ctx right in
        match ty_of_expr left with
        | Ty.String -> (
          match oper with
          | EqOp -> ir_stringeq usage_lvl left' right'
          | NeqOp -> ir_stringneq usage_lvl left' right'
          | _ -> ice "non-equality string operator" )
        | _ -> ir_binop oper left' right' (Ast.string_of_expr op) )
    | RecordExpr {fields; _} ->
        let field_names, field_exprs = List.split fields in
        ir_record usage_lvl
          (List.map (lower_expr ctx) field_exprs)
          field_names field_exprs
    | SeqExpr (exprs, _) as e ->
        ir_seq (List.map (lower_expr ctx) exprs) (is_val e)
    | AssignExpr {var; expr} as ae ->
        let lv = lower_var ctx var in
        let rv = lower_expr ctx expr in
        ir_assign lv rv (Ast.string_of_expr ae)
    | IfExpr {test; then'; else' = None; _} ->
        ir_ifthen (lower_expr ctx test) (lower_expr ctx then')
    | IfExpr {test; then'; else' = Some else'; _} ->
        ir_ifthenelse (lower_expr ctx test) (lower_expr ctx then')
          (lower_expr ctx else')
    | WhileExpr {test = test_expr; body} ->
        let test = lower_expr ctx test_expr in
        let break' = Temp.newlabel "break" in
        let body = lower_expr (venv, usage_lvl, Some break') body in
        ir_while test test_expr body break'
    | ForExpr _ as f ->
        ice
          ("for (" ^ Ast.string_of_expr f ^ ") must be desugared by this point")
    | BreakExpr ->
        let break = br break in
        Nx (Ir.Jmp (Ir.Name break, [break]))
    | LetExpr {decls; body; _} as e ->
        Tbl.scoped venv (fun venv ->
            let ctx = (venv, usage_lvl, break) in
            let decls = List.concat_map (lower_decl ctx) decls in
            let body = lower_expr ctx body in
            (* This is effectively a sequence in the IR, so translate it as such. *)
            ir_seq (List.concat [decls; [body]]) (is_val e) )
    | ArrayExpr {size; init; _} ->
        ir_array usage_lvl (lower_expr ctx size) (lower_expr ctx init)

  and lower_decl ctx =
    let venv, usage_lvl, break = ctx in
    let open Ast in
    function
    | FunctionDecl decls ->
        (* 1. Create [level]s for each function. *)
        List.iter
          (fun {fn_name; params; _} ->
            let lab = newlabel (name fn_name) in
            let escapes = List.map (fun {escape; _} -> !escape) params in
            let param_names = List.map (fun p -> name p.fld_name) params in
            let level = newlevel usage_lvl lab param_names escapes in
            Tbl.add venv fn_name (FunEntry (level, lab)) )
          decls;
        (* 2. Lower body of each function, add a fragment for it. *)
        List.iter
          (fun {fn_name; params; body; _} ->
            Tbl.scoped venv (fun venv ->
                let has_ret = is_val body in
                let level, _ = getfn venv fn_name in
                (* Add params to venv *)
                List.iter2
                  (fun {fld_name = p; _} (lvl, a) ->
                    Tbl.add venv p (VarEntry (lvl, a)) )
                  params (formals level);
                let body = lower_expr (venv, level, break) body in
                proc_entry_exit level body has_ret ) )
          decls;
        []
    | VarDecl {name; escape; init; _} as vd ->
        let lvl, access =
          alloc_local usage_lvl (Some (Front.Symbol.name name)) !escape
        in
        let var = ir_basic_var lvl access lvl in
        let init = lower_expr ctx init in
        Tbl.add venv name (VarEntry (lvl, access));
        [ir_assign var init (Ast.string_of_decl vd)]
    | TypeDecl _ -> []

  let lower expr =
    clear_frags ();
    let expr = Front.Desugar.expr_of_desugared expr in
    let mainlab = Temp.newlabel "_start" in
    let mainlvl = newlevel Toplevel mainlab [] [] in
    let main = lower_expr (base_venv (), mainlvl, None) expr in
    (* Add implicit return 0 if needed. *)
    let main =
      match is_val expr with
      | true -> main
      | false -> Ex (Ir.ESeq (unNx main, Ir.Const 0))
    in
    proc_entry_exit mainlvl main true;
    (mainlab, get_frags ())
end
