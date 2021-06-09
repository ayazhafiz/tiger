open Front
open Util.Sanitize
module L = Llvm

let ice why = failwith ("ICE (llvm lower): " ^ why)

type level = Toplevel | Nest of {parent : level; uuid : int}

let newlevel =
  let uuid = ref 0 in
  fun parent ->
    incr uuid;
    Nest {parent; uuid = !uuid}

let top_type ctx = L.pointer_type (L.i8_type ctx)

let llvm_type ctx =
  let open Type in
  let rec tyof = function
    | Int -> L.i64_type ctx
    | String -> L.pointer_type (L.i8_type ctx)
    (* Our arrays and records are passed around as pointer to the real
       array/record, hence the following translation. *)
    | Record (fields, _) ->
        L.pointer_type
          (L.struct_type ctx
             (List.map (fun (_, ty) -> tyof ty) fields |> Array.of_list) )
    | Array (ty, _) -> L.pointer_type (L.pointer_type (tyof ty))
    (* TODO: Can be smarter about the targeted type here? *)
    | Nil -> top_type ctx
    | Unit -> L.void_type ctx
    | Name (_, ty) as t -> (
      match !ty with
      | Some t -> tyof t
      | None -> ice (string_of_ty t ^ " not expanded") )
  in
  tyof

let lower_op (ctx,b) op left right =
  let arith op =  op left right "" b in
  let cond op =
    let r = L.build_icmp op left right "" b in
    L.build_zext r (L.i64_type ctx) "" b in
  let open Language in
  match op with 
  | PlusOp -> arith L.build_add
  | MinusOp -> arith L.build_sub
  | TimesOp ->arith L.build_mul
  | DivideOp -> arith L.build_sdiv
  | EqOp -> cond L.Icmp.Eq
  | NeqOp -> cond L.Icmp.Ne
  | LtOp -> cond L.Icmp.Slt
  | LeOp -> cond L.Icmp.Sle
  | GtOp -> cond L.Icmp.Sgt
  | GeOp -> cond L.Icmp.Sge

let rec lower_var w =
  let (main_level, ctx, m, b, break) = w in
  let open Language in
  function
    | _ -> ice "todo"

and lower_expr w =
  let (main_level, ctx, m, b, break) = w in
  let open Language in
  function
    | NilExpr -> L.const_int (L.i64_type ctx) 0
    | VarExpr _ -> ice "todo"
    | IntExpr n -> L.const_int (L.i64_type ctx) n
    | StringExpr s -> L.build_global_stringptr s (sanitize_string_for_label s) b
    | CallExpr _  ->   ice "todo"
    | OpExpr {left; oper; right; _} ->
        lower_op (ctx, b) oper (lower_expr w left) (lower_expr w right)
    | RecordExpr _ -> ice "todo"
    | SeqExpr _ -> ice "todo"
    | AssignExpr {var; expr} ->
        L.build_store (lower_expr w expr) (lower_var w var) b
    | IfExpr _ -> ice "todo"
    | WhileExpr _ -> ice "todo"
    | ForExpr _ -> ice "todo"
    | BreakExpr  -> L.build_br (Option.get break) b
    | LetExpr _ -> ice "todo"
    | ArrayExpr _ -> ice "todo"

let lower program_name expr =
  let ctx = L.global_context () in
  let modul = L.create_module ctx program_name in
  let builder = L.builder ctx in
  let main =
    L.declare_function "_start" (L.function_type (L.i64_type ctx) [||]) modul
  in
  let main_block = L.append_block ctx "entry" main in
  L.position_at_end main_block builder;
  let main_level = newlevel Toplevel in
  let _ = lower_expr (main_level, ctx, modul, builder, None) expr;
  L.string_of_llmodule modul 
