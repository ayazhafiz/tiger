open Env
open Language
open Symbol
module P = Print
module Tbl = Symbol.Table
module Ty = Type

exception SemanticError of string

let ice why = raise (SemanticError why)

let scoped venv tenv go =
  Tbl.scoped venv (fun venv -> Tbl.scoped tenv (fun tenv -> go venv tenv))

let getid ?(hint = "") tbl what sym =
  match Tbl.find_opt tbl sym with
  | None ->
      let hint =
        if String.length hint == 0 then "" else Printf.sprintf " (%s)" hint
      in
      ice (Printf.sprintf "undeclared %s %s%s" what (name sym) hint)
  | Some v -> v

let simplifyty ty =
  let rec walk seen = function
    | Ty.Name (n, ity) -> (
      match !ity with
      | None -> ice ("Inconsistent state: type name " ^ name n ^ " not defined")
      | Some ty ->
          if List.mem n seen then
            ice ("unfolded cycle in declaration of type " ^ name n);
          walk (n :: seen) ty )
    | ty -> ty
  in
  walk [] ty

let tyeq t1 t2 =
  let t1 = simplifyty t1 in
  let t2 = simplifyty t2 in
  match (t1, t2) with
  | Ty.Int, Ty.Int | Ty.String, Ty.String | Ty.Nil, Ty.Nil | Ty.Unit, Ty.Unit ->
      true
  | Ty.Record (_, u1), Ty.Record (_, u2) | Ty.Array (_, u1), Ty.Array (_, u2) ->
      u1 == u2
  | Ty.Record _, Ty.Nil | Ty.Nil, Ty.Record _ -> true (* we'll allow it *)
  | Ty.Name (t1, _), Ty.Name (t2, _) ->
      ice
        (Printf.sprintf "Inconsistent state: type names %s, %s not resolved"
           (name t1) (name t2))
  | _ -> false

let getty ?(resolve = true) tbl sym =
  match
    ( resolve
    , getid tbl "type" sym
        ~hint:"if this a forward declaration, uses must be sequential" )
  with
  | false, ty -> ty
  | true, ty -> simplifyty ty

let expect_ty2 check t1 t2 msg =
  if not (check t1 t2) then
    ice
      (Printf.sprintf "mismatched types (%s vs %s): %s" (Ty.string_of_ty t1)
         (Ty.string_of_ty t2) msg)

let rec ck_var venv tenv = function
  | SimpleVar (v, realty) ->
      let ty =
        match getid venv "variable" v with
        | VarEntry ty -> ty
        | FunEntry _ -> ice "functions may not be treated as lvalues"
      in
      realty := Some ty;
      ty
  | FieldVar (v, f, realty) -> (
    match simplifyty (ck_var venv tenv v) with
    | Ty.Record (fields, _) -> (
      match List.assoc_opt f fields with
      | None -> ice ("no field " ^ name f)
      | Some ty ->
          realty := Some ty;
          ty )
    | _ ->
        ice
          (Printf.sprintf "field access \"%s\" must be on a record type"
             (name f)) )
  | SubscriptVar (v, idx, realty) -> (
    match simplifyty (ck_var venv tenv v) with
    | Ty.Array (elty, _) ->
        let elty = simplifyty elty in
        let tidx = ck_expr venv tenv idx in
        expect_ty2 tyeq Ty.Int tidx "index must be an int";
        realty := Some elty;
        elty
    | ty ->
        ice
          ( "subscript access must be on an array type, saw "
          ^ Ty.string_of_ty ty ) )

and ck_expr venv tenv = function
  | NilExpr -> Ty.Nil
  | VarExpr (v, realty) ->
      let ty = ck_var venv tenv v in
      realty := Some ty;
      ty
  | IntExpr _ -> Ty.Int
  | StringExpr _ -> Ty.String
  | CallExpr {func; args; ty = realty} -> (
      let ck_args = List.map (ck_expr venv tenv) args in
      match getid venv "function" func with
      | VarEntry _ -> ice "only functions may be called"
      | FunEntry (param_tys, out_ty) ->
          if List.length param_tys <> List.length ck_args then
            ice "number of arguments differs from formal parameters";
          List.iter2
            (fun ety rty ->
              expect_ty2 tyeq rty ety
                "argument type differs from formal parameter")
            param_tys ck_args;
          realty := Some out_ty;
          out_ty )
  | OpExpr {left; oper; right; ty = realty} ->
      let ty_l = ck_expr venv tenv left in
      let ty_r = ck_expr venv tenv right in
      let ty =
        match oper with
        | PlusOp | MinusOp | TimesOp | DivideOp ->
            expect_ty2 tyeq Ty.Int ty_l "arithmetic of incompatible types";
            expect_ty2 tyeq Ty.Int ty_r "arithmetic of incompatible types";
            Ty.Int
        | LtOp | LeOp | GtOp | GeOp ->
            expect_ty2 tyeq Ty.Int ty_l "comparison of incompatible types";
            expect_ty2 tyeq Ty.Int ty_r "comparison of incompatible types";
            Ty.Int
        | EqOp | NeqOp ->
            expect_ty2
              (fun t1 t2 ->
                match (t1, t2) with
                | Ty.Int, Ty.Int
                 |Ty.String, Ty.String
                 |Ty.Record _, Ty.Record _
                 |Ty.Record _, Ty.Nil
                 |Ty.Nil, Ty.Record _
                 |Ty.Nil, Ty.Nil
                 |Ty.Array _, Ty.Array _ ->
                    tyeq t1 t2
                | _ -> false)
              ty_l ty_r
              "arguments to comparison operators must be both ints, strings, \
               records, or arrays";
            Ty.Int
      in
      realty := Some ty;
      ty
  | RecordExpr {typ; fields; ty = realty} -> (
    match getty tenv typ with
    | Ty.Record (field_tys, _) as rcd ->
        if List.length fields <> List.length field_tys then
          ice "number of fields differs from record declaration";
        List.iter2
          (fun (fname, fty) (rname, rexpr) ->
            if name fname <> name rname then
              ice
                ( name rname ^ " is not a field of " ^ name typ
                ^ ", or is out of order" );
            let rty = ck_expr venv tenv rexpr in
            expect_ty2 tyeq fty rty
              (Printf.sprintf "field %s has incorrect\ntype" (name rname)))
          field_tys fields;
        realty := Some rcd;
        rcd
    | _ -> ice "records must be initialized from record type" )
  | SeqExpr (seq, realty) ->
      let ty =
        List.map (ck_expr venv tenv) seq
        |> List.rev
        |> function [] -> Ty.Unit | fty :: _ -> fty
      in
      realty := Some ty;
      ty
  | AssignExpr {var; expr} ->
      let tvar = ck_var venv tenv var in
      let texpr = ck_expr venv tenv expr in
      expect_ty2 tyeq texpr tvar
        "rhs of assignment does not match declared type";
      Ty.Unit
  | IfExpr {test; then'; else' = None; ty = realty} ->
      let test_ty = ck_expr venv tenv test in
      let then_ty = ck_expr venv tenv then' in
      if test_ty <> Ty.Int then
        ice (Printf.sprintf "test %s must be int" (P.string_of_expr test));
      if then_ty <> Ty.Unit then ice "if-then not unitary";
      realty := Some Ty.Unit;
      Ty.Unit
  | IfExpr {test; then'; else' = Some else'; ty = realty} as e ->
      let test_ty = ck_expr venv tenv test in
      let tthen = ck_expr venv tenv then' in
      let telse = ck_expr venv tenv else' in
      if test_ty <> Ty.Int then
        ice (Printf.sprintf "test %s must be int" (P.string_of_expr e));
      expect_ty2 tyeq tthen telse "branches of if expr differ";
      realty := Some tthen;
      tthen
  | WhileExpr {test; body} ->
      let ttest = ck_expr venv tenv test in
      let tbody = ck_expr venv tenv body in
      expect_ty2 tyeq Ty.Int ttest "test must be int";
      expect_ty2 tyeq Ty.Unit tbody "body of while not unit";
      Ty.Unit
  | ForExpr {var; lo; hi; body; _} ->
      let tlo = ck_expr venv tenv lo in
      let thi = ck_expr venv tenv hi in
      expect_ty2 tyeq Ty.Int tlo "lower bound must be an int";
      expect_ty2 tyeq Ty.Int thi "upper bound must be an int";
      scoped venv tenv (fun venv tenv ->
          Tbl.add venv var (VarEntry Ty.Int);
          let tbody = ck_expr venv tenv body in
          expect_ty2 tyeq Ty.Unit tbody "body must return no value");
      Ty.Unit
  | BreakExpr -> Ty.Unit
  | LetExpr {decls; body; ty = realty} ->
      scoped venv tenv (fun venv tenv ->
          List.iter (ck_decl venv tenv) decls;
          let tbody = ck_expr venv tenv body in
          realty := Some tbody;
          tbody)
  | ArrayExpr {typ; size; init; ty = realty} -> (
      expect_ty2 tyeq (ck_expr venv tenv size) Ty.Int "array size must be int";
      match getty tenv typ with
      | Ty.Array (elty, _) as arr_ty ->
          let tinit = ck_expr venv tenv init in
          expect_ty2 tyeq tinit elty
            "array initializer must match array element type";
          realty := Some arr_ty;
          arr_ty
      | ty ->
          ice
            ("can only create array from array type, saw " ^ Ty.string_of_ty ty)
      )

and ck_decl venv tenv = function
  | FunctionDecl decls ->
      (* 1. Introduce headers for mutually recursive definitions *)
      List.iter
        (fun {fn_name; params; result; _} ->
          let param_tys = List.map (fun {typ; _} -> getty tenv typ) params in
          let out_ty =
            match result with None -> Ty.Unit | Some typ -> getty tenv typ
          in
          Tbl.add venv fn_name (FunEntry (param_tys, out_ty)))
        decls;
      (* 2. Check bodies of each function *)
      List.iter
        (fun {params; result; body; _} ->
          scoped venv tenv (fun venv tenv ->
              List.iter
                (fun {fld_name = pname; typ; _} ->
                  let pty = getty tenv typ in
                  Tbl.add venv pname (VarEntry pty))
                params;
              let tbody = ck_expr venv tenv body in
              match result with
              | None ->
                  expect_ty2 tyeq tbody Ty.Unit
                    "procedure must not return a value"
              | Some typ ->
                  expect_ty2 tyeq (getty tenv typ) tbody
                    "type of body expression differs from declared return\ntype"))
        decls
  | VarDecl {name; typ = None; init; _} ->
      let tinit = ck_expr venv tenv init in
      expect_ty2 ( <> ) tinit Ty.Nil
        "nil-initialization must include a type annotation";
      Tbl.add venv name (VarEntry tinit)
  | VarDecl {name; typ = Some typ; init; _} ->
      let tinit = ck_expr venv tenv init in
      expect_ty2 tyeq tinit (getty tenv typ)
        "initializer does not match declared type";
      Tbl.add venv name (VarEntry tinit)
  | TypeDecl aliases ->
      (* 1. Introduce names for mutually recursive references *)
      let partial_defs = ref [] in
      List.iter
        (fun {name; _} ->
          let realty = ref None in
          Tbl.add tenv name (Ty.Name (name, realty));
          partial_defs := (name, realty) :: !partial_defs)
        aliases;
      (* 2. Compute real definitions of types *)
      List.iter
        (fun {name; ty} ->
          let realty = ck_ty ~resolve:false tenv ty in
          let realdef = List.assoc name !partial_defs in
          realdef := Some realty)
        aliases;
      (* 3. Perform cycle-busting. [getty ~resolve:true] will attempt to
            resolve all names and catch cycles. *)
      List.iter
        (fun {name; _} ->
          let _ = getty ~resolve:true tenv name in
          ())
        aliases

and ck_ty ?(resolve = true) tenv = function
  | NameTy typ -> getty ~resolve tenv typ
  | RecordTy fields ->
      let fields_tys =
        List.map
          (fun {fld_name; typ; _} -> (fld_name, getty ~resolve tenv typ))
          fields
      in
      Ty.Record (fields_tys, Ty.uniq ())
  | ArrayTy typ -> Ty.Array (getty ~resolve tenv typ, Ty.uniq ())

let check_prog expr =
  try
    let _ = ck_expr (base_venv ()) (base_tenv ()) expr in
    Ok ()
  with SemanticError what -> Error what
