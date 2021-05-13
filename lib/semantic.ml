open Language
open Env
open Symbol
module P = Print
module Tbl = Symbol.Table
module T = Translate
module Ty = Type

type expr_ty = T.expr * Ty.ty

let tyof (_, t) = t

exception SemanticError of string

let err why = raise (SemanticError why)

let scoped venv tenv go =
  Tbl.enter venv;
  Tbl.enter tenv;
  let res = go venv tenv in
  Tbl.exit venv;
  Tbl.exit tenv;
  res

let getid ?(hint = "") tbl what sym =
  match Tbl.find tbl sym with
  | None ->
      let hint =
        if String.length hint == 0 then "" else Printf.sprintf " (%s)" hint
      in
      err (Printf.sprintf "undeclared %s %s%s" what (name sym) hint)
  | Some v -> v

let simplifyty ty =
  let rec walk seen = function
    | Ty.Name (n, ity) -> (
        match !ity with
        | None ->
            err ("Inconsistent state: type name " ^ name n ^ " not defined")
        | Some ty ->
            if List.mem n seen then
              err ("unfolded cycle in declaration of type " ^ name n);
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
      err
        (Printf.sprintf "Inconsistent state: type names %s, %s not resolved"
           (name t1) (name t2))
  | _ -> false

let getty ?(resolve = true) tbl sym =
  match
    ( resolve,
      getid tbl "type" sym
        ~hint:"if this a forward declaration, uses must be sequential" )
  with
  | false, ty -> ty
  | true, ty -> simplifyty ty

let expect_ty2 check t1 t2 msg =
  if not (check t1 t2) then
    err
      (Printf.sprintf "mismatched types (%s vs %s): %s" (Ty.string_of_ty t1)
         (Ty.string_of_ty t2) msg)

let rec tr_var venv tenv = function
  | SimpleVar v ->
      let ty =
        match getid venv "variable" v with
        | VarEntry ty -> ty
        | FunEntry _ -> err "functions may not be treated as lvalues"
      in
      ((), ty)
  | FieldVar (v, f) -> (
      match simplifyty (tyof (tr_var venv tenv v)) with
      | Ty.Record (fields, _) -> (
          match List.assoc_opt f fields with
          | None -> err ("no field " ^ name f)
          | Some ty -> ((), ty) )
      | _ ->
          err
            (Printf.sprintf "field access \"%s\" must be on a record type"
               (name f)) )
  | SubscriptVar (v, idx) -> (
      match simplifyty (tyof (tr_var venv tenv v)) with
      | Ty.Array (elty, _) ->
          let tidx = tr_expr venv tenv idx in
          expect_ty2 tyeq Ty.Int (tyof tidx) "index must be an int";
          ((), elty)
      | ty ->
          err
            ( "subscript access must be on an array type, saw "
            ^ Ty.string_of_ty ty ) )

and tr_expr venv tenv = function
  | NilExpr -> ((), Ty.Nil)
  | VarExpr v -> tr_var venv tenv v
  | IntExpr _ -> ((), Ty.Int)
  | StringExpr _ -> ((), Ty.String)
  | CallExpr { func; args } -> (
      let tr_args = List.map (tr_expr venv tenv) args in
      match getid venv "function" func with
      | VarEntry _ -> err "only functions may be called"
      | FunEntry (param_tys, out_ty) ->
          if List.length param_tys <> List.length tr_args then
            err "number of arguments differs from formal parameters";
          List.iter2
            (fun ety (_, rty) ->
              expect_ty2 tyeq rty ety
                "argument type differs from formal parameter")
            param_tys tr_args;
          ((), out_ty) )
  | OpExpr { left; oper; right } -> (
      let tr_l = tr_expr venv tenv left in
      let tr_r = tr_expr venv tenv right in
      match oper with
      | PlusOp | MinusOp | TimesOp | DivideOp ->
          expect_ty2 tyeq Ty.Int (tyof tr_l) "arithmetic of incompatible types";
          expect_ty2 tyeq Ty.Int (tyof tr_r) "arithmetic of incompatible types";
          ((), Ty.Int)
      | LtOp | LeOp | GtOp | GeOp ->
          expect_ty2 tyeq Ty.Int (tyof tr_l) "comparison of incompatible types";
          expect_ty2 tyeq Ty.Int (tyof tr_r) "comparison of incompatible types";
          ((), Ty.Int)
      | EqOp | NeqOp ->
          expect_ty2
            (fun t1 t2 ->
              match (t1, t2) with
              | Ty.Int, Ty.Int
              | Ty.String, Ty.String
              | Ty.Record _, Ty.Record _
              | Ty.Record _, Ty.Nil
              | Ty.Nil, Ty.Record _
              | Ty.Nil, Ty.Nil
              | Ty.Array _, Ty.Array _ ->
                  tyeq t1 t2
              | _ -> false)
            (tyof tr_l) (tyof tr_r)
            "arguments to comparison operators must be both ints, strings, \
             records, or arrays";
          ((), Ty.Int) )
  | RecordExpr { typ; fields } -> (
      match getty tenv typ with
      | Ty.Record (field_tys, _) as rcd ->
          if List.length fields <> List.length field_tys then
            err "number of fields differs from record declaration";
          List.iter2
            (fun (fname, fty) (rname, rexpr) ->
              if name fname <> name rname then
                err
                  ( name rname ^ " is not a field of " ^ name typ
                  ^ ", or is out of order" );
              let _, rty = tr_expr venv tenv rexpr in
              expect_ty2 tyeq fty rty
                (Printf.sprintf "field %s has incorrect\ntype" (name rname)))
            field_tys fields;
          ((), rcd)
      | _ -> err "records must be initialized from record type" )
  | SeqExpr seq -> (
      List.map (tr_expr venv tenv) seq |> List.rev |> function
      | [] -> ((), Ty.Unit)
      | (_, fty) :: _ -> ((), fty) )
  | AssignExpr { var; expr } ->
      let tvar = tr_var venv tenv var in
      let texpr = tr_expr venv tenv expr in
      expect_ty2 tyeq (tyof texpr) (tyof tvar)
        "rhs of assignment does not match declared type";
      ((), Ty.Unit)
  | IfExpr { test; then'; else' = None } ->
      let _, test_ty = tr_expr venv tenv test in
      let _, then_ty = tr_expr venv tenv then' in
      if test_ty <> Ty.Int then
        err (Printf.sprintf "test %s must be int" (P.string_of_expr test));
      if then_ty <> Ty.Unit then err "if-then not unitary";
      ((), Ty.Unit)
  | IfExpr { test; then'; else' = Some else' } as e ->
      let _, test_ty = tr_expr venv tenv test in
      let tthen = tr_expr venv tenv then' in
      let telse = tr_expr venv tenv else' in
      if test_ty <> Ty.Int then
        err (Printf.sprintf "test %s must be int" (P.string_of_expr e));
      expect_ty2 tyeq (tyof tthen) (tyof telse) "branches of if expr differ";
      ((), tyof tthen)
  | WhileExpr { test; body } ->
      let ttest = tr_expr venv tenv test in
      let tbody = tr_expr venv tenv body in
      expect_ty2 tyeq Ty.Int (tyof ttest) "test must be int";
      expect_ty2 tyeq Ty.Unit (tyof tbody) "body of while not unit";
      ((), Ty.Unit)
  | ForExpr { var; lo; hi; body; _ } ->
      let tlo = tr_expr venv tenv lo in
      let thi = tr_expr venv tenv hi in
      expect_ty2 tyeq Ty.Int (tyof tlo) "lower bound must be an int";
      expect_ty2 tyeq Ty.Int (tyof thi) "upper bound must be an int";
      scoped venv tenv (fun venv tenv ->
          Tbl.add venv var (VarEntry Ty.Int);
          let tbody = tr_expr venv tenv body in
          expect_ty2 tyeq Ty.Unit (tyof tbody) "body must return no value");
      ((), Ty.Unit)
  | BreakExpr -> ((), Ty.Unit)
  | LetExpr { decls; body } ->
      scoped venv tenv (fun venv tenv ->
          List.iter (tr_decl venv tenv) decls;
          let tbody = tr_expr venv tenv body in
          tbody)
  | ArrayExpr { typ; init; _ } -> (
      match getty tenv typ with
      | Ty.Array (elty, _) as arr_ty ->
          let tinit = tr_expr venv tenv init in
          expect_ty2 tyeq (tyof tinit) elty
            "array initializer must match array element type";
          ((), arr_ty)
      | ty ->
          err
            ("can only create array from array type, saw " ^ Ty.string_of_ty ty)
      )

and tr_decl venv tenv = function
  | FunctionDecl decls ->
      (* 1. Introduce headers for mutually recursive definitions *)
      List.iter
        (fun { fn_name; params; result; _ } ->
          let param_tys = List.map (fun { typ; _ } -> getty tenv typ) params in
          let out_ty =
            match result with None -> Ty.Unit | Some typ -> getty tenv typ
          in
          Tbl.add venv fn_name (FunEntry (param_tys, out_ty)))
        decls;
      (* 2. Check bodies of each function *)
      List.iter
        (fun { params; result; body; _ } ->
          scoped venv tenv (fun venv tenv ->
              List.iter
                (fun { fld_name = pname; typ; _ } ->
                  let pty = getty tenv typ in
                  Tbl.add venv pname (VarEntry pty))
                params;
              let tbody = tr_expr venv tenv body in
              match result with
              | None ->
                  expect_ty2 tyeq (tyof tbody) Ty.Unit
                    "procedure must not return a value"
              | Some typ ->
                  expect_ty2 tyeq (getty tenv typ) (tyof tbody)
                    "type of body expression differs from declared return\ntype"))
        decls
  | VarDecl { name; typ = None; init; _ } ->
      let tinit = tr_expr venv tenv init in
      expect_ty2 ( <> ) (tyof tinit) Ty.Nil
        "nil-initialization must include a type annotation";
      Tbl.add venv name (VarEntry (tyof tinit))
  | VarDecl { name; typ = Some typ; init; _ } ->
      let tinit = tr_expr venv tenv init in
      expect_ty2 tyeq (tyof tinit) (getty tenv typ)
        "initializer does not match declared type";
      Tbl.add venv name (VarEntry (tyof tinit))
  | TypeDecl aliases ->
      (* 1. Introduce names for mutually recursive references *)
      let partial_defs = ref [] in
      List.iter
        (fun { name; _ } ->
          let realty = ref None in
          Tbl.add tenv name (Ty.Name (name, realty));
          partial_defs := (name, realty) :: !partial_defs)
        aliases;
      (* 2. Compute real definitions of types *)
      List.iter
        (fun { name; ty } ->
          let realty = tr_ty ~resolve:false tenv ty in
          let realdef = List.assoc name !partial_defs in
          realdef := Some realty)
        aliases;
      (* 3. Perform cycle-busting. [getty ~resolve:true] will attempt to
            resolve all names and catch cycles. *)
      List.iter
        (fun { name; _ } ->
          let _ = getty ~resolve:true tenv name in
          ())
        aliases

and tr_ty ?(resolve = true) tenv = function
  | NameTy typ -> getty ~resolve tenv typ
  | RecordTy fields ->
      let fields_tys =
        List.map
          (fun { fld_name; typ; _ } -> (fld_name, getty ~resolve tenv typ))
          fields
      in
      Ty.Record (fields_tys, Ty.uniq ())
  | ArrayTy typ -> Ty.Array (getty ~resolve tenv typ, Ty.uniq ())

let tr_prog expr = tr_expr (base_venv ()) (base_tenv ()) expr
