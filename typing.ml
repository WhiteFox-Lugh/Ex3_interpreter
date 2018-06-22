open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t

let ty_prim op ty1 ty2 = match op with
    Plus -> (match ty1, ty2 with
              TyInt, TyInt -> TyInt
            | _ -> err ("Argument must be of integer: +"))
  | Mult -> (match ty1, ty2 with
              TyInt, TyInt -> TyInt
            | _ -> err ("Argument must be of integer: *"))
  | Lt -> (match ty1, ty2 with
              TyInt, TyInt -> TyBool
            | _ -> err ("Argument must be of integer: <"))
  | _ -> err "Not Implemented(´･ω･`)"

let rec ty_exp tyenv = function
  Var x ->
    (try Environment.lookup x tyenv with
      Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> TyInt
  | BLit _ -> TyBool
  | BinOp (op, exp1, exp2) ->
      let tyarg1 = ty_exp tyenv exp1 in
      let tyarg2 = ty_exp tyenv exp2 in
        ty_prim op tyarg1 tyarg2
  | IfExp (exp1, exp2, exp3) ->
      let tyarg1 = ty_exp tyenv exp1 in
      let tyarg2 = ty_exp tyenv exp2 in
      let tyarg3 = ty_exp tyenv exp3 in
        (match tyarg1 with
          TyBool -> if tyarg2 = tyarg3 then tyarg2 else err ("unmatched type")
        | _ -> err ("condition in if expression must have the tyoe: bool")
        )
  | LetExp (id, exp1, exp2) ->
      let tyarg1 = ty_exp tyenv exp1 in
        ty_exp (Environment.extend id tyarg1 tyenv) exp2
  | _ -> err ("Not Implemented(´･ω･`)(´･ω･`)")

let ty_decl tyenv = function
    Exp e -> ty_exp tyenv e
  | Decl (id, e) -> ty_exp tyenv e
  | _ -> err ("Not Implemented(´･ω･`)(´･ω･`)(´･ω･`)")

type subst = (tyvar * ty) list

let rec subst_type l t =
  match t with
    TyInt -> TyInt
  | TyBool -> TyBool
  | TyFun (arg1, arg2) -> TyFun ((subst_type l arg1), (subst_type l arg2))
  | TyVar v -> (try subst_type l (List.assoc v l) with Not_found -> TyVar v)
  ;;