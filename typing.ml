open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t

type subst = (tyvar * ty) list


let rec subst_type subst t =
  match t with
    TyInt -> TyInt
  | TyBool -> TyBool
  | TyFun (arg1, arg2) -> TyFun ((subst_type subst arg1), (subst_type subst arg2))
  | TyVar v -> (try subst_type subst (List.assoc v subst) with Not_found -> TyVar v)
  ;;

  
(* eqs_of_subst : subst -> (ty * ty) list
    型代入を型の等式集合に変換  *)
let rec eqs_of_subst = function
    [] -> []
  | (tyvar, ty) :: rest -> (TyVar tyvar, ty) :: (eqs_of_subst rest)
  ;;
  
  
(* subst_eqs: subst -> (ty * ty) list -> (ty * ty) list
  型の等式集合に型代入を適用 *)
let rec subst_eqs s = function
    [] -> []
  | (t1, t2) :: rest -> (subst_type s t1, subst_type s t2) :: (subst_eqs s rest)


(* 単一化 *)
let rec unify l =
  match l with
    (* u(phi) = phi *)
    [ ] -> [ ]
  | (x :: rest) ->
    match x with
      (* u( {(tau, tau)} union+ X') = u(X') *)
      (TyInt, TyInt) -> unify rest
    | (TyBool, TyBool) -> unify rest
    | (TyVar alpha, TyVar beta) when alpha = beta -> unify rest
      (* u({(tau11->tau12, tau21->tau22)} union+ X') = u({(tau11, tau21),(tau12, tau22)} union+ X') *)
    | (TyFun (a1, a2), TyFun (b1, b2)) -> unify ((a1, b1) :: ((a2, b2) :: rest))
      (* u({(alpha, tau)} union+ X') or
         u({(tau, alpha)} union+ X') *)
    | (TyVar var1, TyVar var2) -> [(var1, TyVar var2)] @ unify (subst_eqs [(var1, TyVar var2)] rest)
    | (TyVar var, ty) | (ty, TyVar var) -> 
      (* if tau include alpha *)
      if MySet.member var (Syntax.freevar_ty ty) then err ("Can't unify(´･ω･`)")
      (* u({(alpha, tau)} union+ X') or
         u({(tau, alpha)} union+ X') *)
      else [(var, ty)] @ unify (subst_eqs [(var, ty)] rest)
    | (_, _) -> err ("Can't unify(´･_･ `)")
    ;;


let ty_prim op ty1 ty2 = match op with
    Plus | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
  | _ -> err "Not Implemented(´･ω･`)"


let rec ty_exp tyenv = function
  Var x ->
    (try ([], Environment.lookup x tyenv) with
      Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> ([], TyInt)
  | BLit _ -> ([], TyBool)
  | BinOp (op, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (eqs3, ty) = ty_prim op ty1 ty2 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
      let s3 = unify eqs in (s3, subst_type s3 ty)
  | IfExp (exp1, exp2, exp3) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (s3, ty3) = ty_exp tyenv exp3 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3) @ 
        [(ty1, TyBool)] @ [(ty2, ty3)] in
      let s4 = unify eqs in (s4, subst_type s4 ty3)
  | LetExp (id, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp (Environment.extend id ty1 tyenv) exp2 in
      let domty = TyVar (fresh_tyvar ()) in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ [(domty, ty1)] in
      let s3 = unify eqs in
      (s3, subst_type s3 ty2)
  | FunExp (id, exp) ->
      let domty = TyVar (fresh_tyvar ()) in
      let (s, ranty) = ty_exp (Environment.extend id domty tyenv) exp in
      (s, TyFun (subst_type s domty, ranty))
  | AppExp (exp1, exp2) -> 
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let domty = TyVar (fresh_tyvar ()) in
      let eqs = (ty1, TyFun(ty2, domty)) :: (eqs_of_subst s1) @ (eqs_of_subst s2) in
      let s3 = unify eqs in
      (s3, subst_type s3 domty)
  (* Exercise 4.3.6 *)
  | LetRecExp (id1, id2, exp1, exp2) ->
      let dom_ty1 = TyVar (fresh_tyvar ()) in
      let dom_ty2 = TyVar (fresh_tyvar ()) in
      let tyenv_f = Environment.extend id1 dom_ty1 tyenv in
      let tyenv_f_x = Environment.extend id2 dom_ty2 tyenv_f in
      let (s1, ran_ty1) = ty_exp tyenv_f_x exp1 in
      let (s2, ran_ty2) = ty_exp tyenv_f exp2 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) in
      let s3 = unify eqs in
      (s3, subst_type s3 dom_ty2) 
      
  (*| _ -> err ("Not Implemented(´･ω･`)(´･ω･`)")*)


let ty_decl tyenv = function
    Exp e -> ty_exp tyenv e
  | Decl (id, e) -> ty_exp tyenv e
  | RecDecl (_, _, e) -> ty_exp tyenv e