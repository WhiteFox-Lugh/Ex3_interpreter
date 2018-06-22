(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt | And | Or

type exp =
  | Var of id (* Var "x" --> x *)
  | ILit of int (* ILit 3 --> 3 *)
  | BLit of bool (* BLit true --> true *)
  | BinOp of binOp * exp * exp
  (* BinOp(Plus, ILit 4, Var "x") --> 4 + x *)
  | IfExp of exp * exp * exp
  (* IfExp(BinOp(Lt, Var "x", ILit 4), 
           ILit 3, 
           Var "x") --> 
     if x<4 then 3 else x *)
    (* ML2 interpreter *)
  | LetExp of id * exp * exp
    (* ML3 interpreter *)
  | FunExp of id * exp
  | AppExp of exp * exp
    (* ML4 interpreter *)
  | LetRecExp of id * id * exp * exp

type program = 
    Exp of exp
  | Decl of id * exp
  | RecDecl of id * id * exp

(* Static type inference *)
type tyvar = int

type ty =
    TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty

(* pretty printing *)
let fresh_tyvar =
  let counter = ref 0 in
  let body () =
      let v = !counter in
        counter := v + 1; v
    in body

let rec pp_ty = function
    TyInt -> print_string "int"
  | TyBool -> print_string "bool"
  | TyVar _ -> print_string (string_of_int (fresh_tyvar ()))
  | TyFun (ty1, ty2) ->
    print_string "(";
    pp_ty ty1;
    print_string ") -> ";
    pp_ty ty2
  ;;

let rec freevar_ty ty = 
  match ty with
    TyInt -> MySet.empty
  | TyBool -> MySet.empty
  | TyVar v -> MySet.singleton v
  | TyFun (t1, t2) -> MySet.union (freevar_ty t1) (freevar_ty t2)
  ;;
(* ty -> tyvar MySet.t *)
