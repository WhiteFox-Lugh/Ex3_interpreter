(* ML interpreter / type reconstruction *)
type id = string


type binOp = Plus | Mult | Lt | And | Or


type exp =
  Var of id (* Var "x" --> x *)
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
| MultiLetExp of id * exp * exp
  (* ML3 interpreter *)
| LetAndExp of id * exp * exp
| LetAndInExp of id * exp * exp
| FunExp of id * exp
| AppExp of exp * exp
| DFunExp of id * exp
  (* ML4 interpreter *)
| LetRecExp of id * id * exp * exp
  (* ML5 interpreter *)
| ConsExp of exp * exp
| MatchExp of id * id * exp * exp * exp
| EmptyConsList


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
  (* Exercise 4.3.7 *)
  | TyList of ty


(* pretty printing *)
let fresh_tyvar =
  let counter = ref 0 in
  let body () =
      let v = !counter in
        counter := v + 1; v
    in body


let pp_ty ty =
  (* 型変数が 1 -> 2 ではなく 'a -> 'b となるように処理 *)
  let var_id =
    (* 型変数のリスト *)
    let tyvar_list = ref [] in
      let gen_tyvar_id tyvar =
        let rec index l counter = match l with
          [] -> tyvar_list := List.append !tyvar_list [tyvar]; counter
        | x :: rest -> 
          if x = tyvar then counter else index rest (counter+1)
        in index !tyvar_list 0
      in gen_tyvar_id
    in
  (* int、bool、'a -> 'a のように表示する *)
  let rec get_string t = match t with
      TyInt -> "int"
    | TyBool -> "bool"
    | TyVar tyvar -> 
      let tyvar_id = var_id tyvar in
        (* tyvar_id が 25以下なら 'a, 'b, ... 'z *)
        if tyvar_id <= 25 then "'" ^ Char.escaped (char_of_int ((int_of_char 'a') + tyvar_id))
        (* 26個超過した時は 'a1, 'b1, ... 'z1, 'a2, ... と型変数を生成する *)
        else "'" ^ Char.escaped (char_of_int ((int_of_char 'a') + (tyvar_id mod 26))) ^ (string_of_int (tyvar_id / 26))
    | TyFun (ty1, ty2) ->
      let left = match ty1 with
      (* かっこを付けるべきか否かの分岐 *)
          TyFun (_, _) -> "(" ^ (get_string ty1) ^ ")"
        | _ -> get_string ty1
      in
      let right = get_string ty2 in left ^ " -> " ^ right 
    | TyList (ty) -> (get_string ty) ^ " list"
  in print_string (get_string ty)
  ;;


let rec freevar_ty ty = 
  match ty with
    TyInt -> MySet.empty
  | TyBool -> MySet.empty
  | TyVar v -> MySet.singleton v
  | TyFun (t1, t2) -> MySet.union (freevar_ty t1) (freevar_ty t2)
  | TyList t -> freevar_ty t
  ;;
(* ty -> tyvar MySet.t *)
