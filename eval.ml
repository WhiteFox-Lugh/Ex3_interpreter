open Syntax 

(* 値の定義 *)

(* exval は式を評価して得られる値．dnval は変数と紐付けられる値．今回
   の言語ではこの両者は同じになるが，この2つが異なる言語もある．教科書
   参照． *)
type exval =
  | IntV of int
  | BoolV of bool
  | ProcV of id * exp * dnval Environment.t ref
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ProcV (_, _, _) -> "< (`･ω･´)つ<fun> >"

let pp_val v = print_string (string_of_exval v)

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")
  | And, BoolV b1, BoolV b2 -> if b1 = true then BoolV(b2) else BoolV(false)
  | And, BoolV b1, _ -> if b1 = true then err ("syobon") else BoolV(false)
  | And, _, _ -> err ("Both arguments must be boolean: &&")
  | Or, BoolV b1, BoolV b2 -> if b1 = false then BoolV(b2) else BoolV(true)
  | Or, BoolV b1, _ -> if b1 = false then err ("baka") else BoolV(true)
  | Or, _, _ -> err ("Both arguments must be boolean: ||")

let rec eval_exp env = function
    Var x -> 
      (try Environment.lookup x env with 
        Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | BinOp (op, exp1, exp2) -> 
      let arg1 = eval_exp env exp1 in
      let arg2 = eval_exp env exp2 in
      apply_prim op arg1 arg2
  | IfExp (exp1, exp2, exp3) ->
      let test = eval_exp env exp1 in
        (match test with
            BoolV true -> eval_exp env exp2 
          | BoolV false -> eval_exp env exp3
          | _ -> err ("Test expression must be boolean: if"))
  (* ML2 interpreter *)
  | LetExp (id, exp1, exp2) ->
    (* 現在の環境で exp1 を評価 *)
    let value = eval_exp env exp1 in
    (* exp1 の評価結果を id の値として環境に追加して exp2 を評価 *)
    eval_exp (Environment.extend id value env) exp2
  (* ML3 interpreter *)
  (* 現在の環境 env をクロージャ内に保存 *)
  | FunExp (id, exp) -> ProcV (id, exp, ref env)
  | AppExp (exp1, exp2) ->
      let funval = eval_exp env exp1 in
      let arg = eval_exp env exp2 in
        (match funval with
          ProcV (id, body, env') ->
            (* クロージャ内の環境を取り出して仮引数に対する束縛で拡張 *)
            let newenv = Environment.extend id arg !env' in
              eval_exp newenv body
        | _ -> err ("Non-function value is applied")
        )
  | LetRecExp (id, para, exp1, exp2) ->
    (* ダミーの環境への参照を作る *)
    let dummyenv = ref Environment.empty in
    (* 関数閉包を作り、idをこの関数閉包に写像するように現在の環境 env を拡張 *)
    let newenv = 
      Environment.extend id (ProcV (para, exp1, dummyenv)) env in
      (* ダミーへの環境への参照に、拡張された環境を破壊的代入してバックパッチ *)
      dummyenv := newenv;
      eval_exp newenv exp2


let eval_decl env = function
    Exp e -> let v = eval_exp env e in ("-", env, v)
  | Decl (id, e) ->
      let v = eval_exp env e in (id, Environment.extend id v env, v)
  | RecDecl (id, para, e) ->
      let dummy = ref Environment.empty in
        let new_env = Environment.extend id (ProcV (para, e, dummy)) env in
        dummy := new_env;
        (id, new_env, ProcV(para, e, dummy))