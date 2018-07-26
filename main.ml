open Syntax
open Eval
open Typing

let rec read_eval_print env tyenv =
  print_string "# ";
  flush stdout;
  (* Exercise 3.2.2 *)
  let err_process message =
    Printf.printf "Error: %s " message;
    print_newline();
    read_eval_print env tyenv in
  (try 
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let (s, ty) = ty_decl tyenv decl in
    let (id, newenv, v) = eval_decl env decl in
    Printf.printf "val %s : " id;
    pp_ty ty;
    print_string " = ";
    pp_val v;
    print_newline();
    read_eval_print newenv tyenv
    with Eval.Error err_message -> err_process err_message
        | Parsing.Parse_error -> err_process "Parsing error"
        | Typing.Error err_message -> err_process err_message
        | err_message -> err_process (Printexc.to_string err_message)
    )

let initial_env = 
  Environment.extend "i" (IntV 1)
    (Environment.extend "v" (IntV 5) 
      (Environment.extend "x" (IntV 10)
      (* Exercise 3.2.1 *)
        (Environment.extend "ii" (IntV 2) 
          (Environment.extend "iii" (IntV 3) 
            (Environment.extend "iv" (IntV 4) Environment.empty)))))

(* for static type inference *)
let initial_tyenv =
  Environment.extend "i" TyInt
    (Environment.extend "ii" TyInt
      (Environment.extend "iii" TyInt
        (Environment.extend "iv" TyInt
          (Environment.extend "v" TyInt
            (Environment.extend "x" TyInt Environment.empty)))))

let _ = read_eval_print initial_env initial_tyenv
