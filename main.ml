open Syntax
open Eval

let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  (* Exercise 3.2.2 *)
  let err_process message =
    Printf.printf "Error: %s " message;
    print_newline();
    read_eval_print env in
  (try 
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let (id, newenv, v) = eval_decl env decl in
    Printf.printf "val %s = " id;
    pp_val v;
    print_newline();
    read_eval_print newenv
    with Eval.Error err_message -> err_process err_message
    | Parsing.Parse_error -> err_process "Parsing error"
    | _ -> err_process "(´･ω･`)")


(* batch interpreter *)
let read_file_eval_print env =
  let file = Sys.argv.(1) in
  let open_file = open_in file in
  (try 
    let rec decl = Parser.toplevel Lexer.main (Lexing.from_channel open_file) in
    let (id, newenv, v) = eval_decl env decl in
    flush stdout;
    Printf.printf "val %s = " id;
    pp_val v;
    print_newline();
    exit 0
  with End_of_file ->
        close_in_noerr open_file;
        raise End_of_file
      | e ->
        close_in_noerr open_file;
        raise e)



let initial_env = 
  Environment.extend "i" (IntV 1)
    (Environment.extend "v" (IntV 5) 
      (Environment.extend "x" (IntV 10)
      (* Exercise 3.2.1 *)
        (Environment.extend "ii" (IntV 2) 
          (Environment.extend "iii" (IntV 3) 
            (Environment.extend "iv" (IntV 4) Environment.empty)))))

let _ = 
  if (Array.length Sys.argv) = 1 then read_eval_print initial_env
  else if (Array.length Sys.argv) = 2 then read_file_eval_print initial_env
  else print_string "Error: the number of arguments must be less than two.\n"