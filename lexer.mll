{
let reservedWords = [
  (* Keywords *)
  ("else", Parser.ELSE);
  ("false", Parser.FALSE);
  ("if", Parser.IF);
  ("then", Parser.THEN);
  ("true", Parser.TRUE);
  (* ML2 interpreter *)
  ("let", Parser.LET);
  ("in", Parser.IN);
  (* ML3 interpreter *)
  ("fun", Parser.FUN);
] 
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| ";;" { Parser.SEMISEMI }
| "+" { Parser.PLUS }
| "*" { Parser.MULT }
| "<" { Parser.LT }
| "(*" { comment 0 lexbuf }
(* ML2 interpreter *)
| "=" { Parser.EQ }
(* ML3 interpreter *)
| "->" { Parser.RARROW }

| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try 
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
     }
| eof { exit 0 }

(* Exercise 3.2.4 *)

and comment nest_times = parse
  "(*" {comment (nest_times+1) lexbuf}
| "*)" {if nest_times>0 then (comment (nest_times-1) lexbuf) else (main lexbuf)}
| _ {comment nest_times lexbuf}
