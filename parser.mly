%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT
%token IF THEN ELSE TRUE FALSE
(* ML2 interpreter *)
%token LET IN EQ LAND LOR
(* ML3 interpreter *)
%token RARROW FUN
(* ML4 interpreter *)
%token REC

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | LET x=ID EQ e=Expr SEMISEMI { Decl (x, e) }
  | LET REC x1=ID EQ FUN x2=ID RARROW e=Expr SEMISEMI { RecDecl (x1, x2, e) }
  (* Exercise 3.3.2 *)

Expr :
    e=IfExpr { e }
  | e=LetRecExpr { e }
  | e=LetExpr { e }
  | e=OrExpr { e }
  | e=FunExpr { e }

(* ML2 interpreter "Let" expression *)
LetExpr :
    LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) }

(* ML4 interpreter "Let rec" expression *)
LetRecExpr :
    LET REC x1=ID EQ FUN x2=ID RARROW e1=Expr IN e2=Expr { LetRecExp (x1, x2, e1, e2) }

OrExpr :
    l=OrExpr LOR r=AndExpr { BinOp (Or, l, r) }
  | e=AndExpr { e }

AndExpr :
    l=AndExpr LAND r=LTExpr { BinOp (And, l, r) }
  | e=LTExpr { e }

LTExpr : 
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

MExpr :
    e1=MExpr MULT e2=AppExpr { BinOp (Mult, e1, e2) }
  | e=AppExpr { e }

(* ML3 interpreter *)
AppExpr :
    e1=AppExpr e2=AExpr { AppExp (e1, e2) }
  | e=AExpr { e }

FunExpr :
    FUN x=ID RARROW e=Expr { FunExp (x, e) }

AExpr :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }
