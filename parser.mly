%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT
%token IF THEN ELSE TRUE FALSE
(* ML2 interpreter *)
%token LET IN EQ LAND LOR AND
(* ML3 interpreter *)
%token RARROW FUN DFUN
%token PREPLUS PREMULT PRELT PRELAND PRELOR
(* ML4 interpreter *)
%token REC
(* ML5 interpretr *)
%token LBRK RBRK VBAR CONS MATCH WITH

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | LET x=ID EQ e=Expr SEMISEMI { Decl (x, e) }
  (* | LET x=ID EQ e1=Expr e2=MultiLetExpr { MultiDecl (x, e1, e2) } *)
  | LET x=ID e=LetFunSimple SEMISEMI { Decl (x, e) }
  | LET REC x1=ID EQ FUN x2=ID RARROW e=Expr SEMISEMI { RecDecl (x1, x2, e) }
  (* | LET x=ID EQ e1=Expr AND e2=LetAndExpr { MultiAndDecl (x, e1, e2) } *)

Expr :
    e=IfExpr { e }
  | e=LetRecExpr { e }
  | e=LetExpr { e }
  | e=OrExpr { e }
  | e=FunExpr { e }
  (* | LET e=LetAndInExpr { e } *)
  | e=ConsExpr { e }
  | e=MatchExpr { e }

(* ML2 interpreter "Let" expression *)
LetExpr :
    LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) }
  | LET x=ID e1=LetFunSimple IN e2=Expr { LetExp (x, e1, e2) }
 
(* LetAndExpr :
    x=ID EQ e1=Expr AND e2=LetAndExpr { MultiAndDecl (x, e1, e2) }
  | x=ID EQ e=Expr SEMISEMI { Decl(x, e) }

LetAndInExpr :
    x=ID EQ e1=Expr AND e2=LetAndInExpr { LetAndInExp (x, e1, e2) }
  | x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) }
  *)

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
  | PRELT l=AExpr r=AExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

(* Exercise 3.6.1 *)
ConsExpr :
    LBRK RBRK { EmptyConsList }
  | LBRK RBRK CONS e2=Expr { ConsExp (EmptyConsList, e2) }
  | e1=PExpr CONS e2=Expr { ConsExp (e1, e2) }

MatchExpr :
    MATCH e1=Expr WITH LBRK RBRK RARROW e2=Expr VBAR x1=ID CONS x2=ID RARROW e3=Expr { MatchExp (x1, x2, e1, e2, e3) }

PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | PREPLUS l=AExpr r=AExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

MExpr :
    e1=MExpr MULT e2=AppExpr { BinOp (Mult, e1, e2) }
  | PREMULT l=AExpr r=AExpr { BinOp (Mult, l, r) }
  | e=AppExpr { e }

(* ML3 interpreter *)
AppExpr :
    e1=AppExpr e2=AExpr { AppExp (e1, e2) }
  | e=AExpr { e }

FunExpr :
    DFUN x=ID RARROW e=Expr { DFunExp (x, e) }
  | FUN x=ID RARROW e=Expr { FunExp (x, e) }
  | FUN x=ID e=FunSimple { FunExp (x, e) }


(* Optional Exercise *)
(* Exercise 3.3.2 *)
(* MultiLetExpr :
    LET x=ID EQ e1=Expr e2=MultiLetExpr { MultiDecl (x, e1, e2) }
  | LET x=ID EQ e=Expr SEMISEMI { Decl (x, e) } *)

(* Exercise 3.4.3 *)
FunSimple :
    x=ID RARROW e=Expr { FunExp (x, e) }
  | x=ID e=FunSimple { FunExp (x, e) }

LetFunSimple :
    x=ID EQ e=Expr { FunExp (x, e) }
  | x=ID e=LetFunSimple { FunExp (x, e) }


AExpr :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | LBRK RBRK { EmptyConsList }
  | i=ID   { Var i }
  | PREPLUS { FunExp ("a", FunExp("b", BinOp (Plus, Var "a", Var "b"))) }
  | PREMULT { FunExp ("a", FunExp("b", BinOp (Mult, Var "a", Var "b"))) }
  | PRELT { FunExp ("a", FunExp("b", BinOp (Lt, Var "a", Var "b"))) }
  | PRELAND { FunExp ("a", FunExp("b", BinOp (And, Var "a", Var "b"))) }
  | PRELOR { FunExp ("a", FunExp("b", BinOp (Or, Var "a", Var "b"))) }
  | LPAREN e=Expr RPAREN { e }

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

