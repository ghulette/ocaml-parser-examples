%{
open Expr
%}

%token TRUE FALSE
%token NEG
%token CONJ DISJ IMPL EQUIV
%token FORALL EXISTS COLON
%token LPAREN RPAREN
%token EOF
%token <string> VAR

%start <Expr.t> main

%%

main : e=expr EOF        { e }

expr :
| TRUE                   { True }
| FALSE                  { False }
| x=VAR                  { Var x }
| NEG e=expr             { Neg e }
| e1=expr CONJ e2=expr   { Conj (e1, e2) }
| e1=expr DISJ e2=expr   { Disj (e1, e2) }
| e1=expr IMPL e2=expr   { Impl (e1, e2) }
| e1=expr EQUIV e2=expr  { Equiv (e1, e2) }
| LPAREN e=expr RPAREN   { e }

%%
