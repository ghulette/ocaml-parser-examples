%{
open Expr_ast
%}

%token TRUE FALSE
%token NEG
%token CONJ DISJ IMPL EQUIV
%token FORALL EXISTS COLON
%token LPAREN RPAREN
%token EOF
%token <string> VAR
%start <Expr_ast.t> main

%%

main : e=expr EOF          { e }
expr : e=expr6             { e }

expr6 :
| FORALL x=VAR COLON e=expr6  { Forall (x, e) }
| EXISTS x=VAR COLON e=expr6  { Exists (x, e) }
| e=expr5                     { e }
                           
expr5 :
| e1=expr5 EQUIV e2=expr4  { Equiv (e1, e2) }
| e=expr4                  { e }

expr4 :
| e1=expr4 IMPL e2=expr3   { Impl (e1, e2) }
| e=expr3                  { e }

expr3 :
| e1=expr3 DISJ e2=expr2   { Disj (e1, e2) }
| e=expr2                  { e }

expr2 :
| e1=expr2 CONJ e2=expr1   { Conj (e1, e2) }
| e=expr1                  { e }

expr1 :
| TRUE                     { True }
| FALSE                    { False }
| x=VAR                    { Var x }
| NEG e=expr1              { Neg e }
| LPAREN e=expr RPAREN     { e }

%%
