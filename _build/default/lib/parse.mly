%{
  open! Batteries
  open! Uref
  open! U
  open! Glue
  open! M
%}

%token EOF UNIFY
%token<string> CONST VAR

%start<M.term list> expr_file
%start<M.term list * M.term list> problem

%%
expr: 
  | list(term) {$1}

term: 
  | VAR {Monoid (memo $1)}
  | CONST {Letter $1}

expr_file: expr EOF {$1}
problem: expr UNIFY expr EOF {$1, $3}
