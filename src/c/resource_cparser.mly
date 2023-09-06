%{
  open Ast
  open Trm
  open Typ
  open Resource_formula
%}

%token <string> IDENT
%token <int> INT_LIT
%token LPAR RPAR LBRACKET RBRACKET
%token SEMICOLON COLON COMMA AMPERSAND ARROW SQUIG_ARROW FUN EOF

%start <contract_resource list> resource_list

%%

atomic_formula:
  | x=IDENT
      { trm_var { qualifier = []; name = x; id = -1 } }
  | x=INT_LIT
      { trm_int x }
  | func=atomic_formula; LPAR; args=separated_list(COMMA, formula); RPAR
      { trm_apps func args }
  | AMPERSAND; tab=atomic_formula; LBRACKET; index=atomic_formula; RBRACKET;
    { trm_array_access tab index }
  | LPAR; f=formula; RPAR
      { f }

formula:
  | f=atomic_formula;
      { f }
  | t=atomic_formula; SQUIG_ARROW; f=atomic_formula;
      { formula_model t f }
  | FUN; args=separated_nonempty_list(COMMA, IDENT); ARROW; body=formula;
      { trm_fun ~annot:formula_annot (List.map (fun x -> { qualifier = []; name = x; id = -1 }, typ_make Typ_auto) args) None body }

resource:
  | f=formula; SEMICOLON
      { (None, f) }
  | hyp=IDENT; COLON; f=formula; SEMICOLON
      { (Some { qualifier = []; name = hyp; id = -1 }, f) }

resource_list:
  | res_list=resource*; EOF { res_list }
