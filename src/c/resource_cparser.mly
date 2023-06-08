%{
  open Ast
  open Resources_contract
%}

%token <string> IDENT
%token LPAR RPAR
%token SEMICOLON COLON COMMA ARROW FAT_ARROW FUN EOF

%start <contract_resource list> resource_list

%%

atomic_formula:
  | x=IDENT
      { trm_var x }
  | func=atomic_formula; LPAR; args=separated_list(COMMA, formula); RPAR
      { trm_apps func args }
  | LPAR; f=formula; RPAR
      { f }

formula:
  | f=atomic_formula;
      { f }
  | x=IDENT; FAT_ARROW; f=atomic_formula;
      { formula_var_model x f }
  | FUN args=separated_nonempty_list(COMMA, IDENT); ARROW; body=formula;
      { trm_fun (List.map (fun x -> (x, typ_make Typ_auto)) args) None body }

resource:
  | f=formula; SEMICOLON
      { (None, f) }
  | hyp=IDENT; COLON; f=formula; SEMICOLON
      { (Some hyp, f) }

resource_list:
  | res_list=resource*; EOF { res_list }
