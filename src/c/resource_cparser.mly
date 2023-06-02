%{
  open Ast
  open Resources_contract
%}

%token <string> IDENT
%token LPAR RPAR
%token SEMICOLON COLON COMMA ARROW FAT_ARROW FUN EOF

%start <contract_resource list> resource_list

%%

formula:
  | x=IDENT
      { trm_var x }
  | func=formula; LPAR; args=separated_list(COMMA, formula); RPAR
      { trm_apps func args }
  | x=IDENT; FAT_ARROW; f=formula
      { trm_var_model x f }
  | LPAR; FUN args=separated_nonempty_list(COMMA, IDENT); ARROW; body=formula; RPAR;
      { trm_fun (List.map (fun x -> (x, typ_make Typ_auto)) args) None body }
  | LPAR; f=formula; RPAR
      { f }

resource:
  | f=formula; SEMICOLON
      { (None, f) }
  | hyp=IDENT; COLON; f=formula; SEMICOLON
      { (Some hyp, f) }

resource_list:
  | res_list=resource*; EOF { res_list }
