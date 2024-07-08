%{
  open Ast
  open Trm
  open Typ
  open Resource_formula
%}

%token <string> IDENT
%token <int> INT_LIT
%token LPAR RPAR LBRACKET RBRACKET
%token COLON COMMA AMPERSAND ARROW SQUIG_ARROW COLON_EQUAL DOTDOT
%token FUN FOR IN EOF
%token PLUS MINUS STAR SLASH PERCENT
%token EQUAL LT GT LEQ GEQ NEQ

%start <contract_resource_item list> resource_list
%start <resource_item list> ghost_arg_list

%%

flex_list(delimiter, X):
| (* nothing *)
    { [] }
| x = X
    { [x] }
| x = X; delimiter; xs = flex_list(delimiter, X)
    { x :: xs }

atomic_formula:
  | x=IDENT
    { trm_var (name_to_var x) }
  | x=INT_LIT
    { trm_int x }
  | func=atomic_formula; LPAR; args=separated_list(COMMA, formula); RPAR
    { trm_apps func args }
  | AMPERSAND; x=address_formula;
    { trm_address_of x }
  | LPAR; f=formula; RPAR
    { f }

address_formula:
  | tab=address_formula; LBRACKET; index=atomic_formula; RBRACKET;
    { trm_array_get tab index }
  | x=IDENT
    { trm_var (name_to_var x) }

arith_factor:
  | a=arith_factor; STAR; b=atomic_formula;
    { trm_mul a b }
  | a=arith_factor; SLASH; b=atomic_formula;
    { trm_div a b }
  | a=arith_factor; PERCENT; b=atomic_formula;
    { trm_mod a b }
  | a=atomic_formula;
    { a }

arith_term:
  | a=arith_term; PLUS; b=arith_factor;
    { trm_add a b }
  | a=arith_term; MINUS; b=arith_factor;
    { trm_sub a b }
  | a=arith_factor;
    { a }

formula_cmp:
  | a=arith_term; EQUAL; b=arith_term;
    { formula_eq a b }
  | a=arith_term; LT; b=arith_term;
    { formula_lt a b }
  | a=arith_term; GT; b=arith_term;
    { formula_gt a b }
  | a=arith_term; LEQ; b=arith_term;
    { formula_leq a b }
  | a=arith_term; GEQ; b=arith_term;
    { formula_geq a b }
  | a=arith_term; NEQ; b=arith_term;
    { formula_neq a b }
  | start=arith_term; DOTDOT; stop=arith_term;
    { formula_range start stop (trm_int 1) }
  | a=arith_term;
    { a }

formula_arrow:
  | a=formula_cmp; ARROW; b=formula_arrow;
    { formula_fun_type a b }
  | a=formula_cmp;
    { a }

formula:
  | f=formula_arrow;
    { f }
  | t=atomic_formula; SQUIG_ARROW; f=atomic_formula;
    { formula_model t f }
  | FUN; args=separated_nonempty_list(COMMA, IDENT); ARROW; body=formula;
    { trm_fun ~annot:formula_annot (List.map (fun x -> name_to_var x, typ_auto) args) None body }
  | FOR; index=IDENT; IN; range=formula_cmp; ARROW; body=formula;
    { trm_apps ~annot:formula_annot trm_group [range; trm_fun ~annot:formula_annot [name_to_var index, typ_int] None body] }

resource:
  | f=formula
    { (None, f) }
  | hyp=IDENT; COLON; f=formula
    { (Some (name_to_var hyp), f) }

resource_list:
  | res_list=flex_list(COMMA, resource); EOF { res_list }

ghost_arg:
  | ghost_var=IDENT; COLON_EQUAL; formula=formula
    { (name_to_var ghost_var, formula) }
  | formula=formula
    { (dummy_var, formula) }

ghost_arg_list:
  | ghost_args=separated_list(COMMA, ghost_arg); EOF { ghost_args }
