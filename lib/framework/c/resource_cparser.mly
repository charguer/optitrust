%{
  open Ast
  open Trm
  open Typ
  open Resource_formula
  module String_map = Tools.String_map

  let builtins = [
    "size_t", typ_usize;
    "ptrdiff_t", typ_isize;
    "int8_t", typ_i8;
    "uint8_t", typ_u8;
    "int16_t", typ_i16;
    "uint16_t", typ_u16;
    "int32_t", typ_i32;
    "uint32_t", typ_u32;
    "int64_t", typ_i64;
    "uint64_t", typ_u64;
    "float", typ_f32;
    "double", typ_f64;
    "__full", full_frac;
  ] |> List.to_seq |> String_map.of_seq
%}

%token <string> IDENT
%token <int> INT_LIT
%token LPAR RPAR LBRACKET RBRACKET
%token COLON COMMA AMPERSAND ARROW SQUIG_ARROW COLON_EQUAL REV_ARROW DOT DOTDOT
%token FUN FORALL FOR IN EOF
%token PLUS MINUS STAR SLASH PERCENT
%token EQUAL LT GT LEQ GEQ NEQ

%right AMPERSAND ARROW

%start <contract_resource_item list> resource_list
%start <resource_item list> ghost_arg_list
%start <(var * var) list> ghost_bind

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
    { match String_map.find_opt x builtins with
      | Some builtin -> builtin
      | None -> trm_var ~annot:formula_annot (name_to_var x) }
  | x=INT_LIT
    { trm_int x }
  | func=atomic_formula; LPAR; args=separated_list(COMMA, formula); RPAR
    { trm_apps ~annot:formula_annot func args }
  | AMPERSAND; x=address_formula;
    { trm_address_of x }
  | LPAR; f=formula; RPAR
    { f }

address_formula:
  | tab=address_formula; LBRACKET; index=atomic_formula; RBRACKET;
    { trm_array_get tab index }
  | base=address_formula; DOT; field=IDENT;
    { trm_struct_get ~struct_typ:typ_auto base field }
  | base=address_formula; ARROW; field=IDENT;
    { trm_struct_get ~struct_typ:typ_auto (trm_get base) field }
  | LPAR; f=address_formula; RPAR
    { f }
  | x=IDENT
    { trm_var (name_to_var x) }

arith_factor:
  | a=arith_factor; STAR; b=atomic_formula;
    { trm_mul ~typ:typ_int a b }
  | a=arith_factor; SLASH; b=atomic_formula;
    { trm_trunc_div ~typ:typ_int a b }
  | a=arith_factor; PERCENT; b=atomic_formula;
    { trm_trunc_mod ~typ:typ_int a b }
  | a=atomic_formula;
    { a }

arith_term:
  | a=arith_term; PLUS; b=arith_factor;
    { trm_add ~typ:typ_int a b }
  | a=arith_term; MINUS; b=arith_factor;
    { trm_sub ~typ:typ_int a b }
  | MINUS; b=arith_factor;
    { trm_minus ~typ:typ_int b }
  | a=arith_factor;
    { a }

formula_cmp:
  | a=arith_term; EQUAL; b=arith_term;
    { formula_eq ~typ:typ_int a b }
  | a=arith_term; LT; b=arith_term;
    { formula_lt ~typ:typ_int a b }
  | a=arith_term; GT; b=arith_term;
    { formula_gt ~typ:typ_int a b }
  | a=arith_term; LEQ; b=arith_term;
    { formula_leq ~typ:typ_int a b }
  | a=arith_term; GEQ; b=arith_term;
    { formula_geq ~typ:typ_int a b }
  | a=arith_term; NEQ; b=arith_term;
    { formula_neq ~typ:typ_int a b }
  | start=arith_term; DOTDOT; stop=arith_term;
    { formula_range start stop (trm_int 1) }
  | a=arith_term;
    { a }

formula_arrow:
  | args=formula_cmp; ARROW; ret=formula_arrow;
    { let rec extract_arg_tuple t acc =
        match trm_binop_inv Binop_mul t with
        | Some (t1, t2) ->
          extract_arg_tuple t1 (t2 :: acc)
        | None -> t :: acc
      in
      typ_pure_simple_fun (extract_arg_tuple args []) ret }
  | a=formula_cmp;
    { a }

fun_arg:
  | x=IDENT
    { (name_to_var x, typ_auto) }
  | LPAR; x=IDENT; COLON; typ=formula; RPAR
    { (name_to_var x, typ) }

formula:
  | f=formula_arrow;
    { f }
  | t=atomic_formula; SQUIG_ARROW; f=atomic_formula;
    { formula_model t f }
  | FUN; args=nonempty_list(fun_arg); ARROW; body=formula;
    { formula_fun args body }
  | FORALL; args=nonempty_list(fun_arg); ARROW; body=formula;
    { typ_pure_fun args body }
  | FORALL; index=IDENT; IN; range=formula_cmp; ARROW; body=formula;
    { formula_forall_in (name_to_var index) range body }
  | FOR; index=IDENT; IN; range=formula_cmp; ARROW; body=formula;
    { formula_group (name_to_var index) range body }

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

single_ghost_bind:
  | bound_var=IDENT; REV_ARROW; contract_var=IDENT
    { (name_to_var bound_var, name_to_var contract_var) }
  | bound_var=IDENT
    { (name_to_var bound_var, dummy_var) }

ghost_bind:
  | ghost_bind=separated_list(COMMA, single_ghost_bind); EOF { ghost_bind }
