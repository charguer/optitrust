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
    "Cell", trm_cell ();
    "UninitCell", trm_uninit_cell ();
  ] |> List.to_seq |> String_map.of_seq
%}

%token <string> IDENT
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token LPAR RPAR LBRACKET RBRACKET
%token COLON COMMA AMPERSAND ARROW SQUIG_ARROW LONG_SQUIG_ARROW COLON_EQUAL REV_ARROW DOT DOTDOT UNDERSCORE
%token FUN FORALL FOR DESYNC_FOR IN EOF
%token PLUS MINUS STAR SLASH PERCENT
%token EQUAL LT GT LEQ GEQ NEQ

%right AMPERSAND ARROW

%start <contract_resource_item list> resource_list
%start <resource_item list> ghost_arg_list
%start <(var option * var) list> ghost_bind

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
  | x=FLOAT_LIT
    { trm_float ~typ:typ_f32 x }
  | func=atomic_formula; LPAR; args=separated_list(COMMA, formula); RPAR
    { trm_apps ~annot:formula_annot func args }
  | LPAR; f=formula; RPAR
    { f }
  | tab=atomic_formula; LBRACKET; index=formula; RBRACKET;
    { trm_array_get tab index }
  | base=atomic_formula; DOT; field=IDENT;
    { trm_struct_get ~struct_typ:typ_auto base field }

address_formula:
  | base=address_formula; ARROW; field=IDENT;
    { trm_struct_get ~struct_typ:typ_auto (trm_get base) field }
  | a=atomic_formula
    { a }

ampersand_formula:
  | AMPERSAND; x=address_formula;
    { trm_address_of x }
  | a=atomic_formula
    { a }

arith_factor:
  | a=arith_factor; STAR; b=ampersand_formula;
    { trm_mul ~typ:typ_int a b }
  | a=arith_factor; SLASH; b=ampersand_formula;
    { trm_trunc_div ~typ:typ_int a b }
  | a=arith_factor; PERCENT; b=ampersand_formula;
    { trm_trunc_mod ~typ:typ_int a b }
  | a=arith_factor; STAR; DOT; b=ampersand_formula;
    { trm_mul ~typ:typ_f32 a b }
  | a=arith_factor; SLASH; DOT; b=ampersand_formula;
    { trm_exact_div ~typ:typ_f32 a b }
  | a=ampersand_formula;
    { a }

arith_term:
  | a=arith_term; PLUS; b=arith_factor;
    { trm_add ~typ:typ_int a b }
  | a=arith_term; MINUS; b=arith_factor;
    { trm_sub ~typ:typ_int a b }
  | MINUS; b=arith_factor;
    { trm_minus ~typ:typ_int b }
  | a=arith_term; PLUS; DOT; b=arith_factor;
    { trm_add ~typ:typ_f32 a b }
  | a=arith_term; MINUS; DOT; b=arith_factor;
    { trm_sub ~typ:typ_f32 a b }
  | MINUS; DOT; b=arith_factor;
    { trm_minus ~typ:typ_f32 b }
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
  | a=arith_term; EQUAL; DOT; b=arith_term;
    { formula_eq ~typ:typ_f32 a b }
  | a=arith_term; LT; DOT; b=arith_term;
    { formula_lt ~typ:typ_f32 a b }
  | a=arith_term; GT; DOT; b=arith_term;
    { formula_gt ~typ:typ_f32 a b }
  | a=arith_term; LEQ; DOT; b=arith_term;
    { formula_leq ~typ:typ_f32 a b }
  | a=arith_term; GEQ; DOT; b=arith_term;
    { formula_geq ~typ:typ_f32 a b }
  | a=arith_term; NEQ; DOT; b=arith_term;
    { formula_neq ~typ:typ_f32 a b }
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

binder:
  | x=IDENT
    { name_to_var x }
  | UNDERSCORE
    { new_anon_hyp () }

fun_arg:
  | x=binder
    { [x, typ_auto] }
  | LPAR; xs=nonempty_list(binder); COLON; typ=formula; RPAR
    { List.map (fun x -> (x, typ)) xs }

fun_args:
  | args=nonempty_list(fun_arg)
    { List.concat args }

formula:
  | f=formula_arrow;
    { f }
  | t=ampersand_formula; SQUIG_ARROW; f=formula;
    { formula_repr t f }
  | t=ampersand_formula; LONG_SQUIG_ARROW; f=formula;
    { formula_points_to ~mem_typ:mem_typ_any t f }
  | t=ampersand_formula; LONG_SQUIG_ARROW; LBRACKET; h=IDENT; RBRACKET; f=formula;
    { formula_points_to ~mem_typ:(trm_var ~annot:formula_annot (name_to_var h)) t f }
  | FUN; args=fun_args; ARROW; body=formula;
    { formula_fun args body }
  | FORALL; args=fun_args; ARROW; body=formula;
    { typ_pure_fun args body }
  | FORALL; index=binder; IN; range=formula_cmp; ARROW; body=formula;
    { formula_forall_in index range body }
  | FOR; index=binder; IN; range=formula_cmp; ARROW; body=formula;
    { formula_group index range body }
  | DESYNC_FOR; LPAR; range=formula_cmp; RPAR; index=binder; IN; DOTDOT; bound=arith_term; ARROW; body=formula;
    { formula_desyncgroup index range bound body }

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
    { (Some (name_to_var bound_var), name_to_var contract_var) }
  | bound_var=IDENT
    { (Some (name_to_var bound_var), dummy_var) }
  | UNDERSCORE; REV_ARROW; contract_var=IDENT
    { (None, name_to_var contract_var) }
  | UNDERSCORE
    { (None, dummy_var) }

ghost_bind:
  | ghost_bind=separated_list(COMMA, single_ghost_bind); EOF { ghost_bind }
