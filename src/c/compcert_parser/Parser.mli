open Alphabet
open BinNums
open Cabs
open Datatypes
open Grammar
open Int0
open List0
open Main
open Nat
open Specif

type __ = Obj.t

module Coq__1 : sig
 type token =
 | XOR_ASSIGN of loc
 | WHILE of loc
 | VOLATILE of loc
 | VOID of loc
 | VAR_NAME of (String.t * loc)
 | UNSIGNED of loc
 | UNION of loc
 | UNDERSCORE_BOOL of loc
 | TYPEDEF_NAME of (String.t * loc)
 | TYPEDEF of loc
 | TILDE of loc
 | SWITCH of loc
 | SUB_ASSIGN of loc
 | STRUCT of loc
 | STRING_LITERAL of ((bool * char_code list) * loc)
 | STATIC_ASSERT of loc
 | STATIC of loc
 | STAR of loc
 | SLASH of loc
 | SIZEOF of loc
 | SIGNED of loc
 | SHORT of loc
 | SEMICOLON of loc
 | RPAREN of loc
 | RIGHT_ASSIGN of loc
 | RIGHT of loc
 | RETURN of loc
 | RESTRICT of loc
 | REGISTER of loc
 | RBRACK of loc
 | RBRACE of loc
 | QUESTION of loc
 | PTR of loc
 | PRAGMA of (String.t * loc)
 | PLUS of loc
 | PERCENT of loc
 | PACKED of loc
 | OTHER_NAME of (String.t * loc)
 | OR_ASSIGN of loc
 | NORETURN of loc
 | NEQ of loc
 | MUL_ASSIGN of loc
 | MOD_ASSIGN of loc
 | MINUS of loc
 | LT of loc
 | LPAREN of loc
 | LONG of loc
 | LEQ of loc
 | LEFT_ASSIGN of loc
 | LEFT of loc
 | LBRACK of loc
 | LBRACE of loc
 | INT of loc
 | INLINE of loc
 | INC of loc
 | IF_ of loc
 | HAT of loc
 | GT of loc
 | GOTO of loc
 | GEQ of loc
 | FOR of loc
 | FLOAT of loc
 | EXTERN of loc
 | EQEQ of loc
 | EQ of loc
 | EOF of unit
 | ENUM of loc
 | ELSE of loc
 | ELLIPSIS of loc
 | DOUBLE of loc
 | DOT of loc
 | DO of loc
 | DIV_ASSIGN of loc
 | DEFAULT of loc
 | DEC of loc
 | CONTINUE of loc
 | CONSTANT of (constant * loc)
 | CONST of loc
 | COMMA of loc
 | COLON of loc
 | CHAR of loc
 | CASE of loc
 | BUILTIN_VA_ARG of loc
 | BUILTIN_OFFSETOF of loc
 | BREAK of loc
 | BARBAR of loc
 | BAR of loc
 | BANG of loc
 | AUTO of loc
 | ATTRIBUTE of loc
 | ASM of loc
 | AND_ASSIGN of loc
 | ANDAND of loc
 | AND of loc
 | ALIGNOF of loc
 | ALIGNAS of loc
 | ADD_ASSIGN of loc
end
include module type of struct include Coq__1 end

module Gram :
 sig
  type terminal' =
  | ADD_ASSIGN't
  | ALIGNAS't
  | ALIGNOF't
  | AND't
  | ANDAND't
  | AND_ASSIGN't
  | ASM't
  | ATTRIBUTE't
  | AUTO't
  | BANG't
  | BAR't
  | BARBAR't
  | BREAK't
  | BUILTIN_OFFSETOF't
  | BUILTIN_VA_ARG't
  | CASE't
  | CHAR't
  | COLON't
  | COMMA't
  | CONST't
  | CONSTANT't
  | CONTINUE't
  | DEC't
  | DEFAULT't
  | DIV_ASSIGN't
  | DO't
  | DOT't
  | DOUBLE't
  | ELLIPSIS't
  | ELSE't
  | ENUM't
  | EOF't
  | EQ't
  | EQEQ't
  | EXTERN't
  | FLOAT't
  | FOR't
  | GEQ't
  | GOTO't
  | GT't
  | HAT't
  | IF_'t
  | INC't
  | INLINE't
  | INT't
  | LBRACE't
  | LBRACK't
  | LEFT't
  | LEFT_ASSIGN't
  | LEQ't
  | LONG't
  | LPAREN't
  | LT't
  | MINUS't
  | MOD_ASSIGN't
  | MUL_ASSIGN't
  | NEQ't
  | NORETURN't
  | OR_ASSIGN't
  | OTHER_NAME't
  | PACKED't
  | PERCENT't
  | PLUS't
  | PRAGMA't
  | PTR't
  | QUESTION't
  | RBRACE't
  | RBRACK't
  | REGISTER't
  | RESTRICT't
  | RETURN't
  | RIGHT't
  | RIGHT_ASSIGN't
  | RPAREN't
  | SEMICOLON't
  | SHORT't
  | SIGNED't
  | SIZEOF't
  | SLASH't
  | STAR't
  | STATIC't
  | STATIC_ASSERT't
  | STRING_LITERAL't
  | STRUCT't
  | SUB_ASSIGN't
  | SWITCH't
  | TILDE't
  | TYPEDEF't
  | TYPEDEF_NAME't
  | UNDERSCORE_BOOL't
  | UNION't
  | UNSIGNED't
  | VAR_NAME't
  | VOID't
  | VOLATILE't
  | WHILE't
  | XOR_ASSIGN't

  type terminal = terminal'

  val terminalNum : terminal coq_Numbered

  val coq_TerminalAlph : terminal coq_Alphabet

  type nonterminal' =
  | AND_expression'nt
  | Coq_abstract_declarator'nt
  | Coq_additive_expression'nt
  | Coq_argument_expression_list'nt
  | Coq_asm_arguments'nt
  | Coq_asm_attributes'nt
  | Coq_asm_flags'nt
  | Coq_asm_op_name'nt
  | Coq_asm_operand'nt
  | Coq_asm_operands'nt
  | Coq_asm_operands_ne'nt
  | Coq_asm_statement'nt
  | Coq_assignment_expression'nt
  | Coq_assignment_operator'nt
  | Coq_attribute_specifier'nt
  | Coq_attribute_specifier_list'nt
  | Coq_block_item'nt
  | Coq_block_item_list'nt
  | Coq_c_initializer'nt
  | Coq_cast_expression'nt
  | Coq_compound_statement'nt
  | Coq_conditional_expression'nt
  | Coq_constant_expression'nt
  | Coq_declaration'nt
  | Coq_declaration_list'nt
  | Coq_declaration_specifiers'nt
  | Coq_declaration_specifiers_typespec_opt'nt
  | Coq_declarator'nt
  | Coq_declarator_noattrend'nt
  | Coq_designation'nt
  | Coq_designator'nt
  | Coq_designator_list'nt
  | Coq_direct_abstract_declarator'nt
  | Coq_direct_declarator'nt
  | Coq_enum_specifier'nt
  | Coq_enumeration_constant'nt
  | Coq_enumerator'nt
  | Coq_enumerator_list'nt
  | Coq_equality_expression'nt
  | Coq_exclusive_OR_expression'nt
  | Coq_expression'nt
  | Coq_expression_statement'nt
  | Coq_external_declaration'nt
  | Coq_function_definition'nt
  | Coq_function_specifier'nt
  | Coq_gcc_attribute'nt
  | Coq_gcc_attribute_list'nt
  | Coq_gcc_attribute_word'nt
  | Coq_identifier_list'nt
  | Coq_inclusive_OR_expression'nt
  | Coq_init_declarator'nt
  | Coq_init_declarator_list'nt
  | Coq_initializer_list'nt
  | Coq_iteration_statement_statement_dangerous_'nt
  | Coq_iteration_statement_statement_safe_'nt
  | Coq_jump_statement'nt
  | Coq_labeled_statement_statement_dangerous_'nt
  | Coq_labeled_statement_statement_safe_'nt
  | Coq_logical_AND_expression'nt
  | Coq_logical_OR_expression'nt
  | Coq_multiplicative_expression'nt
  | Coq_parameter_declaration'nt
  | Coq_parameter_list'nt
  | Coq_parameter_type_list'nt
  | Coq_pointer'nt
  | Coq_postfix_expression'nt
  | Coq_primary_expression'nt
  | Coq_relational_expression'nt
  | Coq_selection_statement_dangerous'nt
  | Coq_selection_statement_safe'nt
  | Coq_shift_expression'nt
  | Coq_specifier_qualifier_list'nt
  | Coq_statement_dangerous'nt
  | Coq_statement_safe'nt
  | Coq_static_assert_declaration'nt
  | Coq_storage_class_specifier'nt
  | Coq_struct_declaration'nt
  | Coq_struct_declaration_list'nt
  | Coq_struct_declarator'nt
  | Coq_struct_declarator_list'nt
  | Coq_struct_or_union'nt
  | Coq_struct_or_union_specifier'nt
  | Coq_translation_unit'nt
  | Coq_translation_unit_file'nt
  | Coq_type_name'nt
  | Coq_type_qualifier'nt
  | Coq_type_qualifier_list'nt
  | Coq_type_qualifier_noattr'nt
  | Coq_type_specifier'nt
  | Coq_unary_expression'nt
  | Coq_unary_operator'nt

  type nonterminal = nonterminal'

  val nonterminalNum : nonterminal coq_Numbered

  val coq_NonTerminalAlph : nonterminal coq_Alphabet

  type symbol =
  | T of terminal
  | NT of nonterminal

  val symbol_rect : (terminal -> 'a1) -> (nonterminal -> 'a1) -> symbol -> 'a1

  val symbol_rec : (terminal -> 'a1) -> (nonterminal -> 'a1) -> symbol -> 'a1

  val coq_SymbolAlph : symbol coq_Alphabet

  type terminal_semantic_type = __

  type nonterminal_semantic_type = __

  type symbol_semantic_type = __

  type token = Coq__1.token

  val token_term : token -> terminal

  val token_sem : token -> symbol_semantic_type

  type production' =
  | Prod'unary_operator'5
  | Prod'unary_operator'4
  | Prod'unary_operator'3
  | Prod'unary_operator'2
  | Prod'unary_operator'1
  | Prod'unary_operator'0
  | Prod'unary_expression'6
  | Prod'unary_expression'5
  | Prod'unary_expression'4
  | Prod'unary_expression'3
  | Prod'unary_expression'2
  | Prod'unary_expression'1
  | Prod'unary_expression'0
  | Prod'type_specifier'12
  | Prod'type_specifier'11
  | Prod'type_specifier'10
  | Prod'type_specifier'9
  | Prod'type_specifier'8
  | Prod'type_specifier'7
  | Prod'type_specifier'6
  | Prod'type_specifier'5
  | Prod'type_specifier'4
  | Prod'type_specifier'3
  | Prod'type_specifier'2
  | Prod'type_specifier'1
  | Prod'type_specifier'0
  | Prod'type_qualifier_noattr'2
  | Prod'type_qualifier_noattr'1
  | Prod'type_qualifier_noattr'0
  | Prod'type_qualifier_list'1
  | Prod'type_qualifier_list'0
  | Prod'type_qualifier'1
  | Prod'type_qualifier'0
  | Prod'type_name'1
  | Prod'type_name'0
  | Prod'translation_unit_file'1
  | Prod'translation_unit_file'0
  | Prod'translation_unit'3
  | Prod'translation_unit'2
  | Prod'translation_unit'1
  | Prod'translation_unit'0
  | Prod'struct_or_union_specifier'2
  | Prod'struct_or_union_specifier'1
  | Prod'struct_or_union_specifier'0
  | Prod'struct_or_union'1
  | Prod'struct_or_union'0
  | Prod'struct_declarator_list'1
  | Prod'struct_declarator_list'0
  | Prod'struct_declarator'2
  | Prod'struct_declarator'1
  | Prod'struct_declarator'0
  | Prod'struct_declaration_list'1
  | Prod'struct_declaration_list'0
  | Prod'struct_declaration'2
  | Prod'struct_declaration'1
  | Prod'struct_declaration'0
  | Prod'storage_class_specifier'4
  | Prod'storage_class_specifier'3
  | Prod'storage_class_specifier'2
  | Prod'storage_class_specifier'1
  | Prod'storage_class_specifier'0
  | Prod'static_assert_declaration'0
  | Prod'statement_safe'6
  | Prod'statement_safe'5
  | Prod'statement_safe'4
  | Prod'statement_safe'3
  | Prod'statement_safe'2
  | Prod'statement_safe'1
  | Prod'statement_safe'0
  | Prod'statement_dangerous'6
  | Prod'statement_dangerous'5
  | Prod'statement_dangerous'4
  | Prod'statement_dangerous'3
  | Prod'statement_dangerous'2
  | Prod'statement_dangerous'1
  | Prod'statement_dangerous'0
  | Prod'specifier_qualifier_list'3
  | Prod'specifier_qualifier_list'2
  | Prod'specifier_qualifier_list'1
  | Prod'specifier_qualifier_list'0
  | Prod'shift_expression'2
  | Prod'shift_expression'1
  | Prod'shift_expression'0
  | Prod'selection_statement_safe'1
  | Prod'selection_statement_safe'0
  | Prod'selection_statement_dangerous'2
  | Prod'selection_statement_dangerous'1
  | Prod'selection_statement_dangerous'0
  | Prod'relational_expression'4
  | Prod'relational_expression'3
  | Prod'relational_expression'2
  | Prod'relational_expression'1
  | Prod'relational_expression'0
  | Prod'primary_expression'3
  | Prod'primary_expression'2
  | Prod'primary_expression'1
  | Prod'primary_expression'0
  | Prod'postfix_expression'12
  | Prod'postfix_expression'11
  | Prod'postfix_expression'10
  | Prod'postfix_expression'9
  | Prod'postfix_expression'8
  | Prod'postfix_expression'7
  | Prod'postfix_expression'6
  | Prod'postfix_expression'5
  | Prod'postfix_expression'4
  | Prod'postfix_expression'3
  | Prod'postfix_expression'2
  | Prod'postfix_expression'1
  | Prod'postfix_expression'0
  | Prod'pointer'4
  | Prod'pointer'3
  | Prod'pointer'2
  | Prod'pointer'1
  | Prod'pointer'0
  | Prod'parameter_type_list'1
  | Prod'parameter_type_list'0
  | Prod'parameter_list'1
  | Prod'parameter_list'0
  | Prod'parameter_declaration'2
  | Prod'parameter_declaration'1
  | Prod'parameter_declaration'0
  | Prod'multiplicative_expression'3
  | Prod'multiplicative_expression'2
  | Prod'multiplicative_expression'1
  | Prod'multiplicative_expression'0
  | Prod'logical_OR_expression'1
  | Prod'logical_OR_expression'0
  | Prod'logical_AND_expression'1
  | Prod'logical_AND_expression'0
  | Prod'labeled_statement_statement_safe_'2
  | Prod'labeled_statement_statement_safe_'1
  | Prod'labeled_statement_statement_safe_'0
  | Prod'labeled_statement_statement_dangerous_'2
  | Prod'labeled_statement_statement_dangerous_'1
  | Prod'labeled_statement_statement_dangerous_'0
  | Prod'jump_statement'4
  | Prod'jump_statement'3
  | Prod'jump_statement'2
  | Prod'jump_statement'1
  | Prod'jump_statement'0
  | Prod'iteration_statement_statement_safe_'13
  | Prod'iteration_statement_statement_safe_'12
  | Prod'iteration_statement_statement_safe_'11
  | Prod'iteration_statement_statement_safe_'10
  | Prod'iteration_statement_statement_safe_'9
  | Prod'iteration_statement_statement_safe_'8
  | Prod'iteration_statement_statement_safe_'7
  | Prod'iteration_statement_statement_safe_'6
  | Prod'iteration_statement_statement_safe_'5
  | Prod'iteration_statement_statement_safe_'4
  | Prod'iteration_statement_statement_safe_'3
  | Prod'iteration_statement_statement_safe_'2
  | Prod'iteration_statement_statement_safe_'1
  | Prod'iteration_statement_statement_safe_'0
  | Prod'iteration_statement_statement_dangerous_'13
  | Prod'iteration_statement_statement_dangerous_'12
  | Prod'iteration_statement_statement_dangerous_'11
  | Prod'iteration_statement_statement_dangerous_'10
  | Prod'iteration_statement_statement_dangerous_'9
  | Prod'iteration_statement_statement_dangerous_'8
  | Prod'iteration_statement_statement_dangerous_'7
  | Prod'iteration_statement_statement_dangerous_'6
  | Prod'iteration_statement_statement_dangerous_'5
  | Prod'iteration_statement_statement_dangerous_'4
  | Prod'iteration_statement_statement_dangerous_'3
  | Prod'iteration_statement_statement_dangerous_'2
  | Prod'iteration_statement_statement_dangerous_'1
  | Prod'iteration_statement_statement_dangerous_'0
  | Prod'initializer_list'3
  | Prod'initializer_list'2
  | Prod'initializer_list'1
  | Prod'initializer_list'0
  | Prod'init_declarator_list'1
  | Prod'init_declarator_list'0
  | Prod'init_declarator'1
  | Prod'init_declarator'0
  | Prod'inclusive_OR_expression'1
  | Prod'inclusive_OR_expression'0
  | Prod'identifier_list'1
  | Prod'identifier_list'0
  | Prod'gcc_attribute_word'2
  | Prod'gcc_attribute_word'1
  | Prod'gcc_attribute_word'0
  | Prod'gcc_attribute_list'1
  | Prod'gcc_attribute_list'0
  | Prod'gcc_attribute'3
  | Prod'gcc_attribute'2
  | Prod'gcc_attribute'1
  | Prod'gcc_attribute'0
  | Prod'function_specifier'1
  | Prod'function_specifier'0
  | Prod'function_definition'1
  | Prod'function_definition'0
  | Prod'external_declaration'2
  | Prod'external_declaration'1
  | Prod'external_declaration'0
  | Prod'expression_statement'1
  | Prod'expression_statement'0
  | Prod'expression'1
  | Prod'expression'0
  | Prod'exclusive_OR_expression'1
  | Prod'exclusive_OR_expression'0
  | Prod'equality_expression'2
  | Prod'equality_expression'1
  | Prod'equality_expression'0
  | Prod'enumerator_list'1
  | Prod'enumerator_list'0
  | Prod'enumerator'1
  | Prod'enumerator'0
  | Prod'enumeration_constant'0
  | Prod'enum_specifier'4
  | Prod'enum_specifier'3
  | Prod'enum_specifier'2
  | Prod'enum_specifier'1
  | Prod'enum_specifier'0
  | Prod'direct_declarator'8
  | Prod'direct_declarator'7
  | Prod'direct_declarator'6
  | Prod'direct_declarator'5
  | Prod'direct_declarator'4
  | Prod'direct_declarator'3
  | Prod'direct_declarator'2
  | Prod'direct_declarator'1
  | Prod'direct_declarator'0
  | Prod'direct_abstract_declarator'12
  | Prod'direct_abstract_declarator'11
  | Prod'direct_abstract_declarator'10
  | Prod'direct_abstract_declarator'9
  | Prod'direct_abstract_declarator'8
  | Prod'direct_abstract_declarator'7
  | Prod'direct_abstract_declarator'6
  | Prod'direct_abstract_declarator'5
  | Prod'direct_abstract_declarator'4
  | Prod'direct_abstract_declarator'3
  | Prod'direct_abstract_declarator'2
  | Prod'direct_abstract_declarator'1
  | Prod'direct_abstract_declarator'0
  | Prod'designator_list'1
  | Prod'designator_list'0
  | Prod'designator'1
  | Prod'designator'0
  | Prod'designation'0
  | Prod'declarator_noattrend'1
  | Prod'declarator_noattrend'0
  | Prod'declarator'0
  | Prod'declaration_specifiers_typespec_opt'4
  | Prod'declaration_specifiers_typespec_opt'3
  | Prod'declaration_specifiers_typespec_opt'2
  | Prod'declaration_specifiers_typespec_opt'1
  | Prod'declaration_specifiers_typespec_opt'0
  | Prod'declaration_specifiers'4
  | Prod'declaration_specifiers'3
  | Prod'declaration_specifiers'2
  | Prod'declaration_specifiers'1
  | Prod'declaration_specifiers'0
  | Prod'declaration_list'1
  | Prod'declaration_list'0
  | Prod'declaration'2
  | Prod'declaration'1
  | Prod'declaration'0
  | Prod'constant_expression'0
  | Prod'conditional_expression'1
  | Prod'conditional_expression'0
  | Prod'compound_statement'1
  | Prod'compound_statement'0
  | Prod'cast_expression'1
  | Prod'cast_expression'0
  | Prod'c_initializer'2
  | Prod'c_initializer'1
  | Prod'c_initializer'0
  | Prod'block_item_list'1
  | Prod'block_item_list'0
  | Prod'block_item'2
  | Prod'block_item'1
  | Prod'block_item'0
  | Prod'attribute_specifier_list'1
  | Prod'attribute_specifier_list'0
  | Prod'attribute_specifier'3
  | Prod'attribute_specifier'2
  | Prod'attribute_specifier'1
  | Prod'attribute_specifier'0
  | Prod'assignment_operator'10
  | Prod'assignment_operator'9
  | Prod'assignment_operator'8
  | Prod'assignment_operator'7
  | Prod'assignment_operator'6
  | Prod'assignment_operator'5
  | Prod'assignment_operator'4
  | Prod'assignment_operator'3
  | Prod'assignment_operator'2
  | Prod'assignment_operator'1
  | Prod'assignment_operator'0
  | Prod'assignment_expression'1
  | Prod'assignment_expression'0
  | Prod'asm_statement'0
  | Prod'asm_operands_ne'1
  | Prod'asm_operands_ne'0
  | Prod'asm_operands'1
  | Prod'asm_operands'0
  | Prod'asm_operand'0
  | Prod'asm_op_name'1
  | Prod'asm_op_name'0
  | Prod'asm_flags'1
  | Prod'asm_flags'0
  | Prod'asm_attributes'2
  | Prod'asm_attributes'1
  | Prod'asm_attributes'0
  | Prod'asm_arguments'3
  | Prod'asm_arguments'2
  | Prod'asm_arguments'1
  | Prod'asm_arguments'0
  | Prod'argument_expression_list'1
  | Prod'argument_expression_list'0
  | Prod'additive_expression'2
  | Prod'additive_expression'1
  | Prod'additive_expression'0
  | Prod'abstract_declarator'2
  | Prod'abstract_declarator'1
  | Prod'abstract_declarator'0
  | Prod'AND_expression'1
  | Prod'AND_expression'0

  type production = production'

  val productionNum : production coq_Numbered

  val coq_ProductionAlph : production coq_Alphabet

  val prod_contents :
    production -> (nonterminal * symbol list, symbol_semantic_type
    arrows_right) sigT

  val prod_lhs : production -> nonterminal

  val prod_rhs_rev : production -> symbol list

  val prod_action : production -> symbol_semantic_type arrows_right

  type parse_tree =
  | Terminal_pt of token
  | Non_terminal_pt of production * token list * parse_tree_list
  and parse_tree_list =
  | Nil_ptl
  | Cons_ptl of symbol list * token list * parse_tree_list * symbol
     * token list * parse_tree

  val parse_tree_rect :
    (token -> 'a1) -> (production -> token list -> parse_tree_list -> 'a1) ->
    symbol -> token list -> parse_tree -> 'a1

  val parse_tree_rec :
    (token -> 'a1) -> (production -> token list -> parse_tree_list -> 'a1) ->
    symbol -> token list -> parse_tree -> 'a1

  val parse_tree_list_rect :
    'a1 -> (symbol list -> token list -> parse_tree_list -> 'a1 -> symbol ->
    token list -> parse_tree -> 'a1) -> symbol list -> token list ->
    parse_tree_list -> 'a1

  val parse_tree_list_rec :
    'a1 -> (symbol list -> token list -> parse_tree_list -> 'a1 -> symbol ->
    token list -> parse_tree -> 'a1) -> symbol list -> token list ->
    parse_tree_list -> 'a1

  val pt_sem : symbol -> token list -> parse_tree -> symbol_semantic_type

  val ptl_sem :
    symbol list -> token list -> parse_tree_list -> 'a1 arrows_right -> 'a1

  val pt_size : symbol -> token list -> parse_tree -> nat

  val ptl_size : symbol list -> token list -> parse_tree_list -> nat
 end
module Coq__2 : module type of struct include Gram end

module Aut :
 sig
  module Gram :
   sig
    type terminal' = Gram.terminal' =
    | ADD_ASSIGN't
    | ALIGNAS't
    | ALIGNOF't
    | AND't
    | ANDAND't
    | AND_ASSIGN't
    | ASM't
    | ATTRIBUTE't
    | AUTO't
    | BANG't
    | BAR't
    | BARBAR't
    | BREAK't
    | BUILTIN_OFFSETOF't
    | BUILTIN_VA_ARG't
    | CASE't
    | CHAR't
    | COLON't
    | COMMA't
    | CONST't
    | CONSTANT't
    | CONTINUE't
    | DEC't
    | DEFAULT't
    | DIV_ASSIGN't
    | DO't
    | DOT't
    | DOUBLE't
    | ELLIPSIS't
    | ELSE't
    | ENUM't
    | EOF't
    | EQ't
    | EQEQ't
    | EXTERN't
    | FLOAT't
    | FOR't
    | GEQ't
    | GOTO't
    | GT't
    | HAT't
    | IF_'t
    | INC't
    | INLINE't
    | INT't
    | LBRACE't
    | LBRACK't
    | LEFT't
    | LEFT_ASSIGN't
    | LEQ't
    | LONG't
    | LPAREN't
    | LT't
    | MINUS't
    | MOD_ASSIGN't
    | MUL_ASSIGN't
    | NEQ't
    | NORETURN't
    | OR_ASSIGN't
    | OTHER_NAME't
    | PACKED't
    | PERCENT't
    | PLUS't
    | PRAGMA't
    | PTR't
    | QUESTION't
    | RBRACE't
    | RBRACK't
    | REGISTER't
    | RESTRICT't
    | RETURN't
    | RIGHT't
    | RIGHT_ASSIGN't
    | RPAREN't
    | SEMICOLON't
    | SHORT't
    | SIGNED't
    | SIZEOF't
    | SLASH't
    | STAR't
    | STATIC't
    | STATIC_ASSERT't
    | STRING_LITERAL't
    | STRUCT't
    | SUB_ASSIGN't
    | SWITCH't
    | TILDE't
    | TYPEDEF't
    | TYPEDEF_NAME't
    | UNDERSCORE_BOOL't
    | UNION't
    | UNSIGNED't
    | VAR_NAME't
    | VOID't
    | VOLATILE't
    | WHILE't
    | XOR_ASSIGN't

    type terminal = terminal'

    val terminalNum : terminal coq_Numbered

    val coq_TerminalAlph : terminal coq_Alphabet

    type nonterminal' = Gram.nonterminal' =
    | AND_expression'nt
    | Coq_abstract_declarator'nt
    | Coq_additive_expression'nt
    | Coq_argument_expression_list'nt
    | Coq_asm_arguments'nt
    | Coq_asm_attributes'nt
    | Coq_asm_flags'nt
    | Coq_asm_op_name'nt
    | Coq_asm_operand'nt
    | Coq_asm_operands'nt
    | Coq_asm_operands_ne'nt
    | Coq_asm_statement'nt
    | Coq_assignment_expression'nt
    | Coq_assignment_operator'nt
    | Coq_attribute_specifier'nt
    | Coq_attribute_specifier_list'nt
    | Coq_block_item'nt
    | Coq_block_item_list'nt
    | Coq_c_initializer'nt
    | Coq_cast_expression'nt
    | Coq_compound_statement'nt
    | Coq_conditional_expression'nt
    | Coq_constant_expression'nt
    | Coq_declaration'nt
    | Coq_declaration_list'nt
    | Coq_declaration_specifiers'nt
    | Coq_declaration_specifiers_typespec_opt'nt
    | Coq_declarator'nt
    | Coq_declarator_noattrend'nt
    | Coq_designation'nt
    | Coq_designator'nt
    | Coq_designator_list'nt
    | Coq_direct_abstract_declarator'nt
    | Coq_direct_declarator'nt
    | Coq_enum_specifier'nt
    | Coq_enumeration_constant'nt
    | Coq_enumerator'nt
    | Coq_enumerator_list'nt
    | Coq_equality_expression'nt
    | Coq_exclusive_OR_expression'nt
    | Coq_expression'nt
    | Coq_expression_statement'nt
    | Coq_external_declaration'nt
    | Coq_function_definition'nt
    | Coq_function_specifier'nt
    | Coq_gcc_attribute'nt
    | Coq_gcc_attribute_list'nt
    | Coq_gcc_attribute_word'nt
    | Coq_identifier_list'nt
    | Coq_inclusive_OR_expression'nt
    | Coq_init_declarator'nt
    | Coq_init_declarator_list'nt
    | Coq_initializer_list'nt
    | Coq_iteration_statement_statement_dangerous_'nt
    | Coq_iteration_statement_statement_safe_'nt
    | Coq_jump_statement'nt
    | Coq_labeled_statement_statement_dangerous_'nt
    | Coq_labeled_statement_statement_safe_'nt
    | Coq_logical_AND_expression'nt
    | Coq_logical_OR_expression'nt
    | Coq_multiplicative_expression'nt
    | Coq_parameter_declaration'nt
    | Coq_parameter_list'nt
    | Coq_parameter_type_list'nt
    | Coq_pointer'nt
    | Coq_postfix_expression'nt
    | Coq_primary_expression'nt
    | Coq_relational_expression'nt
    | Coq_selection_statement_dangerous'nt
    | Coq_selection_statement_safe'nt
    | Coq_shift_expression'nt
    | Coq_specifier_qualifier_list'nt
    | Coq_statement_dangerous'nt
    | Coq_statement_safe'nt
    | Coq_static_assert_declaration'nt
    | Coq_storage_class_specifier'nt
    | Coq_struct_declaration'nt
    | Coq_struct_declaration_list'nt
    | Coq_struct_declarator'nt
    | Coq_struct_declarator_list'nt
    | Coq_struct_or_union'nt
    | Coq_struct_or_union_specifier'nt
    | Coq_translation_unit'nt
    | Coq_translation_unit_file'nt
    | Coq_type_name'nt
    | Coq_type_qualifier'nt
    | Coq_type_qualifier_list'nt
    | Coq_type_qualifier_noattr'nt
    | Coq_type_specifier'nt
    | Coq_unary_expression'nt
    | Coq_unary_operator'nt

    type nonterminal = nonterminal'

    val nonterminalNum : nonterminal coq_Numbered

    val coq_NonTerminalAlph : nonterminal coq_Alphabet

    type symbol = Gram.symbol =
    | T of terminal
    | NT of nonterminal

    val symbol_rect :
      (terminal -> 'a1) -> (nonterminal -> 'a1) -> symbol -> 'a1

    val symbol_rec :
      (terminal -> 'a1) -> (nonterminal -> 'a1) -> symbol -> 'a1

    val coq_SymbolAlph : symbol coq_Alphabet

    type terminal_semantic_type = __

    type nonterminal_semantic_type = __

    type symbol_semantic_type = __

    type token = Coq__1.token

    val token_term : token -> terminal

    val token_sem : token -> symbol_semantic_type

    type production' = Gram.production' =
    | Prod'unary_operator'5
    | Prod'unary_operator'4
    | Prod'unary_operator'3
    | Prod'unary_operator'2
    | Prod'unary_operator'1
    | Prod'unary_operator'0
    | Prod'unary_expression'6
    | Prod'unary_expression'5
    | Prod'unary_expression'4
    | Prod'unary_expression'3
    | Prod'unary_expression'2
    | Prod'unary_expression'1
    | Prod'unary_expression'0
    | Prod'type_specifier'12
    | Prod'type_specifier'11
    | Prod'type_specifier'10
    | Prod'type_specifier'9
    | Prod'type_specifier'8
    | Prod'type_specifier'7
    | Prod'type_specifier'6
    | Prod'type_specifier'5
    | Prod'type_specifier'4
    | Prod'type_specifier'3
    | Prod'type_specifier'2
    | Prod'type_specifier'1
    | Prod'type_specifier'0
    | Prod'type_qualifier_noattr'2
    | Prod'type_qualifier_noattr'1
    | Prod'type_qualifier_noattr'0
    | Prod'type_qualifier_list'1
    | Prod'type_qualifier_list'0
    | Prod'type_qualifier'1
    | Prod'type_qualifier'0
    | Prod'type_name'1
    | Prod'type_name'0
    | Prod'translation_unit_file'1
    | Prod'translation_unit_file'0
    | Prod'translation_unit'3
    | Prod'translation_unit'2
    | Prod'translation_unit'1
    | Prod'translation_unit'0
    | Prod'struct_or_union_specifier'2
    | Prod'struct_or_union_specifier'1
    | Prod'struct_or_union_specifier'0
    | Prod'struct_or_union'1
    | Prod'struct_or_union'0
    | Prod'struct_declarator_list'1
    | Prod'struct_declarator_list'0
    | Prod'struct_declarator'2
    | Prod'struct_declarator'1
    | Prod'struct_declarator'0
    | Prod'struct_declaration_list'1
    | Prod'struct_declaration_list'0
    | Prod'struct_declaration'2
    | Prod'struct_declaration'1
    | Prod'struct_declaration'0
    | Prod'storage_class_specifier'4
    | Prod'storage_class_specifier'3
    | Prod'storage_class_specifier'2
    | Prod'storage_class_specifier'1
    | Prod'storage_class_specifier'0
    | Prod'static_assert_declaration'0
    | Prod'statement_safe'6
    | Prod'statement_safe'5
    | Prod'statement_safe'4
    | Prod'statement_safe'3
    | Prod'statement_safe'2
    | Prod'statement_safe'1
    | Prod'statement_safe'0
    | Prod'statement_dangerous'6
    | Prod'statement_dangerous'5
    | Prod'statement_dangerous'4
    | Prod'statement_dangerous'3
    | Prod'statement_dangerous'2
    | Prod'statement_dangerous'1
    | Prod'statement_dangerous'0
    | Prod'specifier_qualifier_list'3
    | Prod'specifier_qualifier_list'2
    | Prod'specifier_qualifier_list'1
    | Prod'specifier_qualifier_list'0
    | Prod'shift_expression'2
    | Prod'shift_expression'1
    | Prod'shift_expression'0
    | Prod'selection_statement_safe'1
    | Prod'selection_statement_safe'0
    | Prod'selection_statement_dangerous'2
    | Prod'selection_statement_dangerous'1
    | Prod'selection_statement_dangerous'0
    | Prod'relational_expression'4
    | Prod'relational_expression'3
    | Prod'relational_expression'2
    | Prod'relational_expression'1
    | Prod'relational_expression'0
    | Prod'primary_expression'3
    | Prod'primary_expression'2
    | Prod'primary_expression'1
    | Prod'primary_expression'0
    | Prod'postfix_expression'12
    | Prod'postfix_expression'11
    | Prod'postfix_expression'10
    | Prod'postfix_expression'9
    | Prod'postfix_expression'8
    | Prod'postfix_expression'7
    | Prod'postfix_expression'6
    | Prod'postfix_expression'5
    | Prod'postfix_expression'4
    | Prod'postfix_expression'3
    | Prod'postfix_expression'2
    | Prod'postfix_expression'1
    | Prod'postfix_expression'0
    | Prod'pointer'4
    | Prod'pointer'3
    | Prod'pointer'2
    | Prod'pointer'1
    | Prod'pointer'0
    | Prod'parameter_type_list'1
    | Prod'parameter_type_list'0
    | Prod'parameter_list'1
    | Prod'parameter_list'0
    | Prod'parameter_declaration'2
    | Prod'parameter_declaration'1
    | Prod'parameter_declaration'0
    | Prod'multiplicative_expression'3
    | Prod'multiplicative_expression'2
    | Prod'multiplicative_expression'1
    | Prod'multiplicative_expression'0
    | Prod'logical_OR_expression'1
    | Prod'logical_OR_expression'0
    | Prod'logical_AND_expression'1
    | Prod'logical_AND_expression'0
    | Prod'labeled_statement_statement_safe_'2
    | Prod'labeled_statement_statement_safe_'1
    | Prod'labeled_statement_statement_safe_'0
    | Prod'labeled_statement_statement_dangerous_'2
    | Prod'labeled_statement_statement_dangerous_'1
    | Prod'labeled_statement_statement_dangerous_'0
    | Prod'jump_statement'4
    | Prod'jump_statement'3
    | Prod'jump_statement'2
    | Prod'jump_statement'1
    | Prod'jump_statement'0
    | Prod'iteration_statement_statement_safe_'13
    | Prod'iteration_statement_statement_safe_'12
    | Prod'iteration_statement_statement_safe_'11
    | Prod'iteration_statement_statement_safe_'10
    | Prod'iteration_statement_statement_safe_'9
    | Prod'iteration_statement_statement_safe_'8
    | Prod'iteration_statement_statement_safe_'7
    | Prod'iteration_statement_statement_safe_'6
    | Prod'iteration_statement_statement_safe_'5
    | Prod'iteration_statement_statement_safe_'4
    | Prod'iteration_statement_statement_safe_'3
    | Prod'iteration_statement_statement_safe_'2
    | Prod'iteration_statement_statement_safe_'1
    | Prod'iteration_statement_statement_safe_'0
    | Prod'iteration_statement_statement_dangerous_'13
    | Prod'iteration_statement_statement_dangerous_'12
    | Prod'iteration_statement_statement_dangerous_'11
    | Prod'iteration_statement_statement_dangerous_'10
    | Prod'iteration_statement_statement_dangerous_'9
    | Prod'iteration_statement_statement_dangerous_'8
    | Prod'iteration_statement_statement_dangerous_'7
    | Prod'iteration_statement_statement_dangerous_'6
    | Prod'iteration_statement_statement_dangerous_'5
    | Prod'iteration_statement_statement_dangerous_'4
    | Prod'iteration_statement_statement_dangerous_'3
    | Prod'iteration_statement_statement_dangerous_'2
    | Prod'iteration_statement_statement_dangerous_'1
    | Prod'iteration_statement_statement_dangerous_'0
    | Prod'initializer_list'3
    | Prod'initializer_list'2
    | Prod'initializer_list'1
    | Prod'initializer_list'0
    | Prod'init_declarator_list'1
    | Prod'init_declarator_list'0
    | Prod'init_declarator'1
    | Prod'init_declarator'0
    | Prod'inclusive_OR_expression'1
    | Prod'inclusive_OR_expression'0
    | Prod'identifier_list'1
    | Prod'identifier_list'0
    | Prod'gcc_attribute_word'2
    | Prod'gcc_attribute_word'1
    | Prod'gcc_attribute_word'0
    | Prod'gcc_attribute_list'1
    | Prod'gcc_attribute_list'0
    | Prod'gcc_attribute'3
    | Prod'gcc_attribute'2
    | Prod'gcc_attribute'1
    | Prod'gcc_attribute'0
    | Prod'function_specifier'1
    | Prod'function_specifier'0
    | Prod'function_definition'1
    | Prod'function_definition'0
    | Prod'external_declaration'2
    | Prod'external_declaration'1
    | Prod'external_declaration'0
    | Prod'expression_statement'1
    | Prod'expression_statement'0
    | Prod'expression'1
    | Prod'expression'0
    | Prod'exclusive_OR_expression'1
    | Prod'exclusive_OR_expression'0
    | Prod'equality_expression'2
    | Prod'equality_expression'1
    | Prod'equality_expression'0
    | Prod'enumerator_list'1
    | Prod'enumerator_list'0
    | Prod'enumerator'1
    | Prod'enumerator'0
    | Prod'enumeration_constant'0
    | Prod'enum_specifier'4
    | Prod'enum_specifier'3
    | Prod'enum_specifier'2
    | Prod'enum_specifier'1
    | Prod'enum_specifier'0
    | Prod'direct_declarator'8
    | Prod'direct_declarator'7
    | Prod'direct_declarator'6
    | Prod'direct_declarator'5
    | Prod'direct_declarator'4
    | Prod'direct_declarator'3
    | Prod'direct_declarator'2
    | Prod'direct_declarator'1
    | Prod'direct_declarator'0
    | Prod'direct_abstract_declarator'12
    | Prod'direct_abstract_declarator'11
    | Prod'direct_abstract_declarator'10
    | Prod'direct_abstract_declarator'9
    | Prod'direct_abstract_declarator'8
    | Prod'direct_abstract_declarator'7
    | Prod'direct_abstract_declarator'6
    | Prod'direct_abstract_declarator'5
    | Prod'direct_abstract_declarator'4
    | Prod'direct_abstract_declarator'3
    | Prod'direct_abstract_declarator'2
    | Prod'direct_abstract_declarator'1
    | Prod'direct_abstract_declarator'0
    | Prod'designator_list'1
    | Prod'designator_list'0
    | Prod'designator'1
    | Prod'designator'0
    | Prod'designation'0
    | Prod'declarator_noattrend'1
    | Prod'declarator_noattrend'0
    | Prod'declarator'0
    | Prod'declaration_specifiers_typespec_opt'4
    | Prod'declaration_specifiers_typespec_opt'3
    | Prod'declaration_specifiers_typespec_opt'2
    | Prod'declaration_specifiers_typespec_opt'1
    | Prod'declaration_specifiers_typespec_opt'0
    | Prod'declaration_specifiers'4
    | Prod'declaration_specifiers'3
    | Prod'declaration_specifiers'2
    | Prod'declaration_specifiers'1
    | Prod'declaration_specifiers'0
    | Prod'declaration_list'1
    | Prod'declaration_list'0
    | Prod'declaration'2
    | Prod'declaration'1
    | Prod'declaration'0
    | Prod'constant_expression'0
    | Prod'conditional_expression'1
    | Prod'conditional_expression'0
    | Prod'compound_statement'1
    | Prod'compound_statement'0
    | Prod'cast_expression'1
    | Prod'cast_expression'0
    | Prod'c_initializer'2
    | Prod'c_initializer'1
    | Prod'c_initializer'0
    | Prod'block_item_list'1
    | Prod'block_item_list'0
    | Prod'block_item'2
    | Prod'block_item'1
    | Prod'block_item'0
    | Prod'attribute_specifier_list'1
    | Prod'attribute_specifier_list'0
    | Prod'attribute_specifier'3
    | Prod'attribute_specifier'2
    | Prod'attribute_specifier'1
    | Prod'attribute_specifier'0
    | Prod'assignment_operator'10
    | Prod'assignment_operator'9
    | Prod'assignment_operator'8
    | Prod'assignment_operator'7
    | Prod'assignment_operator'6
    | Prod'assignment_operator'5
    | Prod'assignment_operator'4
    | Prod'assignment_operator'3
    | Prod'assignment_operator'2
    | Prod'assignment_operator'1
    | Prod'assignment_operator'0
    | Prod'assignment_expression'1
    | Prod'assignment_expression'0
    | Prod'asm_statement'0
    | Prod'asm_operands_ne'1
    | Prod'asm_operands_ne'0
    | Prod'asm_operands'1
    | Prod'asm_operands'0
    | Prod'asm_operand'0
    | Prod'asm_op_name'1
    | Prod'asm_op_name'0
    | Prod'asm_flags'1
    | Prod'asm_flags'0
    | Prod'asm_attributes'2
    | Prod'asm_attributes'1
    | Prod'asm_attributes'0
    | Prod'asm_arguments'3
    | Prod'asm_arguments'2
    | Prod'asm_arguments'1
    | Prod'asm_arguments'0
    | Prod'argument_expression_list'1
    | Prod'argument_expression_list'0
    | Prod'additive_expression'2
    | Prod'additive_expression'1
    | Prod'additive_expression'0
    | Prod'abstract_declarator'2
    | Prod'abstract_declarator'1
    | Prod'abstract_declarator'0
    | Prod'AND_expression'1
    | Prod'AND_expression'0

    type production = production'

    val productionNum : production coq_Numbered

    val coq_ProductionAlph : production coq_Alphabet

    val prod_contents :
      production -> (nonterminal * symbol list, symbol_semantic_type
      arrows_right) sigT

    val prod_lhs : production -> nonterminal

    val prod_rhs_rev : production -> symbol list

    val prod_action : production -> symbol_semantic_type arrows_right

    type parse_tree = Gram.parse_tree =
    | Terminal_pt of token
    | Non_terminal_pt of production * token list * parse_tree_list
    and parse_tree_list = Gram.parse_tree_list =
    | Nil_ptl
    | Cons_ptl of symbol list * token list * parse_tree_list * symbol
       * token list * parse_tree

    val parse_tree_rect :
      (token -> 'a1) -> (production -> token list -> parse_tree_list -> 'a1)
      -> symbol -> token list -> parse_tree -> 'a1

    val parse_tree_rec :
      (token -> 'a1) -> (production -> token list -> parse_tree_list -> 'a1)
      -> symbol -> token list -> parse_tree -> 'a1

    val parse_tree_list_rect :
      'a1 -> (symbol list -> token list -> parse_tree_list -> 'a1 -> symbol
      -> token list -> parse_tree -> 'a1) -> symbol list -> token list ->
      parse_tree_list -> 'a1

    val parse_tree_list_rec :
      'a1 -> (symbol list -> token list -> parse_tree_list -> 'a1 -> symbol
      -> token list -> parse_tree -> 'a1) -> symbol list -> token list ->
      parse_tree_list -> 'a1

    val pt_sem : symbol -> token list -> parse_tree -> symbol_semantic_type

    val ptl_sem :
      symbol list -> token list -> parse_tree_list -> 'a1 arrows_right -> 'a1

    val pt_size : symbol -> token list -> parse_tree -> nat

    val ptl_size : symbol list -> token list -> parse_tree_list -> nat
   end

  module GramDefs :
   sig
    type terminal' = Coq__2.terminal' =
    | ADD_ASSIGN't
    | ALIGNAS't
    | ALIGNOF't
    | AND't
    | ANDAND't
    | AND_ASSIGN't
    | ASM't
    | ATTRIBUTE't
    | AUTO't
    | BANG't
    | BAR't
    | BARBAR't
    | BREAK't
    | BUILTIN_OFFSETOF't
    | BUILTIN_VA_ARG't
    | CASE't
    | CHAR't
    | COLON't
    | COMMA't
    | CONST't
    | CONSTANT't
    | CONTINUE't
    | DEC't
    | DEFAULT't
    | DIV_ASSIGN't
    | DO't
    | DOT't
    | DOUBLE't
    | ELLIPSIS't
    | ELSE't
    | ENUM't
    | EOF't
    | EQ't
    | EQEQ't
    | EXTERN't
    | FLOAT't
    | FOR't
    | GEQ't
    | GOTO't
    | GT't
    | HAT't
    | IF_'t
    | INC't
    | INLINE't
    | INT't
    | LBRACE't
    | LBRACK't
    | LEFT't
    | LEFT_ASSIGN't
    | LEQ't
    | LONG't
    | LPAREN't
    | LT't
    | MINUS't
    | MOD_ASSIGN't
    | MUL_ASSIGN't
    | NEQ't
    | NORETURN't
    | OR_ASSIGN't
    | OTHER_NAME't
    | PACKED't
    | PERCENT't
    | PLUS't
    | PRAGMA't
    | PTR't
    | QUESTION't
    | RBRACE't
    | RBRACK't
    | REGISTER't
    | RESTRICT't
    | RETURN't
    | RIGHT't
    | RIGHT_ASSIGN't
    | RPAREN't
    | SEMICOLON't
    | SHORT't
    | SIGNED't
    | SIZEOF't
    | SLASH't
    | STAR't
    | STATIC't
    | STATIC_ASSERT't
    | STRING_LITERAL't
    | STRUCT't
    | SUB_ASSIGN't
    | SWITCH't
    | TILDE't
    | TYPEDEF't
    | TYPEDEF_NAME't
    | UNDERSCORE_BOOL't
    | UNION't
    | UNSIGNED't
    | VAR_NAME't
    | VOID't
    | VOLATILE't
    | WHILE't
    | XOR_ASSIGN't

    type terminal = terminal'

    val terminalNum : terminal coq_Numbered

    val coq_TerminalAlph : terminal coq_Alphabet

    type nonterminal' = Coq__2.nonterminal' =
    | AND_expression'nt
    | Coq_abstract_declarator'nt
    | Coq_additive_expression'nt
    | Coq_argument_expression_list'nt
    | Coq_asm_arguments'nt
    | Coq_asm_attributes'nt
    | Coq_asm_flags'nt
    | Coq_asm_op_name'nt
    | Coq_asm_operand'nt
    | Coq_asm_operands'nt
    | Coq_asm_operands_ne'nt
    | Coq_asm_statement'nt
    | Coq_assignment_expression'nt
    | Coq_assignment_operator'nt
    | Coq_attribute_specifier'nt
    | Coq_attribute_specifier_list'nt
    | Coq_block_item'nt
    | Coq_block_item_list'nt
    | Coq_c_initializer'nt
    | Coq_cast_expression'nt
    | Coq_compound_statement'nt
    | Coq_conditional_expression'nt
    | Coq_constant_expression'nt
    | Coq_declaration'nt
    | Coq_declaration_list'nt
    | Coq_declaration_specifiers'nt
    | Coq_declaration_specifiers_typespec_opt'nt
    | Coq_declarator'nt
    | Coq_declarator_noattrend'nt
    | Coq_designation'nt
    | Coq_designator'nt
    | Coq_designator_list'nt
    | Coq_direct_abstract_declarator'nt
    | Coq_direct_declarator'nt
    | Coq_enum_specifier'nt
    | Coq_enumeration_constant'nt
    | Coq_enumerator'nt
    | Coq_enumerator_list'nt
    | Coq_equality_expression'nt
    | Coq_exclusive_OR_expression'nt
    | Coq_expression'nt
    | Coq_expression_statement'nt
    | Coq_external_declaration'nt
    | Coq_function_definition'nt
    | Coq_function_specifier'nt
    | Coq_gcc_attribute'nt
    | Coq_gcc_attribute_list'nt
    | Coq_gcc_attribute_word'nt
    | Coq_identifier_list'nt
    | Coq_inclusive_OR_expression'nt
    | Coq_init_declarator'nt
    | Coq_init_declarator_list'nt
    | Coq_initializer_list'nt
    | Coq_iteration_statement_statement_dangerous_'nt
    | Coq_iteration_statement_statement_safe_'nt
    | Coq_jump_statement'nt
    | Coq_labeled_statement_statement_dangerous_'nt
    | Coq_labeled_statement_statement_safe_'nt
    | Coq_logical_AND_expression'nt
    | Coq_logical_OR_expression'nt
    | Coq_multiplicative_expression'nt
    | Coq_parameter_declaration'nt
    | Coq_parameter_list'nt
    | Coq_parameter_type_list'nt
    | Coq_pointer'nt
    | Coq_postfix_expression'nt
    | Coq_primary_expression'nt
    | Coq_relational_expression'nt
    | Coq_selection_statement_dangerous'nt
    | Coq_selection_statement_safe'nt
    | Coq_shift_expression'nt
    | Coq_specifier_qualifier_list'nt
    | Coq_statement_dangerous'nt
    | Coq_statement_safe'nt
    | Coq_static_assert_declaration'nt
    | Coq_storage_class_specifier'nt
    | Coq_struct_declaration'nt
    | Coq_struct_declaration_list'nt
    | Coq_struct_declarator'nt
    | Coq_struct_declarator_list'nt
    | Coq_struct_or_union'nt
    | Coq_struct_or_union_specifier'nt
    | Coq_translation_unit'nt
    | Coq_translation_unit_file'nt
    | Coq_type_name'nt
    | Coq_type_qualifier'nt
    | Coq_type_qualifier_list'nt
    | Coq_type_qualifier_noattr'nt
    | Coq_type_specifier'nt
    | Coq_unary_expression'nt
    | Coq_unary_operator'nt

    type nonterminal = nonterminal'

    val nonterminalNum : nonterminal coq_Numbered

    val coq_NonTerminalAlph : nonterminal coq_Alphabet

    type symbol = Coq__2.symbol =
    | T of terminal
    | NT of nonterminal

    val symbol_rect :
      (terminal -> 'a1) -> (nonterminal -> 'a1) -> symbol -> 'a1

    val symbol_rec :
      (terminal -> 'a1) -> (nonterminal -> 'a1) -> symbol -> 'a1

    val coq_SymbolAlph : symbol coq_Alphabet

    type terminal_semantic_type = __

    type nonterminal_semantic_type = __

    type symbol_semantic_type = __

    type token = Coq__1.token

    val token_term : token -> terminal

    val token_sem : token -> symbol_semantic_type

    type production' = Coq__2.production' =
    | Prod'unary_operator'5
    | Prod'unary_operator'4
    | Prod'unary_operator'3
    | Prod'unary_operator'2
    | Prod'unary_operator'1
    | Prod'unary_operator'0
    | Prod'unary_expression'6
    | Prod'unary_expression'5
    | Prod'unary_expression'4
    | Prod'unary_expression'3
    | Prod'unary_expression'2
    | Prod'unary_expression'1
    | Prod'unary_expression'0
    | Prod'type_specifier'12
    | Prod'type_specifier'11
    | Prod'type_specifier'10
    | Prod'type_specifier'9
    | Prod'type_specifier'8
    | Prod'type_specifier'7
    | Prod'type_specifier'6
    | Prod'type_specifier'5
    | Prod'type_specifier'4
    | Prod'type_specifier'3
    | Prod'type_specifier'2
    | Prod'type_specifier'1
    | Prod'type_specifier'0
    | Prod'type_qualifier_noattr'2
    | Prod'type_qualifier_noattr'1
    | Prod'type_qualifier_noattr'0
    | Prod'type_qualifier_list'1
    | Prod'type_qualifier_list'0
    | Prod'type_qualifier'1
    | Prod'type_qualifier'0
    | Prod'type_name'1
    | Prod'type_name'0
    | Prod'translation_unit_file'1
    | Prod'translation_unit_file'0
    | Prod'translation_unit'3
    | Prod'translation_unit'2
    | Prod'translation_unit'1
    | Prod'translation_unit'0
    | Prod'struct_or_union_specifier'2
    | Prod'struct_or_union_specifier'1
    | Prod'struct_or_union_specifier'0
    | Prod'struct_or_union'1
    | Prod'struct_or_union'0
    | Prod'struct_declarator_list'1
    | Prod'struct_declarator_list'0
    | Prod'struct_declarator'2
    | Prod'struct_declarator'1
    | Prod'struct_declarator'0
    | Prod'struct_declaration_list'1
    | Prod'struct_declaration_list'0
    | Prod'struct_declaration'2
    | Prod'struct_declaration'1
    | Prod'struct_declaration'0
    | Prod'storage_class_specifier'4
    | Prod'storage_class_specifier'3
    | Prod'storage_class_specifier'2
    | Prod'storage_class_specifier'1
    | Prod'storage_class_specifier'0
    | Prod'static_assert_declaration'0
    | Prod'statement_safe'6
    | Prod'statement_safe'5
    | Prod'statement_safe'4
    | Prod'statement_safe'3
    | Prod'statement_safe'2
    | Prod'statement_safe'1
    | Prod'statement_safe'0
    | Prod'statement_dangerous'6
    | Prod'statement_dangerous'5
    | Prod'statement_dangerous'4
    | Prod'statement_dangerous'3
    | Prod'statement_dangerous'2
    | Prod'statement_dangerous'1
    | Prod'statement_dangerous'0
    | Prod'specifier_qualifier_list'3
    | Prod'specifier_qualifier_list'2
    | Prod'specifier_qualifier_list'1
    | Prod'specifier_qualifier_list'0
    | Prod'shift_expression'2
    | Prod'shift_expression'1
    | Prod'shift_expression'0
    | Prod'selection_statement_safe'1
    | Prod'selection_statement_safe'0
    | Prod'selection_statement_dangerous'2
    | Prod'selection_statement_dangerous'1
    | Prod'selection_statement_dangerous'0
    | Prod'relational_expression'4
    | Prod'relational_expression'3
    | Prod'relational_expression'2
    | Prod'relational_expression'1
    | Prod'relational_expression'0
    | Prod'primary_expression'3
    | Prod'primary_expression'2
    | Prod'primary_expression'1
    | Prod'primary_expression'0
    | Prod'postfix_expression'12
    | Prod'postfix_expression'11
    | Prod'postfix_expression'10
    | Prod'postfix_expression'9
    | Prod'postfix_expression'8
    | Prod'postfix_expression'7
    | Prod'postfix_expression'6
    | Prod'postfix_expression'5
    | Prod'postfix_expression'4
    | Prod'postfix_expression'3
    | Prod'postfix_expression'2
    | Prod'postfix_expression'1
    | Prod'postfix_expression'0
    | Prod'pointer'4
    | Prod'pointer'3
    | Prod'pointer'2
    | Prod'pointer'1
    | Prod'pointer'0
    | Prod'parameter_type_list'1
    | Prod'parameter_type_list'0
    | Prod'parameter_list'1
    | Prod'parameter_list'0
    | Prod'parameter_declaration'2
    | Prod'parameter_declaration'1
    | Prod'parameter_declaration'0
    | Prod'multiplicative_expression'3
    | Prod'multiplicative_expression'2
    | Prod'multiplicative_expression'1
    | Prod'multiplicative_expression'0
    | Prod'logical_OR_expression'1
    | Prod'logical_OR_expression'0
    | Prod'logical_AND_expression'1
    | Prod'logical_AND_expression'0
    | Prod'labeled_statement_statement_safe_'2
    | Prod'labeled_statement_statement_safe_'1
    | Prod'labeled_statement_statement_safe_'0
    | Prod'labeled_statement_statement_dangerous_'2
    | Prod'labeled_statement_statement_dangerous_'1
    | Prod'labeled_statement_statement_dangerous_'0
    | Prod'jump_statement'4
    | Prod'jump_statement'3
    | Prod'jump_statement'2
    | Prod'jump_statement'1
    | Prod'jump_statement'0
    | Prod'iteration_statement_statement_safe_'13
    | Prod'iteration_statement_statement_safe_'12
    | Prod'iteration_statement_statement_safe_'11
    | Prod'iteration_statement_statement_safe_'10
    | Prod'iteration_statement_statement_safe_'9
    | Prod'iteration_statement_statement_safe_'8
    | Prod'iteration_statement_statement_safe_'7
    | Prod'iteration_statement_statement_safe_'6
    | Prod'iteration_statement_statement_safe_'5
    | Prod'iteration_statement_statement_safe_'4
    | Prod'iteration_statement_statement_safe_'3
    | Prod'iteration_statement_statement_safe_'2
    | Prod'iteration_statement_statement_safe_'1
    | Prod'iteration_statement_statement_safe_'0
    | Prod'iteration_statement_statement_dangerous_'13
    | Prod'iteration_statement_statement_dangerous_'12
    | Prod'iteration_statement_statement_dangerous_'11
    | Prod'iteration_statement_statement_dangerous_'10
    | Prod'iteration_statement_statement_dangerous_'9
    | Prod'iteration_statement_statement_dangerous_'8
    | Prod'iteration_statement_statement_dangerous_'7
    | Prod'iteration_statement_statement_dangerous_'6
    | Prod'iteration_statement_statement_dangerous_'5
    | Prod'iteration_statement_statement_dangerous_'4
    | Prod'iteration_statement_statement_dangerous_'3
    | Prod'iteration_statement_statement_dangerous_'2
    | Prod'iteration_statement_statement_dangerous_'1
    | Prod'iteration_statement_statement_dangerous_'0
    | Prod'initializer_list'3
    | Prod'initializer_list'2
    | Prod'initializer_list'1
    | Prod'initializer_list'0
    | Prod'init_declarator_list'1
    | Prod'init_declarator_list'0
    | Prod'init_declarator'1
    | Prod'init_declarator'0
    | Prod'inclusive_OR_expression'1
    | Prod'inclusive_OR_expression'0
    | Prod'identifier_list'1
    | Prod'identifier_list'0
    | Prod'gcc_attribute_word'2
    | Prod'gcc_attribute_word'1
    | Prod'gcc_attribute_word'0
    | Prod'gcc_attribute_list'1
    | Prod'gcc_attribute_list'0
    | Prod'gcc_attribute'3
    | Prod'gcc_attribute'2
    | Prod'gcc_attribute'1
    | Prod'gcc_attribute'0
    | Prod'function_specifier'1
    | Prod'function_specifier'0
    | Prod'function_definition'1
    | Prod'function_definition'0
    | Prod'external_declaration'2
    | Prod'external_declaration'1
    | Prod'external_declaration'0
    | Prod'expression_statement'1
    | Prod'expression_statement'0
    | Prod'expression'1
    | Prod'expression'0
    | Prod'exclusive_OR_expression'1
    | Prod'exclusive_OR_expression'0
    | Prod'equality_expression'2
    | Prod'equality_expression'1
    | Prod'equality_expression'0
    | Prod'enumerator_list'1
    | Prod'enumerator_list'0
    | Prod'enumerator'1
    | Prod'enumerator'0
    | Prod'enumeration_constant'0
    | Prod'enum_specifier'4
    | Prod'enum_specifier'3
    | Prod'enum_specifier'2
    | Prod'enum_specifier'1
    | Prod'enum_specifier'0
    | Prod'direct_declarator'8
    | Prod'direct_declarator'7
    | Prod'direct_declarator'6
    | Prod'direct_declarator'5
    | Prod'direct_declarator'4
    | Prod'direct_declarator'3
    | Prod'direct_declarator'2
    | Prod'direct_declarator'1
    | Prod'direct_declarator'0
    | Prod'direct_abstract_declarator'12
    | Prod'direct_abstract_declarator'11
    | Prod'direct_abstract_declarator'10
    | Prod'direct_abstract_declarator'9
    | Prod'direct_abstract_declarator'8
    | Prod'direct_abstract_declarator'7
    | Prod'direct_abstract_declarator'6
    | Prod'direct_abstract_declarator'5
    | Prod'direct_abstract_declarator'4
    | Prod'direct_abstract_declarator'3
    | Prod'direct_abstract_declarator'2
    | Prod'direct_abstract_declarator'1
    | Prod'direct_abstract_declarator'0
    | Prod'designator_list'1
    | Prod'designator_list'0
    | Prod'designator'1
    | Prod'designator'0
    | Prod'designation'0
    | Prod'declarator_noattrend'1
    | Prod'declarator_noattrend'0
    | Prod'declarator'0
    | Prod'declaration_specifiers_typespec_opt'4
    | Prod'declaration_specifiers_typespec_opt'3
    | Prod'declaration_specifiers_typespec_opt'2
    | Prod'declaration_specifiers_typespec_opt'1
    | Prod'declaration_specifiers_typespec_opt'0
    | Prod'declaration_specifiers'4
    | Prod'declaration_specifiers'3
    | Prod'declaration_specifiers'2
    | Prod'declaration_specifiers'1
    | Prod'declaration_specifiers'0
    | Prod'declaration_list'1
    | Prod'declaration_list'0
    | Prod'declaration'2
    | Prod'declaration'1
    | Prod'declaration'0
    | Prod'constant_expression'0
    | Prod'conditional_expression'1
    | Prod'conditional_expression'0
    | Prod'compound_statement'1
    | Prod'compound_statement'0
    | Prod'cast_expression'1
    | Prod'cast_expression'0
    | Prod'c_initializer'2
    | Prod'c_initializer'1
    | Prod'c_initializer'0
    | Prod'block_item_list'1
    | Prod'block_item_list'0
    | Prod'block_item'2
    | Prod'block_item'1
    | Prod'block_item'0
    | Prod'attribute_specifier_list'1
    | Prod'attribute_specifier_list'0
    | Prod'attribute_specifier'3
    | Prod'attribute_specifier'2
    | Prod'attribute_specifier'1
    | Prod'attribute_specifier'0
    | Prod'assignment_operator'10
    | Prod'assignment_operator'9
    | Prod'assignment_operator'8
    | Prod'assignment_operator'7
    | Prod'assignment_operator'6
    | Prod'assignment_operator'5
    | Prod'assignment_operator'4
    | Prod'assignment_operator'3
    | Prod'assignment_operator'2
    | Prod'assignment_operator'1
    | Prod'assignment_operator'0
    | Prod'assignment_expression'1
    | Prod'assignment_expression'0
    | Prod'asm_statement'0
    | Prod'asm_operands_ne'1
    | Prod'asm_operands_ne'0
    | Prod'asm_operands'1
    | Prod'asm_operands'0
    | Prod'asm_operand'0
    | Prod'asm_op_name'1
    | Prod'asm_op_name'0
    | Prod'asm_flags'1
    | Prod'asm_flags'0
    | Prod'asm_attributes'2
    | Prod'asm_attributes'1
    | Prod'asm_attributes'0
    | Prod'asm_arguments'3
    | Prod'asm_arguments'2
    | Prod'asm_arguments'1
    | Prod'asm_arguments'0
    | Prod'argument_expression_list'1
    | Prod'argument_expression_list'0
    | Prod'additive_expression'2
    | Prod'additive_expression'1
    | Prod'additive_expression'0
    | Prod'abstract_declarator'2
    | Prod'abstract_declarator'1
    | Prod'abstract_declarator'0
    | Prod'AND_expression'1
    | Prod'AND_expression'0

    type production = production'

    val productionNum : production coq_Numbered

    val coq_ProductionAlph : production coq_Alphabet

    val prod_contents :
      production -> (nonterminal * symbol list, symbol_semantic_type
      arrows_right) sigT

    val prod_lhs : production -> nonterminal

    val prod_rhs_rev : production -> symbol list

    val prod_action : production -> symbol_semantic_type arrows_right

    type parse_tree = Coq__2.parse_tree =
    | Terminal_pt of token
    | Non_terminal_pt of production * token list * parse_tree_list
    and parse_tree_list = Coq__2.parse_tree_list =
    | Nil_ptl
    | Cons_ptl of symbol list * token list * parse_tree_list * symbol
       * token list * parse_tree

    val parse_tree_rect :
      (token -> 'a1) -> (production -> token list -> parse_tree_list -> 'a1)
      -> symbol -> token list -> parse_tree -> 'a1

    val parse_tree_rec :
      (token -> 'a1) -> (production -> token list -> parse_tree_list -> 'a1)
      -> symbol -> token list -> parse_tree -> 'a1

    val parse_tree_list_rect :
      'a1 -> (symbol list -> token list -> parse_tree_list -> 'a1 -> symbol
      -> token list -> parse_tree -> 'a1) -> symbol list -> token list ->
      parse_tree_list -> 'a1

    val parse_tree_list_rec :
      'a1 -> (symbol list -> token list -> parse_tree_list -> 'a1 -> symbol
      -> token list -> parse_tree -> 'a1) -> symbol list -> token list ->
      parse_tree_list -> 'a1

    val pt_sem : symbol -> token list -> parse_tree -> symbol_semantic_type

    val ptl_sem :
      symbol list -> token list -> parse_tree_list -> 'a1 arrows_right -> 'a1

    val pt_size : symbol -> token list -> parse_tree -> nat

    val ptl_size : symbol list -> token list -> parse_tree_list -> nat
   end

  val nullable_nterm : Coq__2.nonterminal -> bool

  val first_nterm : Coq__2.nonterminal -> Coq__2.terminal list

  type noninitstate' =
  | Nis'622
  | Nis'621
  | Nis'620
  | Nis'619
  | Nis'618
  | Nis'617
  | Nis'616
  | Nis'615
  | Nis'614
  | Nis'613
  | Nis'612
  | Nis'611
  | Nis'610
  | Nis'609
  | Nis'608
  | Nis'607
  | Nis'606
  | Nis'605
  | Nis'604
  | Nis'603
  | Nis'602
  | Nis'601
  | Nis'600
  | Nis'599
  | Nis'598
  | Nis'597
  | Nis'596
  | Nis'595
  | Nis'594
  | Nis'593
  | Nis'592
  | Nis'591
  | Nis'590
  | Nis'589
  | Nis'588
  | Nis'587
  | Nis'586
  | Nis'585
  | Nis'584
  | Nis'583
  | Nis'582
  | Nis'581
  | Nis'580
  | Nis'579
  | Nis'578
  | Nis'577
  | Nis'576
  | Nis'575
  | Nis'574
  | Nis'573
  | Nis'572
  | Nis'571
  | Nis'570
  | Nis'569
  | Nis'568
  | Nis'567
  | Nis'566
  | Nis'565
  | Nis'564
  | Nis'563
  | Nis'562
  | Nis'561
  | Nis'560
  | Nis'559
  | Nis'558
  | Nis'557
  | Nis'556
  | Nis'555
  | Nis'554
  | Nis'553
  | Nis'552
  | Nis'551
  | Nis'550
  | Nis'549
  | Nis'548
  | Nis'547
  | Nis'546
  | Nis'545
  | Nis'544
  | Nis'543
  | Nis'542
  | Nis'541
  | Nis'540
  | Nis'539
  | Nis'538
  | Nis'537
  | Nis'536
  | Nis'535
  | Nis'534
  | Nis'533
  | Nis'532
  | Nis'531
  | Nis'530
  | Nis'529
  | Nis'528
  | Nis'527
  | Nis'526
  | Nis'525
  | Nis'524
  | Nis'523
  | Nis'522
  | Nis'521
  | Nis'520
  | Nis'519
  | Nis'518
  | Nis'517
  | Nis'516
  | Nis'515
  | Nis'514
  | Nis'513
  | Nis'512
  | Nis'511
  | Nis'510
  | Nis'509
  | Nis'508
  | Nis'507
  | Nis'506
  | Nis'505
  | Nis'504
  | Nis'503
  | Nis'502
  | Nis'501
  | Nis'500
  | Nis'499
  | Nis'498
  | Nis'497
  | Nis'496
  | Nis'495
  | Nis'494
  | Nis'493
  | Nis'492
  | Nis'491
  | Nis'490
  | Nis'489
  | Nis'488
  | Nis'487
  | Nis'486
  | Nis'485
  | Nis'484
  | Nis'483
  | Nis'482
  | Nis'481
  | Nis'480
  | Nis'479
  | Nis'478
  | Nis'477
  | Nis'476
  | Nis'475
  | Nis'474
  | Nis'473
  | Nis'472
  | Nis'471
  | Nis'470
  | Nis'469
  | Nis'468
  | Nis'467
  | Nis'466
  | Nis'465
  | Nis'464
  | Nis'463
  | Nis'462
  | Nis'461
  | Nis'460
  | Nis'459
  | Nis'458
  | Nis'457
  | Nis'456
  | Nis'455
  | Nis'454
  | Nis'453
  | Nis'452
  | Nis'451
  | Nis'450
  | Nis'449
  | Nis'448
  | Nis'447
  | Nis'446
  | Nis'445
  | Nis'444
  | Nis'443
  | Nis'442
  | Nis'441
  | Nis'440
  | Nis'439
  | Nis'438
  | Nis'437
  | Nis'436
  | Nis'435
  | Nis'434
  | Nis'433
  | Nis'432
  | Nis'431
  | Nis'430
  | Nis'429
  | Nis'428
  | Nis'427
  | Nis'426
  | Nis'425
  | Nis'424
  | Nis'423
  | Nis'422
  | Nis'421
  | Nis'420
  | Nis'419
  | Nis'418
  | Nis'417
  | Nis'416
  | Nis'415
  | Nis'414
  | Nis'413
  | Nis'412
  | Nis'411
  | Nis'410
  | Nis'409
  | Nis'408
  | Nis'407
  | Nis'406
  | Nis'405
  | Nis'404
  | Nis'403
  | Nis'402
  | Nis'401
  | Nis'400
  | Nis'399
  | Nis'398
  | Nis'397
  | Nis'396
  | Nis'395
  | Nis'394
  | Nis'393
  | Nis'392
  | Nis'391
  | Nis'390
  | Nis'389
  | Nis'388
  | Nis'387
  | Nis'386
  | Nis'385
  | Nis'384
  | Nis'383
  | Nis'382
  | Nis'381
  | Nis'380
  | Nis'379
  | Nis'378
  | Nis'377
  | Nis'376
  | Nis'375
  | Nis'374
  | Nis'373
  | Nis'372
  | Nis'371
  | Nis'370
  | Nis'369
  | Nis'368
  | Nis'366
  | Nis'365
  | Nis'364
  | Nis'363
  | Nis'362
  | Nis'361
  | Nis'360
  | Nis'359
  | Nis'358
  | Nis'357
  | Nis'356
  | Nis'355
  | Nis'354
  | Nis'353
  | Nis'352
  | Nis'351
  | Nis'350
  | Nis'349
  | Nis'348
  | Nis'347
  | Nis'346
  | Nis'345
  | Nis'344
  | Nis'343
  | Nis'342
  | Nis'341
  | Nis'340
  | Nis'339
  | Nis'338
  | Nis'337
  | Nis'336
  | Nis'335
  | Nis'334
  | Nis'333
  | Nis'332
  | Nis'331
  | Nis'330
  | Nis'329
  | Nis'328
  | Nis'327
  | Nis'326
  | Nis'325
  | Nis'324
  | Nis'323
  | Nis'322
  | Nis'321
  | Nis'320
  | Nis'319
  | Nis'318
  | Nis'317
  | Nis'316
  | Nis'315
  | Nis'314
  | Nis'313
  | Nis'312
  | Nis'311
  | Nis'310
  | Nis'309
  | Nis'308
  | Nis'307
  | Nis'306
  | Nis'305
  | Nis'304
  | Nis'303
  | Nis'302
  | Nis'301
  | Nis'300
  | Nis'299
  | Nis'298
  | Nis'297
  | Nis'296
  | Nis'295
  | Nis'294
  | Nis'293
  | Nis'292
  | Nis'291
  | Nis'290
  | Nis'289
  | Nis'288
  | Nis'287
  | Nis'286
  | Nis'285
  | Nis'284
  | Nis'283
  | Nis'282
  | Nis'281
  | Nis'280
  | Nis'279
  | Nis'278
  | Nis'277
  | Nis'276
  | Nis'275
  | Nis'274
  | Nis'273
  | Nis'272
  | Nis'271
  | Nis'270
  | Nis'269
  | Nis'268
  | Nis'267
  | Nis'266
  | Nis'265
  | Nis'264
  | Nis'263
  | Nis'262
  | Nis'261
  | Nis'260
  | Nis'259
  | Nis'258
  | Nis'257
  | Nis'256
  | Nis'255
  | Nis'254
  | Nis'253
  | Nis'252
  | Nis'251
  | Nis'250
  | Nis'249
  | Nis'248
  | Nis'247
  | Nis'246
  | Nis'245
  | Nis'244
  | Nis'243
  | Nis'242
  | Nis'241
  | Nis'240
  | Nis'239
  | Nis'238
  | Nis'237
  | Nis'236
  | Nis'235
  | Nis'234
  | Nis'233
  | Nis'232
  | Nis'231
  | Nis'230
  | Nis'229
  | Nis'228
  | Nis'227
  | Nis'226
  | Nis'225
  | Nis'224
  | Nis'223
  | Nis'222
  | Nis'221
  | Nis'220
  | Nis'219
  | Nis'218
  | Nis'217
  | Nis'216
  | Nis'215
  | Nis'214
  | Nis'213
  | Nis'212
  | Nis'211
  | Nis'210
  | Nis'209
  | Nis'208
  | Nis'207
  | Nis'206
  | Nis'205
  | Nis'204
  | Nis'203
  | Nis'202
  | Nis'201
  | Nis'200
  | Nis'199
  | Nis'198
  | Nis'197
  | Nis'196
  | Nis'195
  | Nis'194
  | Nis'193
  | Nis'192
  | Nis'191
  | Nis'190
  | Nis'189
  | Nis'188
  | Nis'187
  | Nis'186
  | Nis'185
  | Nis'184
  | Nis'183
  | Nis'182
  | Nis'181
  | Nis'180
  | Nis'179
  | Nis'178
  | Nis'177
  | Nis'176
  | Nis'175
  | Nis'174
  | Nis'173
  | Nis'172
  | Nis'171
  | Nis'170
  | Nis'169
  | Nis'168
  | Nis'167
  | Nis'166
  | Nis'165
  | Nis'164
  | Nis'163
  | Nis'162
  | Nis'161
  | Nis'160
  | Nis'159
  | Nis'158
  | Nis'157
  | Nis'156
  | Nis'155
  | Nis'154
  | Nis'153
  | Nis'152
  | Nis'151
  | Nis'150
  | Nis'149
  | Nis'148
  | Nis'147
  | Nis'146
  | Nis'145
  | Nis'144
  | Nis'143
  | Nis'142
  | Nis'141
  | Nis'140
  | Nis'139
  | Nis'138
  | Nis'137
  | Nis'136
  | Nis'135
  | Nis'134
  | Nis'133
  | Nis'132
  | Nis'131
  | Nis'130
  | Nis'129
  | Nis'128
  | Nis'127
  | Nis'126
  | Nis'125
  | Nis'124
  | Nis'123
  | Nis'122
  | Nis'121
  | Nis'120
  | Nis'119
  | Nis'118
  | Nis'117
  | Nis'116
  | Nis'115
  | Nis'114
  | Nis'113
  | Nis'112
  | Nis'111
  | Nis'110
  | Nis'109
  | Nis'108
  | Nis'107
  | Nis'106
  | Nis'105
  | Nis'104
  | Nis'103
  | Nis'102
  | Nis'101
  | Nis'100
  | Nis'99
  | Nis'98
  | Nis'97
  | Nis'96
  | Nis'95
  | Nis'94
  | Nis'93
  | Nis'92
  | Nis'91
  | Nis'90
  | Nis'89
  | Nis'88
  | Nis'87
  | Nis'86
  | Nis'85
  | Nis'84
  | Nis'83
  | Nis'82
  | Nis'81
  | Nis'80
  | Nis'79
  | Nis'78
  | Nis'77
  | Nis'76
  | Nis'75
  | Nis'74
  | Nis'73
  | Nis'72
  | Nis'71
  | Nis'70
  | Nis'69
  | Nis'68
  | Nis'67
  | Nis'66
  | Nis'65
  | Nis'64
  | Nis'63
  | Nis'62
  | Nis'61
  | Nis'60
  | Nis'59
  | Nis'58
  | Nis'57
  | Nis'56
  | Nis'55
  | Nis'54
  | Nis'53
  | Nis'52
  | Nis'51
  | Nis'50
  | Nis'49
  | Nis'48
  | Nis'47
  | Nis'46
  | Nis'45
  | Nis'44
  | Nis'43
  | Nis'42
  | Nis'41
  | Nis'40
  | Nis'39
  | Nis'38
  | Nis'37
  | Nis'36
  | Nis'35
  | Nis'34
  | Nis'33
  | Nis'32
  | Nis'31
  | Nis'30
  | Nis'29
  | Nis'28
  | Nis'27
  | Nis'26
  | Nis'25
  | Nis'24
  | Nis'23
  | Nis'22
  | Nis'21
  | Nis'20
  | Nis'19
  | Nis'18
  | Nis'17
  | Nis'16
  | Nis'15
  | Nis'14
  | Nis'13
  | Nis'12
  | Nis'11
  | Nis'10
  | Nis'9
  | Nis'8
  | Nis'7
  | Nis'6
  | Nis'5
  | Nis'4
  | Nis'3
  | Nis'2
  | Nis'1

  type noninitstate = noninitstate'

  val noninitstateNum : noninitstate coq_Numbered

  val coq_NonInitStateAlph : noninitstate coq_Alphabet

  val last_symb_of_non_init_state : noninitstate -> Coq__2.symbol

  type initstate' =
  | Init'0

  type initstate = initstate'

  val initstateNum : initstate coq_Numbered

  val coq_InitStateAlph : initstate coq_Alphabet

  type state =
  | Init of initstate
  | Ninit of noninitstate

  val state_rect : (initstate -> 'a1) -> (noninitstate -> 'a1) -> state -> 'a1

  val state_rec : (initstate -> 'a1) -> (noninitstate -> 'a1) -> state -> 'a1

  val coq_StateAlph : state coq_Alphabet

  type lookahead_action =
  | Shift_act of noninitstate
  | Reduce_act of Gram.production
  | Fail_act

  val lookahead_action_rect :
    Gram.terminal -> (noninitstate -> __ -> 'a1) -> (Gram.production -> 'a1)
    -> 'a1 -> lookahead_action -> 'a1

  val lookahead_action_rec :
    Gram.terminal -> (noninitstate -> __ -> 'a1) -> (Gram.production -> 'a1)
    -> 'a1 -> lookahead_action -> 'a1

  type action =
  | Default_reduce_act of Gram.production
  | Lookahead_act of (Gram.terminal -> lookahead_action)

  val action_rect :
    (Gram.production -> 'a1) -> ((Gram.terminal -> lookahead_action) -> 'a1)
    -> action -> 'a1

  val action_rec :
    (Gram.production -> 'a1) -> ((Gram.terminal -> lookahead_action) -> 'a1)
    -> action -> 'a1

  type item = { prod_item : Gram.production; dot_pos_item : nat;
                lookaheads_item : Gram.terminal list }

  val prod_item : item -> Gram.production

  val dot_pos_item : item -> nat

  val lookaheads_item : item -> Gram.terminal list

  val start_nt : initstate -> Coq__2.nonterminal

  val action_table : state -> action

  val goto_table : state -> Coq__2.nonterminal -> noninitstate option

  val past_symb_of_non_init_state : noninitstate -> Coq__2.symbol list

  val past_state_of_non_init_state : noninitstate -> (state -> bool) list

  val items_of_state : state -> item list

  val coq_N_of_state : state -> coq_N
 end

module MenhirLibParser :
 sig
  module Inter :
   sig
    module ValidSafe :
     sig
      val singleton_state_pred : Aut.state -> Aut.state -> bool

      val past_state_of_state : Aut.state -> (Aut.state -> bool) list

      val head_symbs_of_state : Aut.state -> Aut.Gram.symbol list

      val head_states_of_state : Aut.state -> (Aut.state -> bool) list

      val is_prefix : Aut.Gram.symbol list -> Aut.Gram.symbol list -> bool

      val is_prefix_pred :
        (Aut.state -> bool) list -> (Aut.state -> bool) list -> bool

      val is_state_valid_after_pop :
        Aut.state -> Aut.Gram.symbol list -> (Aut.state -> bool) list -> bool

      val is_safe : unit -> bool
     end

    type coq_Decidable = bool

    val decide : coq_Decidable -> bool

    val comparable_decidable_eq :
      'a1 coq_Comparable -> 'a1 -> 'a1 -> coq_Decidable

    val list_decidable_eq :
      ('a1 -> 'a1 -> coq_Decidable) -> 'a1 list -> 'a1 list -> coq_Decidable

    val cast : 'a1 -> 'a1 -> (unit -> coq_Decidable) -> 'a2 -> 'a2

    type buffer = __buffer Lazy.t
    and __buffer =
    | Buf_cons of Aut.Gram.token * buffer

    val buf_head : buffer -> Aut.Gram.token

    val buf_tail : buffer -> buffer

    val app_buf : Aut.Gram.token list -> buffer -> buffer

    type noninitstate_type = Aut.Gram.symbol_semantic_type

    type stack = (Aut.noninitstate, noninitstate_type) sigT list

    val state_of_stack : Aut.initstate -> stack -> Aut.state

    val state_stack_of_stack :
      Aut.initstate -> stack -> (Aut.state -> bool) list

    val symb_stack_of_stack : stack -> Aut.Gram.symbol list

    val pop : Aut.Gram.symbol list -> stack -> 'a1 arrows_right -> stack * 'a1

    type step_result =
    | Fail_sr_full of Aut.state * Aut.Gram.token
    | Accept_sr of Aut.Gram.symbol_semantic_type * buffer
    | Progress_sr of stack * buffer

    val step_result_rect :
      Aut.initstate -> (Aut.state -> Aut.Gram.token -> 'a1) ->
      (Aut.Gram.symbol_semantic_type -> buffer -> 'a1) -> (stack -> buffer ->
      'a1) -> step_result -> 'a1

    val step_result_rec :
      Aut.initstate -> (Aut.state -> Aut.Gram.token -> 'a1) ->
      (Aut.Gram.symbol_semantic_type -> buffer -> 'a1) -> (stack -> buffer ->
      'a1) -> step_result -> 'a1

    val reduce_step :
      Aut.initstate -> stack -> Aut.Gram.production -> buffer -> step_result

    val step : Aut.initstate -> stack -> buffer -> step_result

    val parse_fix : Aut.initstate -> stack -> buffer -> nat -> step_result

    type 'a parse_result =
    | Fail_pr_full of Aut.state * Aut.Gram.token
    | Timeout_pr
    | Parsed_pr of 'a * buffer

    val parse_result_rect :
      (Aut.state -> Aut.Gram.token -> 'a2) -> 'a2 -> ('a1 -> buffer -> 'a2)
      -> 'a1 parse_result -> 'a2

    val parse_result_rec :
      (Aut.state -> Aut.Gram.token -> 'a2) -> 'a2 -> ('a1 -> buffer -> 'a2)
      -> 'a1 parse_result -> 'a2

    val parse :
      Aut.initstate -> buffer -> nat -> Aut.Gram.symbol_semantic_type
      parse_result
   end

  module Correct :
   sig
   end

  module Complete :
   sig
    module Valid :
     sig
      module TerminalComparableM :
       sig
        type t = Aut.Gram.terminal

        val tComparable : t coq_Comparable
       end

      module TerminalOrderedType :
       sig
        module Alt :
         sig
          type t = TerminalComparableM.t

          val compare : t -> t -> comparison
         end

        type t = Alt.t

        val compare : Alt.t -> Alt.t -> Alt.t OrderedType.coq_Compare

        val eq_dec : Alt.t -> Alt.t -> bool
       end

      module StateProdPosComparableM :
       sig
        type t = (Aut.state * Aut.Gram.production) * nat

        val tComparable : t coq_Comparable
       end

      module StateProdPosOrderedType :
       sig
        module Alt :
         sig
          type t = StateProdPosComparableM.t

          val compare : t -> t -> comparison
         end

        type t = Alt.t

        val compare : Alt.t -> Alt.t -> Alt.t OrderedType.coq_Compare

        val eq_dec : Alt.t -> Alt.t -> bool
       end

      module TerminalSet :
       sig
        module X' :
         sig
          type t = TerminalOrderedType.Alt.t

          val eq_dec :
            TerminalOrderedType.Alt.t -> TerminalOrderedType.Alt.t -> bool

          val compare :
            TerminalOrderedType.Alt.t -> TerminalOrderedType.Alt.t ->
            comparison
         end

        module MSet :
         sig
          module Raw :
           sig
            type elt = TerminalOrderedType.Alt.t

            type tree =
            | Leaf
            | Node of Z_as_Int.t * tree * TerminalOrderedType.Alt.t * tree

            val empty : tree

            val is_empty : tree -> bool

            val mem : TerminalOrderedType.Alt.t -> tree -> bool

            val min_elt : tree -> elt option

            val max_elt : tree -> elt option

            val choose : tree -> elt option

            val fold : (elt -> 'a1 -> 'a1) -> tree -> 'a1 -> 'a1

            val elements_aux :
              TerminalOrderedType.Alt.t list -> tree ->
              TerminalOrderedType.Alt.t list

            val elements : tree -> TerminalOrderedType.Alt.t list

            val rev_elements_aux :
              TerminalOrderedType.Alt.t list -> tree ->
              TerminalOrderedType.Alt.t list

            val rev_elements : tree -> TerminalOrderedType.Alt.t list

            val cardinal : tree -> nat

            val maxdepth : tree -> nat

            val mindepth : tree -> nat

            val for_all : (elt -> bool) -> tree -> bool

            val exists_ : (elt -> bool) -> tree -> bool

            type enumeration =
            | End
            | More of elt * tree * enumeration

            val cons : tree -> enumeration -> enumeration

            val compare_more :
              TerminalOrderedType.Alt.t -> (enumeration -> comparison) ->
              enumeration -> comparison

            val compare_cont :
              tree -> (enumeration -> comparison) -> enumeration -> comparison

            val compare_end : enumeration -> comparison

            val compare : tree -> tree -> comparison

            val equal : tree -> tree -> bool

            val subsetl :
              (tree -> bool) -> TerminalOrderedType.Alt.t -> tree -> bool

            val subsetr :
              (tree -> bool) -> TerminalOrderedType.Alt.t -> tree -> bool

            val subset : tree -> tree -> bool

            type t = tree

            val height : t -> Z_as_Int.t

            val singleton : TerminalOrderedType.Alt.t -> tree

            val create : t -> TerminalOrderedType.Alt.t -> t -> tree

            val assert_false : t -> TerminalOrderedType.Alt.t -> t -> tree

            val bal : t -> TerminalOrderedType.Alt.t -> t -> tree

            val add : TerminalOrderedType.Alt.t -> tree -> tree

            val join : tree -> elt -> t -> t

            val remove_min : tree -> elt -> t -> t * elt

            val merge : tree -> tree -> tree

            val remove : TerminalOrderedType.Alt.t -> tree -> tree

            val concat : tree -> tree -> tree

            type triple = { t_left : t; t_in : bool; t_right : t }

            val t_left : triple -> t

            val t_in : triple -> bool

            val t_right : triple -> t

            val split : TerminalOrderedType.Alt.t -> tree -> triple

            val inter : tree -> tree -> tree

            val diff : tree -> tree -> tree

            val union : tree -> tree -> tree

            val filter : (elt -> bool) -> tree -> tree

            val partition : (elt -> bool) -> t -> t * t

            val ltb_tree : TerminalOrderedType.Alt.t -> tree -> bool

            val gtb_tree : TerminalOrderedType.Alt.t -> tree -> bool

            val isok : tree -> bool

            module MX :
             sig
              module OrderTac :
               sig
                module OTF :
                 sig
                  type t = TerminalOrderedType.Alt.t

                  val compare :
                    TerminalOrderedType.Alt.t -> TerminalOrderedType.Alt.t ->
                    comparison

                  val eq_dec :
                    TerminalOrderedType.Alt.t -> TerminalOrderedType.Alt.t ->
                    bool
                 end

                module TO :
                 sig
                  type t = TerminalOrderedType.Alt.t

                  val compare :
                    TerminalOrderedType.Alt.t -> TerminalOrderedType.Alt.t ->
                    comparison

                  val eq_dec :
                    TerminalOrderedType.Alt.t -> TerminalOrderedType.Alt.t ->
                    bool
                 end
               end

              val eq_dec :
                TerminalOrderedType.Alt.t -> TerminalOrderedType.Alt.t -> bool

              val lt_dec :
                TerminalOrderedType.Alt.t -> TerminalOrderedType.Alt.t -> bool

              val eqb :
                TerminalOrderedType.Alt.t -> TerminalOrderedType.Alt.t -> bool
             end

            type coq_R_min_elt =
            | R_min_elt_0 of tree
            | R_min_elt_1 of tree * Z_as_Int.t * tree
               * TerminalOrderedType.Alt.t * tree
            | R_min_elt_2 of tree * Z_as_Int.t * tree
               * TerminalOrderedType.Alt.t * tree * Z_as_Int.t * tree
               * TerminalOrderedType.Alt.t * tree * elt option * coq_R_min_elt

            type coq_R_max_elt =
            | R_max_elt_0 of tree
            | R_max_elt_1 of tree * Z_as_Int.t * tree
               * TerminalOrderedType.Alt.t * tree
            | R_max_elt_2 of tree * Z_as_Int.t * tree
               * TerminalOrderedType.Alt.t * tree * Z_as_Int.t * tree
               * TerminalOrderedType.Alt.t * tree * elt option * coq_R_max_elt

            module L :
             sig
              module MO :
               sig
                module OrderTac :
                 sig
                  module OTF :
                   sig
                    type t = TerminalOrderedType.Alt.t

                    val compare :
                      TerminalOrderedType.Alt.t -> TerminalOrderedType.Alt.t
                      -> comparison

                    val eq_dec :
                      TerminalOrderedType.Alt.t -> TerminalOrderedType.Alt.t
                      -> bool
                   end

                  module TO :
                   sig
                    type t = TerminalOrderedType.Alt.t

                    val compare :
                      TerminalOrderedType.Alt.t -> TerminalOrderedType.Alt.t
                      -> comparison

                    val eq_dec :
                      TerminalOrderedType.Alt.t -> TerminalOrderedType.Alt.t
                      -> bool
                   end
                 end

                val eq_dec :
                  TerminalOrderedType.Alt.t -> TerminalOrderedType.Alt.t ->
                  bool

                val lt_dec :
                  TerminalOrderedType.Alt.t -> TerminalOrderedType.Alt.t ->
                  bool

                val eqb :
                  TerminalOrderedType.Alt.t -> TerminalOrderedType.Alt.t ->
                  bool
               end
             end

            val flatten_e : enumeration -> elt list

            type coq_R_bal =
            | R_bal_0 of t * TerminalOrderedType.Alt.t * t
            | R_bal_1 of t * TerminalOrderedType.Alt.t * t * Z_as_Int.t
               * tree * TerminalOrderedType.Alt.t * tree
            | R_bal_2 of t * TerminalOrderedType.Alt.t * t * Z_as_Int.t
               * tree * TerminalOrderedType.Alt.t * tree
            | R_bal_3 of t * TerminalOrderedType.Alt.t * t * Z_as_Int.t
               * tree * TerminalOrderedType.Alt.t * tree * Z_as_Int.t * 
               tree * TerminalOrderedType.Alt.t * tree
            | R_bal_4 of t * TerminalOrderedType.Alt.t * t
            | R_bal_5 of t * TerminalOrderedType.Alt.t * t * Z_as_Int.t
               * tree * TerminalOrderedType.Alt.t * tree
            | R_bal_6 of t * TerminalOrderedType.Alt.t * t * Z_as_Int.t
               * tree * TerminalOrderedType.Alt.t * tree
            | R_bal_7 of t * TerminalOrderedType.Alt.t * t * Z_as_Int.t
               * tree * TerminalOrderedType.Alt.t * tree * Z_as_Int.t * 
               tree * TerminalOrderedType.Alt.t * tree
            | R_bal_8 of t * TerminalOrderedType.Alt.t * t

            type coq_R_remove_min =
            | R_remove_min_0 of tree * elt * t
            | R_remove_min_1 of tree * elt * t * Z_as_Int.t * tree
               * TerminalOrderedType.Alt.t * tree * (t * elt)
               * coq_R_remove_min * t * elt

            type coq_R_merge =
            | R_merge_0 of tree * tree
            | R_merge_1 of tree * tree * Z_as_Int.t * tree
               * TerminalOrderedType.Alt.t * tree
            | R_merge_2 of tree * tree * Z_as_Int.t * tree
               * TerminalOrderedType.Alt.t * tree * Z_as_Int.t * tree
               * TerminalOrderedType.Alt.t * tree * t * elt

            type coq_R_concat =
            | R_concat_0 of tree * tree
            | R_concat_1 of tree * tree * Z_as_Int.t * tree
               * TerminalOrderedType.Alt.t * tree
            | R_concat_2 of tree * tree * Z_as_Int.t * tree
               * TerminalOrderedType.Alt.t * tree * Z_as_Int.t * tree
               * TerminalOrderedType.Alt.t * tree * t * elt

            type coq_R_inter =
            | R_inter_0 of tree * tree
            | R_inter_1 of tree * tree * Z_as_Int.t * tree
               * TerminalOrderedType.Alt.t * tree
            | R_inter_2 of tree * tree * Z_as_Int.t * tree
               * TerminalOrderedType.Alt.t * tree * Z_as_Int.t * tree
               * TerminalOrderedType.Alt.t * tree * t * bool * t * tree
               * coq_R_inter * tree * coq_R_inter
            | R_inter_3 of tree * tree * Z_as_Int.t * tree
               * TerminalOrderedType.Alt.t * tree * Z_as_Int.t * tree
               * TerminalOrderedType.Alt.t * tree * t * bool * t * tree
               * coq_R_inter * tree * coq_R_inter

            type coq_R_diff =
            | R_diff_0 of tree * tree
            | R_diff_1 of tree * tree * Z_as_Int.t * tree
               * TerminalOrderedType.Alt.t * tree
            | R_diff_2 of tree * tree * Z_as_Int.t * tree
               * TerminalOrderedType.Alt.t * tree * Z_as_Int.t * tree
               * TerminalOrderedType.Alt.t * tree * t * bool * t * tree
               * coq_R_diff * tree * coq_R_diff
            | R_diff_3 of tree * tree * Z_as_Int.t * tree
               * TerminalOrderedType.Alt.t * tree * Z_as_Int.t * tree
               * TerminalOrderedType.Alt.t * tree * t * bool * t * tree
               * coq_R_diff * tree * coq_R_diff

            type coq_R_union =
            | R_union_0 of tree * tree
            | R_union_1 of tree * tree * Z_as_Int.t * tree
               * TerminalOrderedType.Alt.t * tree
            | R_union_2 of tree * tree * Z_as_Int.t * tree
               * TerminalOrderedType.Alt.t * tree * Z_as_Int.t * tree
               * TerminalOrderedType.Alt.t * tree * t * bool * t * tree
               * coq_R_union * tree * coq_R_union
           end

          module E :
           sig
            type t = TerminalOrderedType.Alt.t

            val compare :
              TerminalOrderedType.Alt.t -> TerminalOrderedType.Alt.t ->
              comparison

            val eq_dec :
              TerminalOrderedType.Alt.t -> TerminalOrderedType.Alt.t -> bool
           end

          type elt = TerminalOrderedType.Alt.t

          type t_ = Raw.t
            (* singleton inductive, whose constructor was Mkt *)

          val this : t_ -> Raw.t

          type t = t_

          val mem : elt -> t -> bool

          val add : elt -> t -> t

          val remove : elt -> t -> t

          val singleton : elt -> t

          val union : t -> t -> t

          val inter : t -> t -> t

          val diff : t -> t -> t

          val equal : t -> t -> bool

          val subset : t -> t -> bool

          val empty : t

          val is_empty : t -> bool

          val elements : t -> elt list

          val choose : t -> elt option

          val fold : (elt -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1

          val cardinal : t -> nat

          val filter : (elt -> bool) -> t -> t

          val for_all : (elt -> bool) -> t -> bool

          val exists_ : (elt -> bool) -> t -> bool

          val partition : (elt -> bool) -> t -> t * t

          val eq_dec : t -> t -> bool

          val compare : t -> t -> comparison

          val min_elt : t -> elt option

          val max_elt : t -> elt option
         end

        type elt = TerminalOrderedType.Alt.t

        type t = MSet.t

        val empty : t

        val is_empty : t -> bool

        val mem : elt -> t -> bool

        val add : elt -> t -> t

        val singleton : elt -> t

        val remove : elt -> t -> t

        val union : t -> t -> t

        val inter : t -> t -> t

        val diff : t -> t -> t

        val eq_dec : t -> t -> bool

        val equal : t -> t -> bool

        val subset : t -> t -> bool

        val fold : (elt -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1

        val for_all : (elt -> bool) -> t -> bool

        val exists_ : (elt -> bool) -> t -> bool

        val filter : (elt -> bool) -> t -> t

        val partition : (elt -> bool) -> t -> t * t

        val cardinal : t -> nat

        val elements : t -> elt list

        val choose : t -> elt option

        module MF :
         sig
          val eqb :
            TerminalOrderedType.Alt.t -> TerminalOrderedType.Alt.t -> bool
         end

        val min_elt : t -> elt option

        val max_elt : t -> elt option

        val compare : t -> t -> t OrderedType.coq_Compare

        module E :
         sig
          type t = TerminalOrderedType.Alt.t

          val compare :
            TerminalOrderedType.Alt.t -> TerminalOrderedType.Alt.t ->
            TerminalOrderedType.Alt.t OrderedType.coq_Compare

          val eq_dec :
            TerminalOrderedType.Alt.t -> TerminalOrderedType.Alt.t -> bool
         end
       end

      module StateProdPosMap :
       sig
        module E :
         sig
          type t = StateProdPosOrderedType.Alt.t

          val compare :
            StateProdPosOrderedType.Alt.t -> StateProdPosOrderedType.Alt.t ->
            StateProdPosOrderedType.Alt.t OrderedType.coq_Compare

          val eq_dec :
            StateProdPosOrderedType.Alt.t -> StateProdPosOrderedType.Alt.t ->
            bool
         end

        module Raw :
         sig
          type key = StateProdPosOrderedType.Alt.t

          type 'elt tree =
          | Leaf
          | Node of 'elt tree * key * 'elt * 'elt tree * Z_as_Int.t

          val tree_rect :
            'a2 -> ('a1 tree -> 'a2 -> key -> 'a1 -> 'a1 tree -> 'a2 ->
            Z_as_Int.t -> 'a2) -> 'a1 tree -> 'a2

          val tree_rec :
            'a2 -> ('a1 tree -> 'a2 -> key -> 'a1 -> 'a1 tree -> 'a2 ->
            Z_as_Int.t -> 'a2) -> 'a1 tree -> 'a2

          val height : 'a1 tree -> Z_as_Int.t

          val cardinal : 'a1 tree -> nat

          val empty : 'a1 tree

          val is_empty : 'a1 tree -> bool

          val mem : StateProdPosOrderedType.Alt.t -> 'a1 tree -> bool

          val find : StateProdPosOrderedType.Alt.t -> 'a1 tree -> 'a1 option

          val create : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree

          val assert_false : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree

          val bal : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree

          val add : key -> 'a1 -> 'a1 tree -> 'a1 tree

          val remove_min :
            'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree * (key * 'a1)

          val merge : 'a1 tree -> 'a1 tree -> 'a1 tree

          val remove : StateProdPosOrderedType.Alt.t -> 'a1 tree -> 'a1 tree

          val join : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree

          type 'elt triple = { t_left : 'elt tree; t_opt : 'elt option;
                               t_right : 'elt tree }

          val t_left : 'a1 triple -> 'a1 tree

          val t_opt : 'a1 triple -> 'a1 option

          val t_right : 'a1 triple -> 'a1 tree

          val split : StateProdPosOrderedType.Alt.t -> 'a1 tree -> 'a1 triple

          val concat : 'a1 tree -> 'a1 tree -> 'a1 tree

          val elements_aux : (key * 'a1) list -> 'a1 tree -> (key * 'a1) list

          val elements : 'a1 tree -> (key * 'a1) list

          val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 tree -> 'a2 -> 'a2

          type 'elt enumeration =
          | End
          | More of key * 'elt * 'elt tree * 'elt enumeration

          val enumeration_rect :
            'a2 -> (key -> 'a1 -> 'a1 tree -> 'a1 enumeration -> 'a2 -> 'a2)
            -> 'a1 enumeration -> 'a2

          val enumeration_rec :
            'a2 -> (key -> 'a1 -> 'a1 tree -> 'a1 enumeration -> 'a2 -> 'a2)
            -> 'a1 enumeration -> 'a2

          val cons : 'a1 tree -> 'a1 enumeration -> 'a1 enumeration

          val equal_more :
            ('a1 -> 'a1 -> bool) -> StateProdPosOrderedType.Alt.t -> 'a1 ->
            ('a1 enumeration -> bool) -> 'a1 enumeration -> bool

          val equal_cont :
            ('a1 -> 'a1 -> bool) -> 'a1 tree -> ('a1 enumeration -> bool) ->
            'a1 enumeration -> bool

          val equal_end : 'a1 enumeration -> bool

          val equal : ('a1 -> 'a1 -> bool) -> 'a1 tree -> 'a1 tree -> bool

          val map : ('a1 -> 'a2) -> 'a1 tree -> 'a2 tree

          val mapi : (key -> 'a1 -> 'a2) -> 'a1 tree -> 'a2 tree

          val map_option : (key -> 'a1 -> 'a2 option) -> 'a1 tree -> 'a2 tree

          val map2_opt :
            (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3
            tree) -> ('a2 tree -> 'a3 tree) -> 'a1 tree -> 'a2 tree -> 'a3
            tree

          val map2 :
            ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 tree -> 'a2 tree
            -> 'a3 tree

          module Proofs :
           sig
            module MX :
             sig
              module TO :
               sig
                type t = StateProdPosOrderedType.Alt.t
               end

              module IsTO :
               sig
               end

              module OrderTac :
               sig
               end

              val eq_dec :
                StateProdPosOrderedType.Alt.t ->
                StateProdPosOrderedType.Alt.t -> bool

              val lt_dec :
                StateProdPosOrderedType.Alt.t ->
                StateProdPosOrderedType.Alt.t -> bool

              val eqb :
                StateProdPosOrderedType.Alt.t ->
                StateProdPosOrderedType.Alt.t -> bool
             end

            module PX :
             sig
              module MO :
               sig
                module TO :
                 sig
                  type t = StateProdPosOrderedType.Alt.t
                 end

                module IsTO :
                 sig
                 end

                module OrderTac :
                 sig
                 end

                val eq_dec :
                  StateProdPosOrderedType.Alt.t ->
                  StateProdPosOrderedType.Alt.t -> bool

                val lt_dec :
                  StateProdPosOrderedType.Alt.t ->
                  StateProdPosOrderedType.Alt.t -> bool

                val eqb :
                  StateProdPosOrderedType.Alt.t ->
                  StateProdPosOrderedType.Alt.t -> bool
               end
             end

            module L :
             sig
              module MX :
               sig
                module TO :
                 sig
                  type t = StateProdPosOrderedType.Alt.t
                 end

                module IsTO :
                 sig
                 end

                module OrderTac :
                 sig
                 end

                val eq_dec :
                  StateProdPosOrderedType.Alt.t ->
                  StateProdPosOrderedType.Alt.t -> bool

                val lt_dec :
                  StateProdPosOrderedType.Alt.t ->
                  StateProdPosOrderedType.Alt.t -> bool

                val eqb :
                  StateProdPosOrderedType.Alt.t ->
                  StateProdPosOrderedType.Alt.t -> bool
               end

              module PX :
               sig
                module MO :
                 sig
                  module TO :
                   sig
                    type t = StateProdPosOrderedType.Alt.t
                   end

                  module IsTO :
                   sig
                   end

                  module OrderTac :
                   sig
                   end

                  val eq_dec :
                    StateProdPosOrderedType.Alt.t ->
                    StateProdPosOrderedType.Alt.t -> bool

                  val lt_dec :
                    StateProdPosOrderedType.Alt.t ->
                    StateProdPosOrderedType.Alt.t -> bool

                  val eqb :
                    StateProdPosOrderedType.Alt.t ->
                    StateProdPosOrderedType.Alt.t -> bool
                 end
               end

              type key = StateProdPosOrderedType.Alt.t

              type 'elt t = (StateProdPosOrderedType.Alt.t * 'elt) list

              val empty : 'a1 t

              val is_empty : 'a1 t -> bool

              val mem : key -> 'a1 t -> bool

              type 'elt coq_R_mem =
              | R_mem_0 of 'elt t
              | R_mem_1 of 'elt t * StateProdPosOrderedType.Alt.t * 'elt
                 * (StateProdPosOrderedType.Alt.t * 'elt) list
              | R_mem_2 of 'elt t * StateProdPosOrderedType.Alt.t * 'elt
                 * (StateProdPosOrderedType.Alt.t * 'elt) list
              | R_mem_3 of 'elt t * StateProdPosOrderedType.Alt.t * 'elt
                 * (StateProdPosOrderedType.Alt.t * 'elt) list * bool
                 * 'elt coq_R_mem

              val coq_R_mem_rect :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t ->
                StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1 t -> bool ->
                'a1 coq_R_mem -> 'a2

              val coq_R_mem_rec :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t ->
                StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1 t -> bool ->
                'a1 coq_R_mem -> 'a2

              val mem_rect :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t ->
                StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2 -> 'a2) -> 'a1 t -> 'a2

              val mem_rec :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t ->
                StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2 -> 'a2) -> 'a1 t -> 'a2

              val coq_R_mem_correct : key -> 'a1 t -> bool -> 'a1 coq_R_mem

              val find : key -> 'a1 t -> 'a1 option

              type 'elt coq_R_find =
              | R_find_0 of 'elt t
              | R_find_1 of 'elt t * StateProdPosOrderedType.Alt.t * 
                 'elt * (StateProdPosOrderedType.Alt.t * 'elt) list
              | R_find_2 of 'elt t * StateProdPosOrderedType.Alt.t * 
                 'elt * (StateProdPosOrderedType.Alt.t * 'elt) list
              | R_find_3 of 'elt t * StateProdPosOrderedType.Alt.t * 
                 'elt * (StateProdPosOrderedType.Alt.t * 'elt) list
                 * 'elt option * 'elt coq_R_find

              val coq_R_find_rect :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t ->
                StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> 'a1 t ->
                'a1 option -> 'a1 coq_R_find -> 'a2

              val coq_R_find_rec :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t ->
                StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> 'a1 t ->
                'a1 option -> 'a1 coq_R_find -> 'a2

              val find_rect :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t ->
                StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2 -> 'a2) -> 'a1 t -> 'a2

              val find_rec :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t ->
                StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2 -> 'a2) -> 'a1 t -> 'a2

              val coq_R_find_correct :
                key -> 'a1 t -> 'a1 option -> 'a1 coq_R_find

              val add : key -> 'a1 -> 'a1 t -> 'a1 t

              type 'elt coq_R_add =
              | R_add_0 of 'elt t
              | R_add_1 of 'elt t * StateProdPosOrderedType.Alt.t * 'elt
                 * (StateProdPosOrderedType.Alt.t * 'elt) list
              | R_add_2 of 'elt t * StateProdPosOrderedType.Alt.t * 'elt
                 * (StateProdPosOrderedType.Alt.t * 'elt) list
              | R_add_3 of 'elt t * StateProdPosOrderedType.Alt.t * 'elt
                 * (StateProdPosOrderedType.Alt.t * 'elt) list * 'elt t
                 * 'elt coq_R_add

              val coq_R_add_rect :
                key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t ->
                StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a1 t -> 'a1 coq_R_add -> 'a2 -> 'a2) -> 'a1 t -> 'a1 t ->
                'a1 coq_R_add -> 'a2

              val coq_R_add_rec :
                key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t ->
                StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a1 t -> 'a1 coq_R_add -> 'a2 -> 'a2) -> 'a1 t -> 'a1 t ->
                'a1 coq_R_add -> 'a2

              val add_rect :
                key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t ->
                StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2 -> 'a2) -> 'a1 t -> 'a2

              val add_rec :
                key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t ->
                StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2 -> 'a2) -> 'a1 t -> 'a2

              val coq_R_add_correct :
                key -> 'a1 -> 'a1 t -> 'a1 t -> 'a1 coq_R_add

              val remove : key -> 'a1 t -> 'a1 t

              type 'elt coq_R_remove =
              | R_remove_0 of 'elt t
              | R_remove_1 of 'elt t * StateProdPosOrderedType.Alt.t * 
                 'elt * (StateProdPosOrderedType.Alt.t * 'elt) list
              | R_remove_2 of 'elt t * StateProdPosOrderedType.Alt.t * 
                 'elt * (StateProdPosOrderedType.Alt.t * 'elt) list
              | R_remove_3 of 'elt t * StateProdPosOrderedType.Alt.t * 
                 'elt * (StateProdPosOrderedType.Alt.t * 'elt) list * 
                 'elt t * 'elt coq_R_remove

              val coq_R_remove_rect :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t ->
                StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a1 t -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> 'a1 t -> 'a1 t
                -> 'a1 coq_R_remove -> 'a2

              val coq_R_remove_rec :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t ->
                StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a1 t -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> 'a1 t -> 'a1 t
                -> 'a1 coq_R_remove -> 'a2

              val remove_rect :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t ->
                StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2 -> 'a2) -> 'a1 t -> 'a2

              val remove_rec :
                key -> ('a1 t -> __ -> 'a2) -> ('a1 t ->
                StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2) -> ('a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2 -> 'a2) -> 'a1 t -> 'a2

              val coq_R_remove_correct :
                key -> 'a1 t -> 'a1 t -> 'a1 coq_R_remove

              val elements : 'a1 t -> 'a1 t

              val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2

              type ('elt, 'a) coq_R_fold =
              | R_fold_0 of 'elt t * 'a
              | R_fold_1 of 'elt t * 'a * StateProdPosOrderedType.Alt.t
                 * 'elt * (StateProdPosOrderedType.Alt.t * 'elt) list * 
                 'a * ('elt, 'a) coq_R_fold

              val coq_R_fold_rect :
                (key -> 'a1 -> 'a2 -> 'a2) -> ('a1 t -> 'a2 -> __ -> 'a3) ->
                ('a1 t -> 'a2 -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> 'a2 ->
                ('a1, 'a2) coq_R_fold -> 'a3 -> 'a3) -> 'a1 t -> 'a2 -> 'a2
                -> ('a1, 'a2) coq_R_fold -> 'a3

              val coq_R_fold_rec :
                (key -> 'a1 -> 'a2 -> 'a2) -> ('a1 t -> 'a2 -> __ -> 'a3) ->
                ('a1 t -> 'a2 -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> 'a2 ->
                ('a1, 'a2) coq_R_fold -> 'a3 -> 'a3) -> 'a1 t -> 'a2 -> 'a2
                -> ('a1, 'a2) coq_R_fold -> 'a3

              val fold_rect :
                (key -> 'a1 -> 'a2 -> 'a2) -> ('a1 t -> 'a2 -> __ -> 'a3) ->
                ('a1 t -> 'a2 -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> 'a3 ->
                'a3) -> 'a1 t -> 'a2 -> 'a3

              val fold_rec :
                (key -> 'a1 -> 'a2 -> 'a2) -> ('a1 t -> 'a2 -> __ -> 'a3) ->
                ('a1 t -> 'a2 -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> 'a3 ->
                'a3) -> 'a1 t -> 'a2 -> 'a3

              val coq_R_fold_correct :
                (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 -> ('a1,
                'a2) coq_R_fold

              val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool

              type 'elt coq_R_equal =
              | R_equal_0 of 'elt t * 'elt t
              | R_equal_1 of 'elt t * 'elt t * StateProdPosOrderedType.Alt.t
                 * 'elt * (StateProdPosOrderedType.Alt.t * 'elt) list
                 * StateProdPosOrderedType.Alt.t * 'elt
                 * (StateProdPosOrderedType.Alt.t * 'elt) list * bool
                 * 'elt coq_R_equal
              | R_equal_2 of 'elt t * 'elt t * StateProdPosOrderedType.Alt.t
                 * 'elt * (StateProdPosOrderedType.Alt.t * 'elt) list
                 * StateProdPosOrderedType.Alt.t * 'elt
                 * (StateProdPosOrderedType.Alt.t * 'elt) list
                 * StateProdPosOrderedType.Alt.t OrderedType.coq_Compare
              | R_equal_3 of 'elt t * 'elt t * 'elt t * 'elt t

              val coq_R_equal_rect :
                ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2)
                -> ('a1 t -> 'a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ ->
                StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> bool -> 'a1 coq_R_equal -> 'a2 -> 'a2) -> ('a1 t -> 'a1 t
                -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ ->
                StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ ->
                StateProdPosOrderedType.Alt.t OrderedType.coq_Compare -> __
                -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t ->
                __ -> __ -> 'a2) -> 'a1 t -> 'a1 t -> bool -> 'a1 coq_R_equal
                -> 'a2

              val coq_R_equal_rec :
                ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2)
                -> ('a1 t -> 'a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ ->
                StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> bool -> 'a1 coq_R_equal -> 'a2 -> 'a2) -> ('a1 t -> 'a1 t
                -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ ->
                StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ ->
                StateProdPosOrderedType.Alt.t OrderedType.coq_Compare -> __
                -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t ->
                __ -> __ -> 'a2) -> 'a1 t -> 'a1 t -> bool -> 'a1 coq_R_equal
                -> 'a2

              val equal_rect :
                ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2)
                -> ('a1 t -> 'a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ ->
                StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2 -> 'a2) -> ('a1 t -> 'a1 t ->
                StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ ->
                StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ ->
                StateProdPosOrderedType.Alt.t OrderedType.coq_Compare -> __
                -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t ->
                __ -> __ -> 'a2) -> 'a1 t -> 'a1 t -> 'a2

              val equal_rec :
                ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2)
                -> ('a1 t -> 'a1 t -> StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ ->
                StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ -> __ -> __
                -> 'a2 -> 'a2) -> ('a1 t -> 'a1 t ->
                StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ ->
                StateProdPosOrderedType.Alt.t -> 'a1 ->
                (StateProdPosOrderedType.Alt.t * 'a1) list -> __ ->
                StateProdPosOrderedType.Alt.t OrderedType.coq_Compare -> __
                -> __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t ->
                __ -> __ -> 'a2) -> 'a1 t -> 'a1 t -> 'a2

              val coq_R_equal_correct :
                ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool -> 'a1
                coq_R_equal

              val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t

              val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t

              val option_cons :
                key -> 'a1 option -> (key * 'a1) list -> (key * 'a1) list

              val map2_l :
                ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a3 t

              val map2_r :
                ('a1 option -> 'a2 option -> 'a3 option) -> 'a2 t -> 'a3 t

              val map2 :
                ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t ->
                'a3 t

              val combine : 'a1 t -> 'a2 t -> ('a1 option * 'a2 option) t

              val fold_right_pair :
                ('a1 -> 'a2 -> 'a3 -> 'a3) -> ('a1 * 'a2) list -> 'a3 -> 'a3

              val map2_alt :
                ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t ->
                (key * 'a3) list

              val at_least_one :
                'a1 option -> 'a2 option -> ('a1 option * 'a2 option) option

              val at_least_one_then_f :
                ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 option -> 'a2
                option -> 'a3 option
             end

            type 'elt coq_R_mem =
            | R_mem_0 of 'elt tree
            | R_mem_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Z_as_Int.t * bool * 'elt coq_R_mem
            | R_mem_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Z_as_Int.t
            | R_mem_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Z_as_Int.t * bool * 'elt coq_R_mem

            val coq_R_mem_rect :
              StateProdPosOrderedType.Alt.t -> ('a1 tree -> __ -> 'a2) ->
              ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
              -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) ->
              ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
              -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
              'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> bool -> 'a1
              coq_R_mem -> 'a2 -> 'a2) -> 'a1 tree -> bool -> 'a1 coq_R_mem
              -> 'a2

            val coq_R_mem_rec :
              StateProdPosOrderedType.Alt.t -> ('a1 tree -> __ -> 'a2) ->
              ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
              -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) ->
              ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
              -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key ->
              'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> bool -> 'a1
              coq_R_mem -> 'a2 -> 'a2) -> 'a1 tree -> bool -> 'a1 coq_R_mem
              -> 'a2

            type 'elt coq_R_find =
            | R_find_0 of 'elt tree
            | R_find_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Z_as_Int.t * 'elt option * 'elt coq_R_find
            | R_find_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Z_as_Int.t
            | R_find_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Z_as_Int.t * 'elt option * 'elt coq_R_find

            val coq_R_find_rect :
              StateProdPosOrderedType.Alt.t -> ('a1 tree -> __ -> 'a2) ->
              ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
              -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find -> 'a2 ->
              'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree
              -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ ->
              'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> 'a1 tree -> 'a1
              option -> 'a1 coq_R_find -> 'a2

            val coq_R_find_rec :
              StateProdPosOrderedType.Alt.t -> ('a1 tree -> __ -> 'a2) ->
              ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
              -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find -> 'a2 ->
              'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree
              -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ ->
              'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> 'a1 tree -> 'a1
              option -> 'a1 coq_R_find -> 'a2

            type 'elt coq_R_bal =
            | R_bal_0 of 'elt tree * key * 'elt * 'elt tree
            | R_bal_1 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
               key * 'elt * 'elt tree * Z_as_Int.t
            | R_bal_2 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
               key * 'elt * 'elt tree * Z_as_Int.t
            | R_bal_3 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
               key * 'elt * 'elt tree * Z_as_Int.t * 'elt tree * key * 
               'elt * 'elt tree * Z_as_Int.t
            | R_bal_4 of 'elt tree * key * 'elt * 'elt tree
            | R_bal_5 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
               key * 'elt * 'elt tree * Z_as_Int.t
            | R_bal_6 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
               key * 'elt * 'elt tree * Z_as_Int.t
            | R_bal_7 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
               key * 'elt * 'elt tree * Z_as_Int.t * 'elt tree * key * 
               'elt * 'elt tree * Z_as_Int.t
            | R_bal_8 of 'elt tree * key * 'elt * 'elt tree

            val coq_R_bal_rect :
              ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> 'a2)
              -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree
              -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ ->
              'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1
              tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __
              -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ ->
              __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ ->
              __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t ->
              __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __
              -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1
              tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
              tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
              key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree ->
              key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> __ ->
              'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __
              -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __
              -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
              -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ ->
              __ -> __ -> __ -> 'a2) -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              'a1 tree -> 'a1 coq_R_bal -> 'a2

            val coq_R_bal_rec :
              ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> 'a2)
              -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree
              -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ ->
              'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1
              tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __
              -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ ->
              __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ ->
              __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t ->
              __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __
              -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1
              tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
              tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
              key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree ->
              key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> __ ->
              'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __
              -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __
              -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
              -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ ->
              __ -> __ -> __ -> 'a2) -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              'a1 tree -> 'a1 coq_R_bal -> 'a2

            type 'elt coq_R_add =
            | R_add_0 of 'elt tree
            | R_add_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Z_as_Int.t * 'elt tree * 'elt coq_R_add
            | R_add_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Z_as_Int.t
            | R_add_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Z_as_Int.t * 'elt tree * 'elt coq_R_add

            val coq_R_add_rect :
              key -> 'a1 -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree
              -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ ->
              'a1 tree -> 'a1 coq_R_add -> 'a2 -> 'a2) -> ('a1 tree -> 'a1
              tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __
              -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_add ->
              'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_add -> 'a2

            val coq_R_add_rec :
              key -> 'a1 -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree
              -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ ->
              'a1 tree -> 'a1 coq_R_add -> 'a2 -> 'a2) -> ('a1 tree -> 'a1
              tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __
              -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_add ->
              'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_add -> 'a2

            type 'elt coq_R_remove_min =
            | R_remove_min_0 of 'elt tree * key * 'elt * 'elt tree
            | R_remove_min_1 of 'elt tree * key * 'elt * 'elt tree
               * 'elt tree * key * 'elt * 'elt tree * Z_as_Int.t
               * ('elt tree * (key * 'elt)) * 'elt coq_R_remove_min
               * 'elt tree * (key * 'elt)

            val coq_R_remove_min_rect :
              ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> 'a2) -> ('a1 tree
              -> key -> 'a1 -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
              -> Z_as_Int.t -> __ -> ('a1 tree * (key * 'a1)) -> 'a1
              coq_R_remove_min -> 'a2 -> 'a1 tree -> (key * 'a1) -> __ ->
              'a2) -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> ('a1
              tree * (key * 'a1)) -> 'a1 coq_R_remove_min -> 'a2

            val coq_R_remove_min_rec :
              ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> 'a2) -> ('a1 tree
              -> key -> 'a1 -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
              -> Z_as_Int.t -> __ -> ('a1 tree * (key * 'a1)) -> 'a1
              coq_R_remove_min -> 'a2 -> 'a1 tree -> (key * 'a1) -> __ ->
              'a2) -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> ('a1
              tree * (key * 'a1)) -> 'a1 coq_R_remove_min -> 'a2

            type 'elt coq_R_merge =
            | R_merge_0 of 'elt tree * 'elt tree
            | R_merge_1 of 'elt tree * 'elt tree * 'elt tree * key * 
               'elt * 'elt tree * Z_as_Int.t
            | R_merge_2 of 'elt tree * 'elt tree * 'elt tree * key * 
               'elt * 'elt tree * Z_as_Int.t * 'elt tree * key * 'elt
               * 'elt tree * Z_as_Int.t * 'elt tree * (key * 'elt) * 
               key * 'elt

            val coq_R_merge_rect :
              ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
              'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ ->
              'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
              tree -> Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree
              -> Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ -> key ->
              'a1 -> __ -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1
              coq_R_merge -> 'a2

            val coq_R_merge_rec :
              ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
              'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ ->
              'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
              tree -> Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree
              -> Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ -> key ->
              'a1 -> __ -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1
              coq_R_merge -> 'a2

            type 'elt coq_R_remove =
            | R_remove_0 of 'elt tree
            | R_remove_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Z_as_Int.t * 'elt tree * 'elt coq_R_remove
            | R_remove_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Z_as_Int.t
            | R_remove_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Z_as_Int.t * 'elt tree * 'elt coq_R_remove

            val coq_R_remove_rect :
              StateProdPosOrderedType.Alt.t -> ('a1 tree -> __ -> 'a2) ->
              ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
              -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove -> 'a2 ->
              'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree
              -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ ->
              'a1 tree -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> 'a1 tree -> 'a1
              tree -> 'a1 coq_R_remove -> 'a2

            val coq_R_remove_rec :
              StateProdPosOrderedType.Alt.t -> ('a1 tree -> __ -> 'a2) ->
              ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
              -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove -> 'a2 ->
              'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
              Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree
              -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ ->
              'a1 tree -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> 'a1 tree -> 'a1
              tree -> 'a1 coq_R_remove -> 'a2

            type 'elt coq_R_concat =
            | R_concat_0 of 'elt tree * 'elt tree
            | R_concat_1 of 'elt tree * 'elt tree * 'elt tree * key * 
               'elt * 'elt tree * Z_as_Int.t
            | R_concat_2 of 'elt tree * 'elt tree * 'elt tree * key * 
               'elt * 'elt tree * Z_as_Int.t * 'elt tree * key * 'elt
               * 'elt tree * Z_as_Int.t * 'elt tree * (key * 'elt)

            val coq_R_concat_rect :
              ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
              'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ ->
              'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
              tree -> Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree
              -> Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ -> 'a2) ->
              'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_concat -> 'a2

            val coq_R_concat_rec :
              ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
              'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ ->
              'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
              tree -> Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree
              -> Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ -> 'a2) ->
              'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_concat -> 'a2

            type 'elt coq_R_split =
            | R_split_0 of 'elt tree
            | R_split_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Z_as_Int.t * 'elt triple * 'elt coq_R_split * 'elt tree
               * 'elt option * 'elt tree
            | R_split_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Z_as_Int.t
            | R_split_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
               * Z_as_Int.t * 'elt triple * 'elt coq_R_split * 'elt tree
               * 'elt option * 'elt tree

            val coq_R_split_rect :
              StateProdPosOrderedType.Alt.t -> ('a1 tree -> __ -> 'a2) ->
              ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
              -> __ -> __ -> __ -> 'a1 triple -> 'a1 coq_R_split -> 'a2 ->
              'a1 tree -> 'a1 option -> 'a1 tree -> __ -> 'a2) -> ('a1 tree
              -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __
              -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
              tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 triple -> 'a1
              coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __
              -> 'a2) -> 'a1 tree -> 'a1 triple -> 'a1 coq_R_split -> 'a2

            val coq_R_split_rec :
              StateProdPosOrderedType.Alt.t -> ('a1 tree -> __ -> 'a2) ->
              ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
              -> __ -> __ -> __ -> 'a1 triple -> 'a1 coq_R_split -> 'a2 ->
              'a1 tree -> 'a1 option -> 'a1 tree -> __ -> 'a2) -> ('a1 tree
              -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __
              -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
              tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 triple -> 'a1
              coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __
              -> 'a2) -> 'a1 tree -> 'a1 triple -> 'a1 coq_R_split -> 'a2

            type ('elt, 'x) coq_R_map_option =
            | R_map_option_0 of 'elt tree
            | R_map_option_1 of 'elt tree * 'elt tree * key * 'elt
               * 'elt tree * Z_as_Int.t * 'x * 'x tree
               * ('elt, 'x) coq_R_map_option * 'x tree
               * ('elt, 'x) coq_R_map_option
            | R_map_option_2 of 'elt tree * 'elt tree * key * 'elt
               * 'elt tree * Z_as_Int.t * 'x tree
               * ('elt, 'x) coq_R_map_option * 'x tree
               * ('elt, 'x) coq_R_map_option

            val coq_R_map_option_rect :
              (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1
              tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __
              -> 'a2 -> __ -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3
              -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a3) ->
              ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
              -> __ -> __ -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3
              -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a3) ->
              'a1 tree -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3

            val coq_R_map_option_rec :
              (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1
              tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __
              -> 'a2 -> __ -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3
              -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a3) ->
              ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
              -> __ -> __ -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3
              -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a3) ->
              'a1 tree -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3

            type ('elt, 'x0, 'x) coq_R_map2_opt =
            | R_map2_opt_0 of 'elt tree * 'x0 tree
            | R_map2_opt_1 of 'elt tree * 'x0 tree * 'elt tree * key * 
               'elt * 'elt tree * Z_as_Int.t
            | R_map2_opt_2 of 'elt tree * 'x0 tree * 'elt tree * key * 
               'elt * 'elt tree * Z_as_Int.t * 'x0 tree * key * 'x0
               * 'x0 tree * Z_as_Int.t * 'x0 tree * 'x0 option * 'x0 tree
               * 'x * 'x tree * ('elt, 'x0, 'x) coq_R_map2_opt * 'x tree
               * ('elt, 'x0, 'x) coq_R_map2_opt
            | R_map2_opt_3 of 'elt tree * 'x0 tree * 'elt tree * key * 
               'elt * 'elt tree * Z_as_Int.t * 'x0 tree * key * 'x0
               * 'x0 tree * Z_as_Int.t * 'x0 tree * 'x0 option * 'x0 tree
               * 'x tree * ('elt, 'x0, 'x) coq_R_map2_opt * 'x tree
               * ('elt, 'x0, 'x) coq_R_map2_opt

            val coq_R_map2_opt_rect :
              (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3
              tree) -> ('a2 tree -> 'a3 tree) -> ('a1 tree -> 'a2 tree -> __
              -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 ->
              'a1 tree -> Z_as_Int.t -> __ -> __ -> 'a4) -> ('a1 tree -> 'a2
              tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __
              -> 'a2 tree -> key -> 'a2 -> 'a2 tree -> Z_as_Int.t -> __ ->
              'a2 tree -> 'a2 option -> 'a2 tree -> __ -> 'a3 -> __ -> 'a3
              tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a3 tree ->
              ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a4) -> ('a1 tree ->
              'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t ->
              __ -> 'a2 tree -> key -> 'a2 -> 'a2 tree -> Z_as_Int.t -> __ ->
              'a2 tree -> 'a2 option -> 'a2 tree -> __ -> __ -> 'a3 tree ->
              ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1, 'a2,
              'a3) coq_R_map2_opt -> 'a4 -> 'a4) -> 'a1 tree -> 'a2 tree ->
              'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4

            val coq_R_map2_opt_rec :
              (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3
              tree) -> ('a2 tree -> 'a3 tree) -> ('a1 tree -> 'a2 tree -> __
              -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 ->
              'a1 tree -> Z_as_Int.t -> __ -> __ -> 'a4) -> ('a1 tree -> 'a2
              tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __
              -> 'a2 tree -> key -> 'a2 -> 'a2 tree -> Z_as_Int.t -> __ ->
              'a2 tree -> 'a2 option -> 'a2 tree -> __ -> 'a3 -> __ -> 'a3
              tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a3 tree ->
              ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a4) -> ('a1 tree ->
              'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t ->
              __ -> 'a2 tree -> key -> 'a2 -> 'a2 tree -> Z_as_Int.t -> __ ->
              'a2 tree -> 'a2 option -> 'a2 tree -> __ -> __ -> 'a3 tree ->
              ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1, 'a2,
              'a3) coq_R_map2_opt -> 'a4 -> 'a4) -> 'a1 tree -> 'a2 tree ->
              'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4

            val fold' : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 tree -> 'a2 -> 'a2

            val flatten_e : 'a1 enumeration -> (key * 'a1) list
           end
         end

        type 'elt bst =
          'elt Raw.tree
          (* singleton inductive, whose constructor was Bst *)

        val this : 'a1 bst -> 'a1 Raw.tree

        type 'elt t = 'elt bst

        type key = StateProdPosOrderedType.Alt.t

        val empty : 'a1 t

        val is_empty : 'a1 t -> bool

        val add : key -> 'a1 -> 'a1 t -> 'a1 t

        val remove : key -> 'a1 t -> 'a1 t

        val mem : key -> 'a1 t -> bool

        val find : key -> 'a1 t -> 'a1 option

        val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t

        val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t

        val map2 :
          ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t

        val elements : 'a1 t -> (key * 'a1) list

        val cardinal : 'a1 t -> nat

        val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2

        val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool
       end

      val nullable_symb : Aut.Gram.symbol -> bool

      val nullable_word : Aut.Gram.symbol list -> bool

      val first_nterm_set : Aut.Gram.nonterminal -> TerminalSet.t

      val first_symb_set : Aut.Gram.symbol -> TerminalSet.t

      val first_word_set : Aut.Gram.symbol list -> TerminalSet.t

      val future_of_prod : Aut.Gram.production -> nat -> Aut.Gram.symbol list

      val items_map : unit -> TerminalSet.t StateProdPosMap.t

      val find_items_map :
        TerminalSet.t StateProdPosMap.t -> Aut.state -> Aut.Gram.production
        -> nat -> TerminalSet.t

      val forallb_items :
        TerminalSet.t StateProdPosMap.t -> (Aut.state -> Aut.Gram.production
        -> nat -> TerminalSet.t -> bool) -> bool

      val is_end_reduce : TerminalSet.t StateProdPosMap.t -> bool

      val is_complete_0 : TerminalSet.t StateProdPosMap.t -> bool

      val is_complete : unit -> bool
     end

    type pt_zipper =
    | Top_ptz
    | Cons_ptl_ptz of Aut.Gram.symbol list * Aut.Gram.token list
       * Aut.GramDefs.parse_tree_list * Aut.Gram.symbol * Aut.Gram.token list
       * ptl_zipper
    and ptl_zipper =
    | Non_terminal_pt_ptlz of Aut.Gram.production * Aut.Gram.token list
       * pt_zipper
    | Cons_ptl_ptlz of Aut.Gram.symbol list * Aut.Gram.token list
       * Aut.Gram.symbol * Aut.Gram.token list * Aut.GramDefs.parse_tree
       * ptl_zipper

    val pt_zipper_rect :
      Aut.initstate -> Aut.Gram.token list -> 'a1 -> (Aut.Gram.symbol list ->
      Aut.Gram.token list -> Aut.GramDefs.parse_tree_list -> Aut.Gram.symbol
      -> Aut.Gram.token list -> ptl_zipper -> 'a1) -> Aut.Gram.symbol ->
      Aut.Gram.token list -> pt_zipper -> 'a1

    val pt_zipper_rec :
      Aut.initstate -> Aut.Gram.token list -> 'a1 -> (Aut.Gram.symbol list ->
      Aut.Gram.token list -> Aut.GramDefs.parse_tree_list -> Aut.Gram.symbol
      -> Aut.Gram.token list -> ptl_zipper -> 'a1) -> Aut.Gram.symbol ->
      Aut.Gram.token list -> pt_zipper -> 'a1

    val ptl_zipper_rect :
      Aut.initstate -> Aut.Gram.token list -> (Aut.Gram.production ->
      Aut.Gram.token list -> pt_zipper -> 'a1) -> (Aut.Gram.symbol list ->
      Aut.Gram.token list -> Aut.Gram.symbol -> Aut.Gram.token list ->
      Aut.GramDefs.parse_tree -> ptl_zipper -> 'a1 -> 'a1) -> Aut.Gram.symbol
      list -> Aut.Gram.token list -> ptl_zipper -> 'a1

    val ptl_zipper_rec :
      Aut.initstate -> Aut.Gram.token list -> (Aut.Gram.production ->
      Aut.Gram.token list -> pt_zipper -> 'a1) -> (Aut.Gram.symbol list ->
      Aut.Gram.token list -> Aut.Gram.symbol -> Aut.Gram.token list ->
      Aut.GramDefs.parse_tree -> ptl_zipper -> 'a1 -> 'a1) -> Aut.Gram.symbol
      list -> Aut.Gram.token list -> ptl_zipper -> 'a1

    type pt_dot =
    | Reduce_ptd of Aut.Gram.production * Aut.Gram.token list
       * Aut.GramDefs.parse_tree_list * pt_zipper
    | Shift_ptd of Aut.Gram.token * Aut.Gram.symbol list
       * Aut.Gram.token list * Aut.GramDefs.parse_tree_list * ptl_zipper

    val pt_dot_rect :
      Aut.initstate -> Aut.Gram.token list -> (Aut.Gram.production ->
      Aut.Gram.token list -> Aut.GramDefs.parse_tree_list -> pt_zipper ->
      'a1) -> (Aut.Gram.token -> Aut.Gram.symbol list -> Aut.Gram.token list
      -> Aut.GramDefs.parse_tree_list -> ptl_zipper -> 'a1) -> pt_dot -> 'a1

    val pt_dot_rec :
      Aut.initstate -> Aut.Gram.token list -> (Aut.Gram.production ->
      Aut.Gram.token list -> Aut.GramDefs.parse_tree_list -> pt_zipper ->
      'a1) -> (Aut.Gram.token -> Aut.Gram.symbol list -> Aut.Gram.token list
      -> Aut.GramDefs.parse_tree_list -> ptl_zipper -> 'a1) -> pt_dot -> 'a1

    val ptlz_sem :
      Aut.initstate -> Aut.Gram.token list -> Aut.Gram.symbol list ->
      Aut.Gram.token list -> ptl_zipper -> (__ -> __ arrows_right -> __) ->
      Aut.Gram.symbol_semantic_type

    val ptz_sem :
      Aut.initstate -> Aut.Gram.token list -> Aut.Gram.symbol ->
      Aut.Gram.token list -> pt_zipper -> Aut.Gram.symbol_semantic_type ->
      Aut.Gram.symbol_semantic_type

    val ptd_sem :
      Aut.initstate -> Aut.Gram.token list -> pt_dot ->
      Aut.Gram.symbol_semantic_type

    val ptlz_buffer :
      Aut.initstate -> Aut.Gram.token list -> Inter.buffer -> Aut.Gram.symbol
      list -> Aut.Gram.token list -> ptl_zipper -> Inter.buffer

    val ptz_buffer :
      Aut.initstate -> Aut.Gram.token list -> Inter.buffer -> Aut.Gram.symbol
      -> Aut.Gram.token list -> pt_zipper -> Inter.buffer

    val ptd_buffer :
      Aut.initstate -> Aut.Gram.token list -> Inter.buffer -> pt_dot ->
      Inter.buffer

    val ptlz_prod :
      Aut.initstate -> Aut.Gram.token list -> Aut.Gram.symbol list ->
      Aut.Gram.token list -> ptl_zipper -> Aut.Gram.production

    val ptlz_future :
      Aut.initstate -> Aut.Gram.token list -> Aut.Gram.symbol list ->
      Aut.Gram.token list -> ptl_zipper -> Aut.Gram.symbol list

    val ptlz_lookahead :
      Aut.initstate -> Aut.Gram.token list -> Inter.buffer -> Aut.Gram.symbol
      list -> Aut.Gram.token list -> ptl_zipper -> Aut.Gram.terminal

    val build_pt_dot_from_pt :
      Aut.initstate -> Aut.Gram.token list -> Aut.Gram.symbol ->
      Aut.Gram.token list -> Aut.GramDefs.parse_tree -> pt_zipper -> pt_dot

    val build_pt_dot_from_pt_rec :
      Aut.initstate -> Aut.Gram.token list -> Aut.Gram.symbol list ->
      Aut.Gram.token list -> Aut.GramDefs.parse_tree_list -> ptl_zipper ->
      pt_dot

    val build_pt_dot_from_ptl :
      Aut.initstate -> Aut.Gram.token list -> Aut.Gram.symbol list ->
      Aut.Gram.token list -> Aut.GramDefs.parse_tree_list -> ptl_zipper ->
      pt_dot

    val next_ptd :
      Aut.initstate -> Aut.Gram.token list -> pt_dot -> pt_dot option

    val next_ptd_iter :
      Aut.initstate -> Aut.Gram.token list -> pt_dot -> nat -> pt_dot option

    val ptlz_cost :
      Aut.initstate -> Aut.Gram.token list -> Aut.Gram.symbol list ->
      Aut.Gram.token list -> ptl_zipper -> nat

    val ptz_cost :
      Aut.initstate -> Aut.Gram.token list -> Aut.Gram.symbol ->
      Aut.Gram.token list -> pt_zipper -> nat

    val ptd_cost : Aut.initstate -> Aut.Gram.token list -> pt_dot -> nat
   end

  val complete_validator : unit -> bool

  val safe_validator : unit -> bool

  val parse :
    Aut.initstate -> nat -> Inter.buffer -> Aut.Gram.symbol_semantic_type
    Inter.parse_result
 end

val translation_unit_file :
  nat -> MenhirLibParser.Inter.buffer -> definition list
  MenhirLibParser.Inter.parse_result
