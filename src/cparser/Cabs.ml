
type char_code = int64

type loc =
  { lineno : int;
   filename: string;
   byteno: int;
   ident : int;
 }

type floatInfo = { isHex_FI : bool; integer_FI : String.t option;
                   fraction_FI : String.t option;
                   exponent_FI : String.t option; suffix_FI : String.t option }

type structOrUnion =
| STRUCT
| UNION

type typeSpecifier =
| Tvoid
| Tchar
| Tshort
| Tint
| Tlong
| Tfloat
| Tdouble
| Tsigned
| Tunsigned
| T_Bool
| Tnamed of String.t
| Tstruct_union of structOrUnion * String.t option * field_group list option
   * attribute list
| Tenum of String.t option
   * ((String.t * expression option) * loc) list option * attribute list
and storage =
| AUTO
| STATIC
| EXTERN
| REGISTER
| TYPEDEF
and cvspec =
| CV_CONST
| CV_VOLATILE
| CV_RESTRICT
| CV_ATTR of attribute
and funspec =
| INLINE
| NORETURN
and spec_elem =
| SpecCV of cvspec
| SpecStorage of storage
| SpecFunction of funspec
| SpecType of typeSpecifier
and decl_type =
| JUSTBASE
| ARRAY of decl_type * cvspec list * expression option
| PTR of cvspec list * decl_type
| PROTO of decl_type * (parameter list * bool)
| PROTO_OLD of decl_type * String.t list
and parameter =
| PARAM of spec_elem list * String.t option * decl_type * attribute list * loc
and field_group =
| Field_group of spec_elem list * (name option * expression option) list * loc
| Field_group_static_assert of expression * loc * constant * loc * loc
and name =
| Name of String.t * decl_type * attribute list * loc
and init_name =
| Init_name of name * init_expression
and binary_operator =
| ADD
| SUB
| MUL
| DIV
| MOD
| AND
| OR
| BAND
| BOR
| XOR
| SHL
| SHR
| EQ
| NE
| LT
| GT
| LE
| GE
| ASSIGN
| ADD_ASSIGN
| SUB_ASSIGN
| MUL_ASSIGN
| DIV_ASSIGN
| MOD_ASSIGN
| BAND_ASSIGN
| BOR_ASSIGN
| XOR_ASSIGN
| SHL_ASSIGN
| SHR_ASSIGN
| COMMA
and unary_operator =
| MINUS
| PLUS
| NOT
| BNOT
| MEMOF
| ADDROF
| PREINCR
| PREDECR
| POSINCR
| POSDECR
and expression =
| UNARY of unary_operator * expression
| BINARY of binary_operator * expression * expression
| QUESTION of expression * expression * expression
| CAST of (spec_elem list * decl_type) * init_expression
| CALL of expression * expression list
| BUILTIN_VA_ARG of expression * (spec_elem list * decl_type)
| CONSTANT of constant
| VARIABLE of String.t
| EXPR_SIZEOF of expression
| TYPE_SIZEOF of (spec_elem list * decl_type)
| INDEX of expression * expression
| MEMBEROF of expression * String.t
| MEMBEROFPTR of expression * String.t
| ALIGNOF of (spec_elem list * decl_type)
| BUILTIN_OFFSETOF of (spec_elem list * decl_type) * initwhat list
and constant =
| CONST_INT of String.t
| CONST_FLOAT of floatInfo
| CONST_CHAR of bool * char_code list
| CONST_STRING of bool * char_code list
and init_expression =
| NO_INIT
| SINGLE_INIT of expression
| COMPOUND_INIT of (initwhat list * init_expression) list
and initwhat =
| INFIELD_INIT of String.t
| ATINDEX_INIT of expression
and attribute =
| GCC_ATTR of gcc_attribute list * loc
| PACKED_ATTR of expression list * loc
| ALIGNAS_ATTR of expression list * loc
and gcc_attribute =
| GCC_ATTR_EMPTY
| GCC_ATTR_NOARGS of gcc_attribute_word
| GCC_ATTR_ARGS of gcc_attribute_word * expression list
and gcc_attribute_word =
| GCC_ATTR_IDENT of String.t
| GCC_ATTR_CONST
| GCC_ATTR_PACKED

type init_name_group = spec_elem list * init_name list

type asm_operand =
| ASMOPERAND of String.t option * bool * char_code list * expression

type asm_flag = bool * char_code list

type definition =
| FUNDEF of spec_elem list * name * definition list * statement * loc
| DECDEF of init_name_group * loc
| PRAGMA of String.t * loc
| STATIC_ASSERT of expression * loc * constant * loc * loc
and statement =
| NOP of loc
| COMPUTATION of expression * loc
| BLOCK of statement list * loc
| If of expression * statement * statement option * loc
| WHILE of expression * statement * loc
| DOWHILE of expression * statement * loc
| FOR of for_clause option * expression option * expression option
   * statement * loc
| BREAK of loc
| CONTINUE of loc
| RETURN of expression option * loc
| SWITCH of expression * statement * loc
| CASE of expression * statement * loc
| DEFAULT of statement * loc
| LABEL of String.t * statement * loc
| GOTO of String.t * loc
| ASM of cvspec list * bool * char_code list * asm_operand list
   * asm_operand list * asm_flag list * loc
| DEFINITION of definition
and for_clause =
| FC_EXP of expression
| FC_DECL of definition
