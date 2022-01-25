open AST
open Builtins0
open Datatypes
open Floats

type platform_builtin =
| BI_fmin
| BI_fmax

val platform_builtin_table : (char list * platform_builtin) list

val platform_builtin_sig : platform_builtin -> signature

val platform_builtin_sem : platform_builtin -> builtin_sem
