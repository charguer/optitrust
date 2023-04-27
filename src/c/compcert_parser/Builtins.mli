open AST
open Builtins0
open Builtins1

type builtin_function =
| BI_standard of standard_builtin
| BI_platform of platform_builtin

val builtin_function_sig : builtin_function -> signature

val builtin_function_sem : builtin_function -> builtin_sem

val lookup_builtin_function :
  char list -> signature -> builtin_function option
