open AST
open Builtins0
open Builtins1

type builtin_function =
| BI_standard of standard_builtin
| BI_platform of platform_builtin

(** val builtin_function_sig : builtin_function -> signature **)

let builtin_function_sig = function
| BI_standard b0 -> standard_builtin_sig b0
| BI_platform b0 -> platform_builtin_sig b0

(** val builtin_function_sem : builtin_function -> builtin_sem **)

let builtin_function_sem = function
| BI_standard b0 -> standard_builtin_sem b0
| BI_platform b0 -> platform_builtin_sem b0

(** val lookup_builtin_function :
    char list -> signature -> builtin_function option **)

let lookup_builtin_function name sg =
  match lookup_builtin standard_builtin_sig name sg standard_builtin_table with
  | Some b -> Some (BI_standard b)
  | None ->
    (match lookup_builtin platform_builtin_sig name sg platform_builtin_table with
     | Some b -> Some (BI_platform b)
     | None -> None)
