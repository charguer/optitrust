open AST
open Builtins0
open Datatypes
open Floats

type platform_builtin =
| BI_fmin
| BI_fmax

(** val platform_builtin_table : (char list * platform_builtin) list **)

let platform_builtin_table =
  (('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('f'::('m'::('i'::('n'::[])))))))))))))),
    BI_fmin) :: ((('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('f'::('m'::('a'::('x'::[])))))))))))))),
    BI_fmax) :: [])

(** val platform_builtin_sig : platform_builtin -> signature **)

let platform_builtin_sig _ =
  { sig_args = (Tfloat :: (Tfloat :: [])); sig_res = (Tret Tfloat); sig_cc =
    cc_default }

(** val platform_builtin_sem : platform_builtin -> builtin_sem **)

let platform_builtin_sem = function
| BI_fmin ->
  mkbuiltin_n2t Tfloat Tfloat (Tret Tfloat) (fun f1 f2 ->
    match Float.compare (Obj.magic f1) (Obj.magic f2) with
    | Some c -> (match c with
                 | Gt -> f2
                 | _ -> f1)
    | None -> f2)
| BI_fmax ->
  mkbuiltin_n2t Tfloat Tfloat (Tret Tfloat) (fun f1 f2 ->
    match Float.compare (Obj.magic f1) (Obj.magic f2) with
    | Some c -> (match c with
                 | Lt -> f2
                 | _ -> f1)
    | None -> f2)
