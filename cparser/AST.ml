open Archi
open BinInt
open BinNums
open Bool
open Coqlib
open Datatypes
open Errors
open Floats
open Integers
open List0
open Maps
open String0

let __ = let rec f _ = Obj.repr f in Obj.repr f

type ident = positive

(** val ident_eq : positive -> positive -> bool **)

let ident_eq =
  peq

type typ =
| Tint
| Tfloat
| Tlong
| Tsingle
| Tany32
| Tany64

(** val typ_eq : typ -> typ -> bool **)

let typ_eq t1 t2 =
  match t1 with
  | Tint -> (match t2 with
             | Tint -> true
             | _ -> false)
  | Tfloat -> (match t2 with
               | Tfloat -> true
               | _ -> false)
  | Tlong -> (match t2 with
              | Tlong -> true
              | _ -> false)
  | Tsingle -> (match t2 with
                | Tsingle -> true
                | _ -> false)
  | Tany32 -> (match t2 with
               | Tany32 -> true
               | _ -> false)
  | Tany64 -> (match t2 with
               | Tany64 -> true
               | _ -> false)

(** val list_typ_eq : typ list -> typ list -> bool **)

let list_typ_eq =
  list_eq_dec typ_eq

(** val coq_Tptr : typ **)

let coq_Tptr =
  if ptr64 then Tlong else Tint

(** val typesize : typ -> coq_Z **)

let typesize = function
| Tint -> Zpos (Coq_xO (Coq_xO Coq_xH))
| Tsingle -> Zpos (Coq_xO (Coq_xO Coq_xH))
| Tany32 -> Zpos (Coq_xO (Coq_xO Coq_xH))
| _ -> Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))

(** val subtype : typ -> typ -> bool **)

let subtype ty1 ty2 =
  match ty1 with
  | Tint ->
    (match ty2 with
     | Tint -> true
     | Tany32 -> true
     | Tany64 -> true
     | _ -> false)
  | Tfloat -> (match ty2 with
               | Tfloat -> true
               | Tany64 -> true
               | _ -> false)
  | Tlong -> (match ty2 with
              | Tlong -> true
              | Tany64 -> true
              | _ -> false)
  | Tsingle ->
    (match ty2 with
     | Tint -> false
     | Tfloat -> false
     | Tlong -> false
     | _ -> true)
  | Tany32 -> (match ty2 with
               | Tany32 -> true
               | Tany64 -> true
               | _ -> false)
  | Tany64 -> (match ty2 with
               | Tany64 -> true
               | _ -> false)

type rettype =
| Tret of typ
| Tint8signed
| Tint8unsigned
| Tint16signed
| Tint16unsigned
| Tvoid

(** val rettype_eq : rettype -> rettype -> bool **)

let rettype_eq t1 t2 =
  match t1 with
  | Tret x -> (match t2 with
               | Tret t0 -> typ_eq x t0
               | _ -> false)
  | Tint8signed -> (match t2 with
                    | Tint8signed -> true
                    | _ -> false)
  | Tint8unsigned -> (match t2 with
                      | Tint8unsigned -> true
                      | _ -> false)
  | Tint16signed -> (match t2 with
                     | Tint16signed -> true
                     | _ -> false)
  | Tint16unsigned -> (match t2 with
                       | Tint16unsigned -> true
                       | _ -> false)
  | Tvoid -> (match t2 with
              | Tvoid -> true
              | _ -> false)

(** val proj_rettype : rettype -> typ **)

let proj_rettype = function
| Tret t0 -> t0
| _ -> Tint

type calling_convention = { cc_vararg : coq_Z option; cc_unproto : bool;
                            cc_structret : bool }

(** val cc_default : calling_convention **)

let cc_default =
  { cc_vararg = None; cc_unproto = false; cc_structret = false }

(** val calling_convention_eq :
    calling_convention -> calling_convention -> bool **)

let calling_convention_eq x y =
  let { cc_vararg = cc_vararg0; cc_unproto = cc_unproto0; cc_structret =
    cc_structret0 } = x
  in
  let { cc_vararg = cc_vararg1; cc_unproto = cc_unproto1; cc_structret =
    cc_structret1 } = y
  in
  if match cc_vararg0 with
     | Some x0 ->
       (match cc_vararg1 with
        | Some z -> Z.eq_dec x0 z
        | None -> false)
     | None -> (match cc_vararg1 with
                | Some _ -> false
                | None -> true)
  then if bool_dec cc_unproto0 cc_unproto1
       then bool_dec cc_structret0 cc_structret1
       else false
  else false

type signature = { sig_args : typ list; sig_res : rettype;
                   sig_cc : calling_convention }

(** val proj_sig_res : signature -> typ **)

let proj_sig_res s =
  proj_rettype s.sig_res

(** val signature_eq : signature -> signature -> bool **)

let signature_eq s1 s2 =
  let { sig_args = sig_args0; sig_res = sig_res0; sig_cc = sig_cc0 } = s1 in
  let { sig_args = sig_args1; sig_res = sig_res1; sig_cc = sig_cc1 } = s2 in
  if list_typ_eq sig_args0 sig_args1
  then if rettype_eq sig_res0 sig_res1
       then calling_convention_eq sig_cc0 sig_cc1
       else false
  else false

(** val signature_main : signature **)

let signature_main =
  { sig_args = []; sig_res = (Tret Tint); sig_cc = cc_default }

type memory_chunk =
| Mint8signed
| Mint8unsigned
| Mint16signed
| Mint16unsigned
| Mint32
| Mint64
| Mfloat32
| Mfloat64
| Many32
| Many64

(** val chunk_eq : memory_chunk -> memory_chunk -> bool **)

let chunk_eq c1 c2 =
  match c1 with
  | Mint8signed -> (match c2 with
                    | Mint8signed -> true
                    | _ -> false)
  | Mint8unsigned -> (match c2 with
                      | Mint8unsigned -> true
                      | _ -> false)
  | Mint16signed -> (match c2 with
                     | Mint16signed -> true
                     | _ -> false)
  | Mint16unsigned -> (match c2 with
                       | Mint16unsigned -> true
                       | _ -> false)
  | Mint32 -> (match c2 with
               | Mint32 -> true
               | _ -> false)
  | Mint64 -> (match c2 with
               | Mint64 -> true
               | _ -> false)
  | Mfloat32 -> (match c2 with
                 | Mfloat32 -> true
                 | _ -> false)
  | Mfloat64 -> (match c2 with
                 | Mfloat64 -> true
                 | _ -> false)
  | Many32 -> (match c2 with
               | Many32 -> true
               | _ -> false)
  | Many64 -> (match c2 with
               | Many64 -> true
               | _ -> false)

(** val coq_Mptr : memory_chunk **)

let coq_Mptr =
  if ptr64 then Mint64 else Mint32

(** val type_of_chunk : memory_chunk -> typ **)

let type_of_chunk = function
| Mint64 -> Tlong
| Mfloat32 -> Tsingle
| Mfloat64 -> Tfloat
| Many32 -> Tany32
| Many64 -> Tany64
| _ -> Tint

(** val rettype_of_chunk : memory_chunk -> rettype **)

let rettype_of_chunk = function
| Mint8signed -> Tint8signed
| Mint8unsigned -> Tint8unsigned
| Mint16signed -> Tint16signed
| Mint16unsigned -> Tint16unsigned
| Mint32 -> Tret Tint
| Mint64 -> Tret Tlong
| Mfloat32 -> Tret Tsingle
| Mfloat64 -> Tret Tfloat
| Many32 -> Tret Tany32
| Many64 -> Tret Tany64

(** val chunk_of_type : typ -> memory_chunk **)

let chunk_of_type = function
| Tint -> Mint32
| Tfloat -> Mfloat64
| Tlong -> Mint64
| Tsingle -> Mfloat32
| Tany32 -> Many32
| Tany64 -> Many64

type init_data =
| Init_int8 of Int.int
| Init_int16 of Int.int
| Init_int32 of Int.int
| Init_int64 of Int64.int
| Init_float32 of float32
| Init_float64 of float
| Init_space of coq_Z
| Init_addrof of ident * Ptrofs.int

(** val init_data_size : init_data -> coq_Z **)

let init_data_size = function
| Init_int8 _ -> Zpos Coq_xH
| Init_int16 _ -> Zpos (Coq_xO Coq_xH)
| Init_int32 _ -> Zpos (Coq_xO (Coq_xO Coq_xH))
| Init_float32 _ -> Zpos (Coq_xO (Coq_xO Coq_xH))
| Init_space n -> Z.max n Z0
| Init_addrof (_, _) ->
  if ptr64
  then Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
  else Zpos (Coq_xO (Coq_xO Coq_xH))
| _ -> Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))

(** val init_data_list_size : init_data list -> coq_Z **)

let rec init_data_list_size = function
| [] -> Z0
| i :: il' -> Z.add (init_data_size i) (init_data_list_size il')

type 'v globvar = { gvar_info : 'v; gvar_init : init_data list;
                    gvar_readonly : bool; gvar_volatile : bool }

type ('f, 'v) globdef =
| Gfun of 'f
| Gvar of 'v globvar

type ('f, 'v) program = { prog_defs : (ident * ('f, 'v) globdef) list;
                          prog_public : ident list; prog_main : ident }

(** val prog_defmap : ('a1, 'a2) program -> ('a1, 'a2) globdef PTree.t **)

let prog_defmap p =
  PTree_Properties.of_list p.prog_defs

(** val transform_program_globdef :
    ('a1 -> 'a2) -> (ident * ('a1, 'a3) globdef) -> ident * ('a2, 'a3) globdef **)

let transform_program_globdef transf = function
| (id, g) ->
  (match g with
   | Gfun f -> (id, (Gfun (transf f)))
   | Gvar v -> (id, (Gvar v)))

(** val transform_program :
    ('a1 -> 'a2) -> ('a1, 'a3) program -> ('a2, 'a3) program **)

let transform_program transf p =
  { prog_defs = (map (transform_program_globdef transf) p.prog_defs);
    prog_public = p.prog_public; prog_main = p.prog_main }

(** val transf_globvar :
    (ident -> 'a1 -> 'a2 res) -> ident -> 'a1 globvar -> 'a2 globvar res **)

let transf_globvar transf_var i g =
  match transf_var i g.gvar_info with
  | OK x ->
    OK { gvar_info = x; gvar_init = g.gvar_init; gvar_readonly =
      g.gvar_readonly; gvar_volatile = g.gvar_volatile }
  | Error msg -> Error msg

(** val transf_globdefs :
    (ident -> 'a1 -> 'a2 res) -> (ident -> 'a3 -> 'a4 res) -> (ident * ('a1,
    'a3) globdef) list -> (ident * ('a2, 'a4) globdef) list res **)

let rec transf_globdefs transf_fun transf_var = function
| [] -> OK []
| p :: l' ->
  let (id, g) = p in
  (match g with
   | Gfun f ->
     (match transf_fun id f with
      | OK tf ->
        (match transf_globdefs transf_fun transf_var l' with
         | OK x -> OK ((id, (Gfun tf)) :: x)
         | Error msg -> Error msg)
      | Error msg ->
        Error ((MSG
          ('I'::('n'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::[]))))))))))))) :: ((CTX
          id) :: ((MSG (':'::(' '::[]))) :: msg))))
   | Gvar v ->
     (match transf_globvar transf_var id v with
      | OK tv ->
        (match transf_globdefs transf_fun transf_var l' with
         | OK x -> OK ((id, (Gvar tv)) :: x)
         | Error msg -> Error msg)
      | Error msg ->
        Error ((MSG
          ('I'::('n'::(' '::('v'::('a'::('r'::('i'::('a'::('b'::('l'::('e'::(' '::[]))))))))))))) :: ((CTX
          id) :: ((MSG (':'::(' '::[]))) :: msg)))))

(** val transform_partial_program2 :
    (ident -> 'a1 -> 'a2 res) -> (ident -> 'a3 -> 'a4 res) -> ('a1, 'a3)
    program -> ('a2, 'a4) program res **)

let transform_partial_program2 transf_fun transf_var p =
  match transf_globdefs transf_fun transf_var p.prog_defs with
  | OK x ->
    OK { prog_defs = x; prog_public = p.prog_public; prog_main = p.prog_main }
  | Error msg -> Error msg

(** val transform_partial_program :
    ('a1 -> 'a2 res) -> ('a1, 'a3) program -> ('a2, 'a3) program res **)

let transform_partial_program transf_fun p =
  transform_partial_program2 (fun _ -> transf_fun) (fun _ v -> OK v) p

type external_function =
| EF_external of char list * signature
| EF_builtin of char list * signature
| EF_runtime of char list * signature
| EF_vload of memory_chunk
| EF_vstore of memory_chunk
| EF_malloc
| EF_free
| EF_memcpy of coq_Z * coq_Z
| EF_annot of positive * char list * typ list
| EF_annot_val of positive * char list * typ
| EF_inline_asm of char list * signature * char list list
| EF_debug of positive * ident * typ list

(** val ef_sig : external_function -> signature **)

let ef_sig = function
| EF_external (_, sg) -> sg
| EF_builtin (_, sg) -> sg
| EF_runtime (_, sg) -> sg
| EF_vload chunk ->
  { sig_args = (coq_Tptr :: []); sig_res = (rettype_of_chunk chunk); sig_cc =
    cc_default }
| EF_vstore chunk ->
  { sig_args = (coq_Tptr :: ((type_of_chunk chunk) :: [])); sig_res = Tvoid;
    sig_cc = cc_default }
| EF_malloc ->
  { sig_args = (coq_Tptr :: []); sig_res = (Tret coq_Tptr); sig_cc =
    cc_default }
| EF_free ->
  { sig_args = (coq_Tptr :: []); sig_res = Tvoid; sig_cc = cc_default }
| EF_memcpy (_, _) ->
  { sig_args = (coq_Tptr :: (coq_Tptr :: [])); sig_res = Tvoid; sig_cc =
    cc_default }
| EF_annot (_, _, targs) ->
  { sig_args = targs; sig_res = Tvoid; sig_cc = cc_default }
| EF_annot_val (_, _, targ) ->
  { sig_args = (targ :: []); sig_res = (Tret targ); sig_cc = cc_default }
| EF_inline_asm (_, sg, _) -> sg
| EF_debug (_, _, targs) ->
  { sig_args = targs; sig_res = Tvoid; sig_cc = cc_default }

(** val ef_inline : external_function -> bool **)

let ef_inline = function
| EF_external (_, _) -> false
| EF_runtime (_, _) -> false
| EF_malloc -> false
| EF_free -> false
| _ -> true

(** val external_function_eq :
    external_function -> external_function -> bool **)

let external_function_eq =
  let x = fun _ -> list_eq_dec in
  (fun ef1 ef2 ->
  match ef1 with
  | EF_external (x0, x1) ->
    (match ef2 with
     | EF_external (name0, sg0) ->
       if string_dec x0 name0 then signature_eq x1 sg0 else false
     | _ -> false)
  | EF_builtin (x0, x1) ->
    (match ef2 with
     | EF_builtin (name0, sg0) ->
       if string_dec x0 name0 then signature_eq x1 sg0 else false
     | _ -> false)
  | EF_runtime (x0, x1) ->
    (match ef2 with
     | EF_runtime (name0, sg0) ->
       if string_dec x0 name0 then signature_eq x1 sg0 else false
     | _ -> false)
  | EF_vload x0 ->
    (match ef2 with
     | EF_vload chunk0 -> chunk_eq x0 chunk0
     | _ -> false)
  | EF_vstore x0 ->
    (match ef2 with
     | EF_vstore chunk0 -> chunk_eq x0 chunk0
     | _ -> false)
  | EF_malloc -> (match ef2 with
                  | EF_malloc -> true
                  | _ -> false)
  | EF_free -> (match ef2 with
                | EF_free -> true
                | _ -> false)
  | EF_memcpy (x0, x1) ->
    (match ef2 with
     | EF_memcpy (sz0, al0) -> if zeq x0 sz0 then zeq x1 al0 else false
     | _ -> false)
  | EF_annot (x0, x1, x2) ->
    (match ef2 with
     | EF_annot (kind0, text0, targs0) ->
       if ident_eq x0 kind0
       then if string_dec x1 text0 then x __ typ_eq x2 targs0 else false
       else false
     | _ -> false)
  | EF_annot_val (x0, x1, x2) ->
    (match ef2 with
     | EF_annot_val (kind0, text0, targ0) ->
       if ident_eq x0 kind0
       then if string_dec x1 text0 then typ_eq x2 targ0 else false
       else false
     | _ -> false)
  | EF_inline_asm (x0, x1, x2) ->
    (match ef2 with
     | EF_inline_asm (text0, sg0, clobbers0) ->
       if string_dec x0 text0
       then if signature_eq x1 sg0
            then x __ string_dec x2 clobbers0
            else false
       else false
     | _ -> false)
  | EF_debug (x0, x1, x2) ->
    (match ef2 with
     | EF_debug (kind0, text0, targs0) ->
       if ident_eq x0 kind0
       then if ident_eq x1 text0 then x __ typ_eq x2 targs0 else false
       else false
     | _ -> false))

type 'f fundef =
| Internal of 'f
| External of external_function

(** val transf_fundef : ('a1 -> 'a2) -> 'a1 fundef -> 'a2 fundef **)

let transf_fundef transf = function
| Internal f -> Internal (transf f)
| External ef -> External ef

(** val transf_partial_fundef :
    ('a1 -> 'a2 res) -> 'a1 fundef -> 'a2 fundef res **)

let transf_partial_fundef transf_partial = function
| Internal f ->
  (match transf_partial f with
   | OK x -> OK (Internal x)
   | Error msg -> Error msg)
| External ef -> OK (External ef)

type 'a rpair =
| One of 'a
| Twolong of 'a * 'a

(** val map_rpair : ('a1 -> 'a2) -> 'a1 rpair -> 'a2 rpair **)

let map_rpair f = function
| One r -> One (f r)
| Twolong (rhi, rlo) -> Twolong ((f rhi), (f rlo))

(** val regs_of_rpair : 'a1 rpair -> 'a1 list **)

let regs_of_rpair = function
| One r -> r :: []
| Twolong (rhi, rlo) -> rhi :: (rlo :: [])

(** val regs_of_rpairs : 'a1 rpair list -> 'a1 list **)

let rec regs_of_rpairs = function
| [] -> []
| p :: l0 -> app (regs_of_rpair p) (regs_of_rpairs l0)

type 'a builtin_arg =
| BA of 'a
| BA_int of Int.int
| BA_long of Int64.int
| BA_float of float
| BA_single of float32
| BA_loadstack of memory_chunk * Ptrofs.int
| BA_addrstack of Ptrofs.int
| BA_loadglobal of memory_chunk * ident * Ptrofs.int
| BA_addrglobal of ident * Ptrofs.int
| BA_splitlong of 'a builtin_arg * 'a builtin_arg
| BA_addptr of 'a builtin_arg * 'a builtin_arg

type 'a builtin_res =
| BR of 'a
| BR_none
| BR_splitlong of 'a builtin_res * 'a builtin_res

(** val globals_of_builtin_arg : 'a1 builtin_arg -> ident list **)

let rec globals_of_builtin_arg = function
| BA_loadglobal (_, id, _) -> id :: []
| BA_addrglobal (id, _) -> id :: []
| BA_splitlong (hi, lo) ->
  app (globals_of_builtin_arg hi) (globals_of_builtin_arg lo)
| BA_addptr (a1, a2) ->
  app (globals_of_builtin_arg a1) (globals_of_builtin_arg a2)
| _ -> []

(** val globals_of_builtin_args : 'a1 builtin_arg list -> ident list **)

let globals_of_builtin_args al =
  fold_right (fun a l -> app (globals_of_builtin_arg a) l) [] al

(** val params_of_builtin_arg : 'a1 builtin_arg -> 'a1 list **)

let rec params_of_builtin_arg = function
| BA x -> x :: []
| BA_splitlong (hi, lo) ->
  app (params_of_builtin_arg hi) (params_of_builtin_arg lo)
| BA_addptr (a1, a2) ->
  app (params_of_builtin_arg a1) (params_of_builtin_arg a2)
| _ -> []

(** val params_of_builtin_args : 'a1 builtin_arg list -> 'a1 list **)

let params_of_builtin_args al =
  fold_right (fun a l -> app (params_of_builtin_arg a) l) [] al

(** val params_of_builtin_res : 'a1 builtin_res -> 'a1 list **)

let rec params_of_builtin_res = function
| BR x -> x :: []
| BR_none -> []
| BR_splitlong (hi, lo) ->
  app (params_of_builtin_res hi) (params_of_builtin_res lo)

(** val map_builtin_arg :
    ('a1 -> 'a2) -> 'a1 builtin_arg -> 'a2 builtin_arg **)

let rec map_builtin_arg f = function
| BA x -> BA (f x)
| BA_int n -> BA_int n
| BA_long n -> BA_long n
| BA_float n -> BA_float n
| BA_single n -> BA_single n
| BA_loadstack (chunk, ofs) -> BA_loadstack (chunk, ofs)
| BA_addrstack ofs -> BA_addrstack ofs
| BA_loadglobal (chunk, id, ofs) -> BA_loadglobal (chunk, id, ofs)
| BA_addrglobal (id, ofs) -> BA_addrglobal (id, ofs)
| BA_splitlong (hi, lo) ->
  BA_splitlong ((map_builtin_arg f hi), (map_builtin_arg f lo))
| BA_addptr (a1, a2) ->
  BA_addptr ((map_builtin_arg f a1), (map_builtin_arg f a2))

(** val map_builtin_res :
    ('a1 -> 'a2) -> 'a1 builtin_res -> 'a2 builtin_res **)

let rec map_builtin_res f = function
| BR x -> BR (f x)
| BR_none -> BR_none
| BR_splitlong (hi, lo) ->
  BR_splitlong ((map_builtin_res f hi), (map_builtin_res f lo))

type builtin_arg_constraint =
| OK_default
| OK_const
| OK_addrstack
| OK_addressing
| OK_all
