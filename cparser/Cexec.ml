open AST
open Archi
open BinInt
open BinNums
open Builtins
open Builtins0
open Cop
open Coqlib
open Csem
open Csyntax
open Ctypes
open Datatypes
open Decidableplus
open Determinism
open Errors
open Events
open Globalenvs
open Integers
open List0
open Maps
open Memdata
open Memory
open Values
open Znumtheory

(** val is_val : expr -> (coq_val * coq_type) option **)

let is_val = function
| Eval (v, ty) -> Some (v, ty)
| _ -> None

(** val is_loc :
    expr -> (((block * Ptrofs.int) * bitfield) * coq_type) option **)

let is_loc = function
| Eloc (b, ofs, bf, ty) -> Some (((b, ofs), bf), ty)
| _ -> None

(** val is_val_list : exprlist -> (coq_val * coq_type) list option **)

let rec is_val_list = function
| Enil -> Some []
| Econs (a1, al0) ->
  (match is_val a1 with
   | Some vt1 ->
     (match is_val_list al0 with
      | Some vtl -> Some (vt1 :: vtl)
      | None -> None)
   | None -> None)

(** val is_skip : statement -> bool **)

let is_skip = function
| Sskip -> true
| _ -> false

(** val eventval_of_val : genv -> coq_val -> typ -> eventval option **)

let eventval_of_val ge v t0 =
  match v with
  | Vundef -> None
  | Vint i -> if typ_eq t0 AST.Tint then Some (EVint i) else None
  | Vlong n -> if typ_eq t0 AST.Tlong then Some (EVlong n) else None
  | Vfloat f -> if typ_eq t0 AST.Tfloat then Some (EVfloat f) else None
  | Vsingle f -> if typ_eq t0 Tsingle then Some (EVsingle f) else None
  | Vptr (b, ofs) ->
    (match Genv.invert_symbol ge.genv_genv b with
     | Some id ->
       if Genv.public_symbol ge.genv_genv id
       then if typ_eq t0 coq_Tptr then Some (EVptr_global (id, ofs)) else None
       else None
     | None -> None)

(** val list_eventval_of_val :
    genv -> coq_val list -> typ list -> eventval list option **)

let rec list_eventval_of_val ge vl tl =
  match vl with
  | [] -> (match tl with
           | [] -> Some []
           | _ :: _ -> None)
  | v1 :: vl0 ->
    (match tl with
     | [] -> None
     | t1 :: tl0 ->
       (match eventval_of_val ge v1 t1 with
        | Some ev1 ->
          (match list_eventval_of_val ge vl0 tl0 with
           | Some evl -> Some (ev1 :: evl)
           | None -> None)
        | None -> None))

(** val val_of_eventval : genv -> eventval -> typ -> coq_val option **)

let val_of_eventval ge ev t0 =
  match ev with
  | EVint i -> if typ_eq t0 AST.Tint then Some (Vint i) else None
  | EVlong n -> if typ_eq t0 AST.Tlong then Some (Vlong n) else None
  | EVfloat f -> if typ_eq t0 AST.Tfloat then Some (Vfloat f) else None
  | EVsingle f -> if typ_eq t0 Tsingle then Some (Vsingle f) else None
  | EVptr_global (id, ofs) ->
    if Genv.public_symbol ge.genv_genv id
    then if typ_eq t0 coq_Tptr
         then (match Genv.find_symbol ge.genv_genv id with
               | Some b -> Some (Vptr (b, ofs))
               | None -> None)
         else None
    else None

(** val do_volatile_load :
    genv -> world -> memory_chunk -> Mem.mem -> block -> Ptrofs.int ->
    ((world * trace) * coq_val) option **)

let do_volatile_load ge w chunk m b ofs =
  if Genv.block_is_volatile ge.genv_genv b
  then (match Genv.invert_symbol ge.genv_genv b with
        | Some id ->
          (match nextworld_vload w chunk id ofs with
           | Some p ->
             let (res, w') = p in
             (match val_of_eventval ge res (type_of_chunk chunk) with
              | Some vres ->
                Some ((w', ((Event_vload (chunk, id, ofs, res)) :: [])),
                  (Val.load_result chunk vres))
              | None -> None)
           | None -> None)
        | None -> None)
  else (match Mem.load chunk m b (Ptrofs.unsigned ofs) with
        | Some v -> Some ((w, coq_E0), v)
        | None -> None)

(** val do_volatile_store :
    genv -> world -> memory_chunk -> Mem.mem -> block -> Ptrofs.int ->
    coq_val -> (((world * trace) * Mem.mem) * coq_val) option **)

let do_volatile_store ge w chunk m b ofs v =
  if Genv.block_is_volatile ge.genv_genv b
  then (match Genv.invert_symbol ge.genv_genv b with
        | Some id ->
          (match eventval_of_val ge (Val.load_result chunk v)
                   (type_of_chunk chunk) with
           | Some ev ->
             (match nextworld_vstore w chunk id ofs ev with
              | Some w' ->
                Some (((w', ((Event_vstore (chunk, id, ofs, ev)) :: [])), m),
                  v)
              | None -> None)
           | None -> None)
        | None -> None)
  else (match Mem.store chunk m b (Ptrofs.unsigned ofs) v with
        | Some m' -> Some (((w, coq_E0), m'), v)
        | None -> None)

(** val do_deref_loc :
    genv -> world -> coq_type -> Mem.mem -> block -> Ptrofs.int -> bitfield
    -> ((world * trace) * coq_val) option **)

let do_deref_loc ge w ty m b ofs = function
| Full ->
  (match access_mode ty with
   | By_value chunk ->
     if type_is_volatile ty
     then do_volatile_load ge w chunk m b ofs
     else (match Mem.loadv chunk m (Vptr (b, ofs)) with
           | Some v -> Some ((w, coq_E0), v)
           | None -> None)
   | By_nothing -> None
   | _ -> Some ((w, coq_E0), (Vptr (b, ofs))))
| Bits (sz, sg, pos, width) ->
  (match ty with
   | Tint (sz1, sg1, _) ->
     if (&&)
          ((&&)
            ((&&)
              ((&&)
                ((&&) ((fun x -> x) (intsize_eq sz1 sz))
                  ((fun x -> x)
                    (signedness_eq sg1
                      (if zlt width (bitsize_intsize sz) then Signed else sg))))
                ((fun x -> x) (zle Z0 pos))) ((fun x -> x) (zlt Z0 width)))
            ((fun x -> x) (zle width (bitsize_intsize sz))))
          ((fun x -> x) (zle (Z.add pos width) (bitsize_carrier sz)))
     then (match Mem.loadv (chunk_for_carrier sz) m (Vptr (b, ofs)) with
           | Some v ->
             (match v with
              | Vint c ->
                Some ((w, coq_E0), (Vint
                  (bitfield_extract sz sg pos width c)))
              | _ -> None)
           | None -> None)
     else None
   | _ -> None)

(** val check_assign_copy :
    genv -> coq_type -> block -> Ptrofs.int -> block -> Ptrofs.int -> bool **)

let check_assign_copy ge ty b ofs b' ofs' =
  let s =
    coq_Zdivide_dec (alignof_blockcopy ge.genv_cenv ty) (Ptrofs.unsigned ofs')
  in
  if s
  then let s0 =
         coq_Zdivide_dec (alignof_blockcopy ge.genv_cenv ty)
           (Ptrofs.unsigned ofs)
       in
       if s0
       then let s1 = eq_block b' b in
            if s1
            then let s2 = zeq (Ptrofs.unsigned ofs') (Ptrofs.unsigned ofs) in
                 if s2
                 then true
                 else let s3 =
                        zle
                          (Z.add (Ptrofs.unsigned ofs')
                            (sizeof ge.genv_cenv ty)) (Ptrofs.unsigned ofs)
                      in
                      if s3
                      then true
                      else zle
                             (Z.add (Ptrofs.unsigned ofs)
                               (sizeof ge.genv_cenv ty))
                             (Ptrofs.unsigned ofs')
            else true
       else false
  else false

(** val do_assign_loc :
    genv -> world -> coq_type -> Mem.mem -> block -> Ptrofs.int -> bitfield
    -> coq_val -> (((world * trace) * Mem.mem) * coq_val) option **)

let do_assign_loc ge w ty m b ofs bf v =
  match bf with
  | Full ->
    (match access_mode ty with
     | By_value chunk ->
       if type_is_volatile ty
       then do_volatile_store ge w chunk m b ofs v
       else (match Mem.storev chunk m (Vptr (b, ofs)) v with
             | Some m' -> Some (((w, coq_E0), m'), v)
             | None -> None)
     | By_copy ->
       (match v with
        | Vptr (b', ofs') ->
          if check_assign_copy ge ty b ofs b' ofs'
          then (match Mem.loadbytes m b' (Ptrofs.unsigned ofs')
                        (sizeof ge.genv_cenv ty) with
                | Some bytes ->
                  (match Mem.storebytes m b (Ptrofs.unsigned ofs) bytes with
                   | Some m' -> Some (((w, coq_E0), m'), v)
                   | None -> None)
                | None -> None)
          else None
        | _ -> None)
     | _ -> None)
  | Bits (sz, sg, pos, width) ->
    if (&&)
         ((&&)
           ((&&) ((fun x -> x) (zle Z0 pos)) ((fun x -> x) (zlt Z0 width)))
           ((fun x -> x) (zle width (bitsize_intsize sz))))
         ((fun x -> x) (zle (Z.add pos width) (bitsize_carrier sz)))
    then (match ty with
          | Tint (sz1, sg1, _) ->
            (match v with
             | Vint n ->
               (match Mem.loadv (chunk_for_carrier sz) m (Vptr (b, ofs)) with
                | Some v0 ->
                  (match v0 with
                   | Vint c ->
                     if (&&) ((fun x -> x) (intsize_eq sz1 sz))
                          ((fun x -> x)
                            (signedness_eq sg1
                              (if zlt width (bitsize_intsize sz)
                               then Signed
                               else sg)))
                     then (match Mem.storev (chunk_for_carrier sz) m (Vptr
                                   (b, ofs)) (Vint
                                   (Int.bitfield_insert
                                     (first_bit sz pos width) width c n)) with
                           | Some m' ->
                             Some (((w, coq_E0), m'), (Vint
                               (bitfield_normalize sz sg width n)))
                           | None -> None)
                     else None
                   | _ -> None)
                | None -> None)
             | _ -> None)
          | _ -> None)
    else None

(** val do_ef_volatile_load :
    genv -> memory_chunk -> world -> coq_val list -> Mem.mem ->
    (((world * trace) * coq_val) * Mem.mem) option **)

let do_ef_volatile_load ge chunk w vargs m =
  match vargs with
  | [] -> None
  | v :: l ->
    (match v with
     | Vptr (b, ofs) ->
       (match l with
        | [] ->
          (match do_volatile_load ge w chunk m b ofs with
           | Some p -> Some (p, m)
           | None -> None)
        | _ :: _ -> None)
     | _ -> None)

(** val do_ef_volatile_store :
    genv -> memory_chunk -> world -> coq_val list -> Mem.mem ->
    (((world * trace) * coq_val) * Mem.mem) option **)

let do_ef_volatile_store ge chunk w vargs m =
  match vargs with
  | [] -> None
  | v0 :: l ->
    (match v0 with
     | Vptr (b, ofs) ->
       (match l with
        | [] -> None
        | v :: l0 ->
          (match l0 with
           | [] ->
             (match do_volatile_store ge w chunk m b ofs v with
              | Some p ->
                let (p0, _) = p in
                let (p1, m') = p0 in Some ((p1, Vundef), m')
              | None -> None)
           | _ :: _ -> None))
     | _ -> None)

(** val do_alloc_size : coq_val -> Ptrofs.int option **)

let do_alloc_size = function
| Vint n -> if ptr64 then None else Some (Ptrofs.of_int n)
| Vlong n -> if ptr64 then Some (Ptrofs.of_int64 n) else None
| _ -> None

(** val do_ef_malloc :
    world -> coq_val list -> Mem.mem ->
    (((world * trace) * coq_val) * Mem.mem) option **)

let do_ef_malloc w vargs m =
  match vargs with
  | [] -> None
  | v :: l ->
    (match l with
     | [] ->
       (match do_alloc_size v with
        | Some sz ->
          let (m', b) =
            Mem.alloc m (Z.opp (size_chunk coq_Mptr)) (Ptrofs.unsigned sz)
          in
          (match Mem.store coq_Mptr m' b (Z.opp (size_chunk coq_Mptr)) v with
           | Some m'' -> Some (((w, coq_E0), (Vptr (b, Ptrofs.zero))), m'')
           | None -> None)
        | None -> None)
     | _ :: _ -> None)

(** val do_ef_free :
    world -> coq_val list -> Mem.mem ->
    (((world * trace) * coq_val) * Mem.mem) option **)

let do_ef_free w vargs m =
  match vargs with
  | [] -> None
  | v :: l ->
    (match v with
     | Vint n ->
       (match l with
        | [] ->
          if (&&) ((fun x -> x) (Int.eq_dec n Int.zero)) (negb ptr64)
          then Some (((w, coq_E0), Vundef), m)
          else None
        | _ :: _ -> None)
     | Vlong n ->
       (match l with
        | [] ->
          if (&&) ((fun x -> x) (Int64.eq_dec n Int64.zero)) ptr64
          then Some (((w, coq_E0), Vundef), m)
          else None
        | _ :: _ -> None)
     | Vptr (b, lo) ->
       (match l with
        | [] ->
          (match Mem.load coq_Mptr m b
                   (Z.sub (Ptrofs.unsigned lo) (size_chunk coq_Mptr)) with
           | Some vsz ->
             (match do_alloc_size vsz with
              | Some sz ->
                if zlt Z0 (Ptrofs.unsigned sz)
                then (match Mem.free m b
                              (Z.sub (Ptrofs.unsigned lo)
                                (size_chunk coq_Mptr))
                              (Z.add (Ptrofs.unsigned lo)
                                (Ptrofs.unsigned sz)) with
                      | Some m' -> Some (((w, coq_E0), Vundef), m')
                      | None -> None)
                else None
              | None -> None)
           | None -> None)
        | _ :: _ -> None)
     | _ -> None)

(** val do_ef_memcpy :
    coq_Z -> coq_Z -> world -> coq_val list -> Mem.mem ->
    (((world * trace) * coq_val) * Mem.mem) option **)

let do_ef_memcpy sz al w vargs m =
  match vargs with
  | [] -> None
  | v :: l ->
    (match v with
     | Vptr (bdst, odst) ->
       (match l with
        | [] -> None
        | v0 :: l0 ->
          (match v0 with
           | Vptr (bsrc, osrc) ->
             (match l0 with
              | [] ->
                if (&&)
                     ((||) (coq_Decidable_eq_Z al (Zpos Coq_xH))
                       ((||) (coq_Decidable_eq_Z al (Zpos (Coq_xO Coq_xH)))
                         ((||)
                           (coq_Decidable_eq_Z al (Zpos (Coq_xO (Coq_xO
                             Coq_xH))))
                           (coq_Decidable_eq_Z al (Zpos (Coq_xO (Coq_xO
                             (Coq_xO Coq_xH))))))))
                     ((&&) (coq_Decidable_ge_Z sz Z0)
                       ((&&) (coq_Decidable_divides al sz)
                         ((&&)
                           (if coq_Decidable_gt_Z sz Z0
                            then coq_Decidable_divides al
                                   (Ptrofs.unsigned osrc)
                            else true)
                           ((&&)
                             (if coq_Decidable_gt_Z sz Z0
                              then coq_Decidable_divides al
                                     (Ptrofs.unsigned odst)
                              else true)
                             ((||)
                               (negb (coq_Decidable_eq_positive bsrc bdst))
                               ((||)
                                 (coq_Decidable_eq_Z (Ptrofs.unsigned osrc)
                                   (Ptrofs.unsigned odst))
                                 ((||)
                                   (coq_Decidable_le_Z
                                     (Z.add (Ptrofs.unsigned osrc) sz)
                                     (Ptrofs.unsigned odst))
                                   (coq_Decidable_le_Z
                                     (Z.add (Ptrofs.unsigned odst) sz)
                                     (Ptrofs.unsigned osrc)))))))))
                then (match Mem.loadbytes m bsrc (Ptrofs.unsigned osrc) sz with
                      | Some bytes ->
                        (match Mem.storebytes m bdst (Ptrofs.unsigned odst)
                                 bytes with
                         | Some m' -> Some (((w, coq_E0), Vundef), m')
                         | None -> None)
                      | None -> None)
                else None
              | _ :: _ -> None)
           | _ -> None))
     | _ -> None)

(** val do_ef_annot :
    genv -> char list -> typ list -> world -> coq_val list -> Mem.mem ->
    (((world * trace) * coq_val) * Mem.mem) option **)

let do_ef_annot ge text targs w vargs m =
  match list_eventval_of_val ge vargs targs with
  | Some args ->
    Some (((w, ((Event_annot (text, args)) :: coq_E0)), Vundef), m)
  | None -> None

(** val do_ef_annot_val :
    genv -> char list -> typ -> world -> coq_val list -> Mem.mem ->
    (((world * trace) * coq_val) * Mem.mem) option **)

let do_ef_annot_val ge text targ w vargs m =
  match vargs with
  | [] -> None
  | varg :: l ->
    (match l with
     | [] ->
       (match eventval_of_val ge varg targ with
        | Some arg ->
          Some (((w, ((Event_annot (text, (arg :: []))) :: coq_E0)), varg), m)
        | None -> None)
     | _ :: _ -> None)

(** val do_ef_debug :
    positive -> ident -> typ list -> world -> coq_val list -> Mem.mem ->
    (((world * trace) * coq_val) * Mem.mem) option **)

let do_ef_debug _ _ _ w _ m =
  Some (((w, coq_E0), Vundef), m)

(** val do_builtin_or_external :
    genv -> (char list -> signature -> Senv.t -> world -> coq_val list ->
    Mem.mem -> (((world * trace) * coq_val) * Mem.mem) option) -> char list
    -> signature -> world -> coq_val list -> Mem.mem ->
    (((world * trace) * coq_val) * Mem.mem) option **)

let do_builtin_or_external ge do_external_function name sg w vargs m =
  match lookup_builtin_function name sg with
  | Some bf ->
    (match bs_sem (builtin_function_sig bf).sig_res (builtin_function_sem bf)
             vargs with
     | Some v -> Some (((w, coq_E0), v), m)
     | None -> None)
  | None -> do_external_function name sg (Genv.to_senv ge.genv_genv) w vargs m

(** val do_external :
    genv -> (char list -> signature -> Senv.t -> world -> coq_val list ->
    Mem.mem -> (((world * trace) * coq_val) * Mem.mem) option) -> (char list
    -> signature -> Senv.t -> world -> coq_val list -> Mem.mem ->
    (((world * trace) * coq_val) * Mem.mem) option) -> external_function ->
    world -> coq_val list -> Mem.mem ->
    (((world * trace) * coq_val) * Mem.mem) option **)

let do_external ge do_external_function do_inline_assembly = function
| EF_external (name, sg) ->
  do_external_function name sg (Genv.to_senv ge.genv_genv)
| EF_builtin (name, sg) ->
  do_builtin_or_external ge do_external_function name sg
| EF_runtime (name, sg) ->
  do_builtin_or_external ge do_external_function name sg
| EF_vload chunk -> do_ef_volatile_load ge chunk
| EF_vstore chunk -> do_ef_volatile_store ge chunk
| EF_malloc -> do_ef_malloc
| EF_free -> do_ef_free
| EF_memcpy (sz, al) -> do_ef_memcpy sz al
| EF_annot (_, text, targs) -> do_ef_annot ge text targs
| EF_annot_val (_, text, targ) -> do_ef_annot_val ge text targ
| EF_inline_asm (text, sg, _) ->
  do_inline_assembly text sg (Genv.to_senv ge.genv_genv)
| EF_debug (kind0, text, targs) -> do_ef_debug kind0 text targs

type reduction =
| Lred of char list * expr * Mem.mem
| Rred of char list * expr * Mem.mem * trace
| Callred of char list * Csyntax.fundef * coq_val list * coq_type * Mem.mem
| Stuckred

(** val sem_cast_arguments :
    (coq_val * coq_type) list -> typelist -> Mem.mem -> coq_val list option **)

let rec sem_cast_arguments vtl tl m =
  match vtl with
  | [] -> (match tl with
           | Tnil -> Some []
           | Tcons (_, _) -> None)
  | p :: vtl0 ->
    let (v1, t1) = p in
    (match tl with
     | Tnil -> None
     | Tcons (t1', tl0) ->
       (match sem_cast v1 t1 t1' m with
        | Some v ->
          (match sem_cast_arguments vtl0 tl0 m with
           | Some vl -> Some (v :: vl)
           | None -> None)
        | None -> None))

type 'a reducts = ((expr -> 'a) * reduction) list

(** val topred : reduction -> expr reducts **)

let topred r =
  ((fun x -> x), r) :: []

(** val stuck : expr reducts **)

let stuck =
  ((fun x -> x), Stuckred) :: []

(** val incontext : ('a1 -> 'a2) -> 'a1 reducts -> 'a2 reducts **)

let incontext ctx ll =
  map (fun z -> ((fun x -> ctx (fst z x)), (snd z))) ll

(** val incontext2 :
    ('a1 -> 'a3) -> 'a1 reducts -> ('a2 -> 'a3) -> 'a2 reducts -> 'a3 reducts **)

let incontext2 ctx1 ll1 ctx2 ll2 =
  app (incontext ctx1 ll1) (incontext ctx2 ll2)

(** val step_expr :
    genv -> (char list -> signature -> Senv.t -> world -> coq_val list ->
    Mem.mem -> (((world * trace) * coq_val) * Mem.mem) option) -> (char list
    -> signature -> Senv.t -> world -> coq_val list -> Mem.mem ->
    (((world * trace) * coq_val) * Mem.mem) option) -> env -> world -> kind
    -> expr -> Mem.mem -> expr reducts **)

let step_expr ge do_external_function do_inline_assembly e w =
  let rec step_expr0 k a m =
    match k with
    | LV ->
      (match a with
       | Evar (x, ty) ->
         (match PTree.get x e with
          | Some p ->
            let (b, ty') = p in
            if type_eq ty ty'
            then topred (Lred
                   (('r'::('e'::('d'::('_'::('v'::('a'::('r'::('_'::('l'::('o'::('c'::('a'::('l'::[]))))))))))))),
                   (Eloc (b, Ptrofs.zero, Full, ty)), m))
            else stuck
          | None ->
            (match Genv.find_symbol ge.genv_genv x with
             | Some b ->
               topred (Lred
                 (('r'::('e'::('d'::('_'::('v'::('a'::('r'::('_'::('g'::('l'::('o'::('b'::('a'::('l'::[])))))))))))))),
                 (Eloc (b, Ptrofs.zero, Full, ty)), m))
             | None -> stuck))
       | Efield (r, f, ty) ->
         (match is_val r with
          | Some p ->
            let (v, ty') = p in
            (match v with
             | Vptr (b, ofs) ->
               (match ty' with
                | Tstruct (id, _) ->
                  (match PTree.get id ge.genv_cenv with
                   | Some co ->
                     (match field_offset ge.genv_cenv f co.co_members with
                      | OK p0 ->
                        let (delta, bf) = p0 in
                        topred (Lred
                          (('r'::('e'::('d'::('_'::('f'::('i'::('e'::('l'::('d'::('_'::('s'::('t'::('r'::('u'::('c'::('t'::[])))))))))))))))),
                          (Eloc (b, (Ptrofs.add ofs (Ptrofs.repr delta)), bf,
                          ty)), m))
                      | Error _ -> stuck)
                   | None -> stuck)
                | Tunion (id, _) ->
                  (match PTree.get id ge.genv_cenv with
                   | Some co ->
                     (match union_field_offset ge.genv_cenv f co.co_members with
                      | OK p0 ->
                        let (delta, bf) = p0 in
                        topred (Lred
                          (('r'::('e'::('d'::('_'::('f'::('i'::('e'::('l'::('d'::('_'::('u'::('n'::('i'::('o'::('n'::[]))))))))))))))),
                          (Eloc (b, (Ptrofs.add ofs (Ptrofs.repr delta)), bf,
                          ty)), m))
                      | Error _ -> stuck)
                   | None -> stuck)
                | _ -> stuck)
             | _ -> stuck)
          | None -> incontext (fun x -> Efield (x, f, ty)) (step_expr0 RV r m))
       | Ederef (r, ty) ->
         (match is_val r with
          | Some p ->
            let (v, _) = p in
            (match v with
             | Vptr (b, ofs) ->
               topred (Lred
                 (('r'::('e'::('d'::('_'::('d'::('e'::('r'::('e'::('f'::[]))))))))),
                 (Eloc (b, ofs, Full, ty)), m))
             | _ -> stuck)
          | None -> incontext (fun x -> Ederef (x, ty)) (step_expr0 RV r m))
       | Eloc (_, _, _, _) -> []
       | _ -> stuck)
    | RV ->
      (match a with
       | Eval (_, _) -> []
       | Evalof (l, ty) ->
         (match is_loc l with
          | Some p ->
            let (p0, ty') = p in
            let (p1, bf) = p0 in
            let (b, ofs) = p1 in
            if type_eq ty ty'
            then (match do_deref_loc ge w ty m b ofs bf with
                  | Some p2 ->
                    let (p3, v) = p2 in
                    let (_, t0) = p3 in
                    topred (Rred
                      (('r'::('e'::('d'::('_'::('r'::('v'::('a'::('l'::('o'::('f'::[])))))))))),
                      (Eval (v, ty)), m, t0))
                  | None -> stuck)
            else stuck
          | None -> incontext (fun x -> Evalof (x, ty)) (step_expr0 LV l m))
       | Eaddrof (l, ty) ->
         (match is_loc l with
          | Some p ->
            let (p0, _) = p in
            let (p1, bf) = p0 in
            let (b, ofs) = p1 in
            (match bf with
             | Full ->
               topred (Rred
                 (('r'::('e'::('d'::('_'::('a'::('d'::('d'::('r'::('o'::('f'::[])))))))))),
                 (Eval ((Vptr (b, ofs)), ty)), m, coq_E0))
             | Bits (_, _, _, _) -> stuck)
          | None -> incontext (fun x -> Eaddrof (x, ty)) (step_expr0 LV l m))
       | Eunop (op, r1, ty) ->
         (match is_val r1 with
          | Some p ->
            let (v1, ty1) = p in
            (match sem_unary_operation op v1 ty1 m with
             | Some v ->
               topred (Rred
                 (('r'::('e'::('d'::('_'::('u'::('n'::('o'::('p'::[])))))))),
                 (Eval (v, ty)), m, coq_E0))
             | None -> stuck)
          | None ->
            incontext (fun x -> Eunop (op, x, ty)) (step_expr0 RV r1 m))
       | Ebinop (op, r1, r2, ty) ->
         (match is_val r1 with
          | Some p ->
            let (v1, ty1) = p in
            (match is_val r2 with
             | Some p0 ->
               let (v2, ty2) = p0 in
               (match sem_binary_operation ge.genv_cenv op v1 ty1 v2 ty2 m with
                | Some v ->
                  topred (Rred
                    (('r'::('e'::('d'::('_'::('b'::('i'::('n'::('o'::('p'::[]))))))))),
                    (Eval (v, ty)), m, coq_E0))
                | None -> stuck)
             | None ->
               incontext2 (fun x -> Ebinop (op, x, r2, ty))
                 (step_expr0 RV r1 m) (fun x -> Ebinop (op, r1, x, ty))
                 (step_expr0 RV r2 m))
          | None ->
            incontext2 (fun x -> Ebinop (op, x, r2, ty)) (step_expr0 RV r1 m)
              (fun x -> Ebinop (op, r1, x, ty)) (step_expr0 RV r2 m))
       | Ecast (r1, ty) ->
         (match is_val r1 with
          | Some p ->
            let (v1, ty1) = p in
            (match sem_cast v1 ty1 ty m with
             | Some v ->
               topred (Rred
                 (('r'::('e'::('d'::('_'::('c'::('a'::('s'::('t'::[])))))))),
                 (Eval (v, ty)), m, coq_E0))
             | None -> stuck)
          | None -> incontext (fun x -> Ecast (x, ty)) (step_expr0 RV r1 m))
       | Eseqand (r1, r2, ty) ->
         (match is_val r1 with
          | Some p ->
            let (v1, ty1) = p in
            (match bool_val v1 ty1 m with
             | Some b ->
               if b
               then topred (Rred
                      (('r'::('e'::('d'::('_'::('s'::('e'::('q'::('a'::('n'::('d'::('_'::('t'::('r'::('u'::('e'::[]))))))))))))))),
                      (Eparen (r2, type_bool, ty)), m, coq_E0))
               else topred (Rred
                      (('r'::('e'::('d'::('_'::('s'::('e'::('q'::('a'::('n'::('d'::('_'::('f'::('a'::('l'::('s'::('e'::[])))))))))))))))),
                      (Eval ((Vint Int.zero), ty)), m, coq_E0))
             | None -> stuck)
          | None ->
            incontext (fun x -> Eseqand (x, r2, ty)) (step_expr0 RV r1 m))
       | Eseqor (r1, r2, ty) ->
         (match is_val r1 with
          | Some p ->
            let (v1, ty1) = p in
            (match bool_val v1 ty1 m with
             | Some b ->
               if b
               then topred (Rred
                      (('r'::('e'::('d'::('_'::('s'::('e'::('q'::('o'::('r'::('_'::('t'::('r'::('u'::('e'::[])))))))))))))),
                      (Eval ((Vint Int.one), ty)), m, coq_E0))
               else topred (Rred
                      (('r'::('e'::('d'::('_'::('s'::('e'::('q'::('o'::('r'::('_'::('f'::('a'::('l'::('s'::('e'::[]))))))))))))))),
                      (Eparen (r2, type_bool, ty)), m, coq_E0))
             | None -> stuck)
          | None ->
            incontext (fun x -> Eseqor (x, r2, ty)) (step_expr0 RV r1 m))
       | Econdition (r1, r2, r3, ty) ->
         (match is_val r1 with
          | Some p ->
            let (v1, ty1) = p in
            (match bool_val v1 ty1 m with
             | Some b ->
               topred (Rred
                 (('r'::('e'::('d'::('_'::('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::[]))))))))))))),
                 (Eparen ((if b then r2 else r3), ty, ty)), m, coq_E0))
             | None -> stuck)
          | None ->
            incontext (fun x -> Econdition (x, r2, r3, ty))
              (step_expr0 RV r1 m))
       | Esizeof (ty', ty) ->
         topred (Rred
           (('r'::('e'::('d'::('_'::('s'::('i'::('z'::('e'::('o'::('f'::[])))))))))),
           (Eval ((coq_Vptrofs (Ptrofs.repr (sizeof ge.genv_cenv ty'))),
           ty)), m, coq_E0))
       | Ealignof (ty', ty) ->
         topred (Rred
           (('r'::('e'::('d'::('_'::('a'::('l'::('i'::('g'::('n'::('o'::('f'::[]))))))))))),
           (Eval ((coq_Vptrofs (Ptrofs.repr (alignof ge.genv_cenv ty'))),
           ty)), m, coq_E0))
       | Eassign (l1, r2, ty) ->
         (match is_loc l1 with
          | Some p ->
            let (p0, ty1) = p in
            let (p1, bf) = p0 in
            let (b, ofs) = p1 in
            (match is_val r2 with
             | Some p2 ->
               let (v2, ty2) = p2 in
               if type_eq ty1 ty
               then (match sem_cast v2 ty2 ty1 m with
                     | Some v ->
                       (match do_assign_loc ge w ty1 m b ofs bf v with
                        | Some p3 ->
                          let (p4, v') = p3 in
                          let (p5, m') = p4 in
                          let (_, t0) = p5 in
                          topred (Rred
                            (('r'::('e'::('d'::('_'::('a'::('s'::('s'::('i'::('g'::('n'::[])))))))))),
                            (Eval (v', ty)), m', t0))
                        | None -> stuck)
                     | None -> stuck)
               else stuck
             | None ->
               incontext2 (fun x -> Eassign (x, r2, ty)) (step_expr0 LV l1 m)
                 (fun x -> Eassign (l1, x, ty)) (step_expr0 RV r2 m))
          | None ->
            incontext2 (fun x -> Eassign (x, r2, ty)) (step_expr0 LV l1 m)
              (fun x -> Eassign (l1, x, ty)) (step_expr0 RV r2 m))
       | Eassignop (op, l1, r2, tyres, ty) ->
         (match is_loc l1 with
          | Some p ->
            let (p0, ty1) = p in
            let (p1, bf) = p0 in
            let (b, ofs) = p1 in
            (match is_val r2 with
             | Some p2 ->
               let (v2, ty2) = p2 in
               if type_eq ty1 ty
               then (match do_deref_loc ge w ty1 m b ofs bf with
                     | Some p3 ->
                       let (p4, v1) = p3 in
                       let (_, t0) = p4 in
                       let r' = Eassign ((Eloc (b, ofs, bf, ty1)), (Ebinop
                         (op, (Eval (v1, ty1)), (Eval (v2, ty2)), tyres)),
                         ty1)
                       in
                       topred (Rred
                         (('r'::('e'::('d'::('_'::('a'::('s'::('s'::('i'::('g'::('n'::('o'::('p'::[])))))))))))),
                         r', m, t0))
                     | None -> stuck)
               else stuck
             | None ->
               incontext2 (fun x -> Eassignop (op, x, r2, tyres, ty))
                 (step_expr0 LV l1 m) (fun x -> Eassignop (op, l1, x, tyres,
                 ty)) (step_expr0 RV r2 m))
          | None ->
            incontext2 (fun x -> Eassignop (op, x, r2, tyres, ty))
              (step_expr0 LV l1 m) (fun x -> Eassignop (op, l1, x, tyres,
              ty)) (step_expr0 RV r2 m))
       | Epostincr (id, l, ty) ->
         (match is_loc l with
          | Some p ->
            let (p0, ty1) = p in
            let (p1, bf) = p0 in
            let (b, ofs) = p1 in
            if type_eq ty1 ty
            then (match do_deref_loc ge w ty m b ofs bf with
                  | Some p2 ->
                    let (p3, v1) = p2 in
                    let (_, t0) = p3 in
                    let op = match id with
                             | Incr -> Oadd
                             | Decr -> Osub in
                    let r' = Ecomma ((Eassign ((Eloc (b, ofs, bf, ty)),
                      (Ebinop (op, (Eval (v1, ty)), (Eval ((Vint Int.one),
                      type_int32s)), (incrdecr_type ty))), ty)), (Eval (v1,
                      ty)), ty)
                    in
                    topred (Rred
                      (('r'::('e'::('d'::('_'::('p'::('o'::('s'::('t'::('i'::('n'::('c'::('r'::[])))))))))))),
                      r', m, t0))
                  | None -> stuck)
            else stuck
          | None ->
            incontext (fun x -> Epostincr (id, x, ty)) (step_expr0 LV l m))
       | Ecomma (r1, r2, ty) ->
         (match is_val r1 with
          | Some _ ->
            if type_eq (typeof r2) ty
            then topred (Rred
                   (('r'::('e'::('d'::('_'::('c'::('o'::('m'::('m'::('a'::[]))))))))),
                   r2, m, coq_E0))
            else stuck
          | None ->
            incontext (fun x -> Ecomma (x, r2, ty)) (step_expr0 RV r1 m))
       | Ecall (r1, rargs, ty) ->
         (match is_val r1 with
          | Some p ->
            let (vf, tyf) = p in
            (match is_val_list rargs with
             | Some vtl ->
               (match classify_fun tyf with
                | Coq_fun_case_f (tyargs, tyres, cconv) ->
                  (match Genv.find_funct ge.genv_genv vf with
                   | Some fd ->
                     (match sem_cast_arguments vtl tyargs m with
                      | Some vargs ->
                        if type_eq (type_of_fundef fd) (Tfunction (tyargs,
                             tyres, cconv))
                        then topred (Callred
                               (('r'::('e'::('d'::('_'::('c'::('a'::('l'::('l'::[])))))))),
                               fd, vargs, ty, m))
                        else stuck
                      | None -> stuck)
                   | None -> stuck)
                | Coq_fun_default -> stuck)
             | None ->
               incontext2 (fun x -> Ecall (x, rargs, ty))
                 (step_expr0 RV r1 m) (fun x -> Ecall (r1, x, ty))
                 (step_exprlist rargs m))
          | None ->
            incontext2 (fun x -> Ecall (x, rargs, ty)) (step_expr0 RV r1 m)
              (fun x -> Ecall (r1, x, ty)) (step_exprlist rargs m))
       | Ebuiltin (ef, tyargs, rargs, ty) ->
         (match is_val_list rargs with
          | Some vtl ->
            (match sem_cast_arguments vtl tyargs m with
             | Some vargs ->
               (match do_external ge do_external_function do_inline_assembly
                        ef w vargs m with
                | Some p ->
                  let (p0, m') = p in
                  let (p1, v) = p0 in
                  let (_, t0) = p1 in
                  topred (Rred
                    (('r'::('e'::('d'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::[]))))))))))),
                    (Eval (v, ty)), m', t0))
                | None -> stuck)
             | None -> stuck)
          | None ->
            incontext (fun x -> Ebuiltin (ef, tyargs, x, ty))
              (step_exprlist rargs m))
       | Eparen (r1, tycast, ty) ->
         (match is_val r1 with
          | Some p ->
            let (v1, ty1) = p in
            (match sem_cast v1 ty1 tycast m with
             | Some v ->
               topred (Rred
                 (('r'::('e'::('d'::('_'::('p'::('a'::('r'::('e'::('n'::[]))))))))),
                 (Eval (v, ty)), m, coq_E0))
             | None -> stuck)
          | None ->
            incontext (fun x -> Eparen (x, tycast, ty)) (step_expr0 RV r1 m))
       | _ -> stuck)
  and step_exprlist rl m =
    match rl with
    | Enil -> []
    | Econs (r1, rs) ->
      incontext2 (fun x -> Econs (x, rs)) (step_expr0 RV r1 m) (fun x ->
        Econs (r1, x)) (step_exprlist rs m)
  in step_expr0

(** val do_alloc_variables :
    genv -> env -> Mem.mem -> (ident * coq_type) list -> env * Mem.mem **)

let rec do_alloc_variables ge e m = function
| [] -> (e, m)
| p :: l' ->
  let (id, ty) = p in
  let (m1, b1) = Mem.alloc m Z0 (sizeof ge.genv_cenv ty) in
  do_alloc_variables ge (PTree.set id (b1, ty) e) m1 l'

(** val sem_bind_parameters :
    genv -> world -> env -> Mem.mem -> (ident * coq_type) list -> coq_val
    list -> Mem.mem option **)

let rec sem_bind_parameters ge w e m l lv =
  match l with
  | [] -> (match lv with
           | [] -> Some m
           | _ :: _ -> None)
  | p :: params ->
    let (id, ty) = p in
    (match lv with
     | [] -> None
     | v1 :: lv0 ->
       (match PTree.get id e with
        | Some p0 ->
          let (b, ty') = p0 in
          if type_eq ty ty'
          then (match do_assign_loc ge w ty m b Ptrofs.zero Full v1 with
                | Some p1 ->
                  let (p2, _) = p1 in
                  let (p3, m1) = p2 in
                  let (_, t0) = p3 in
                  (match t0 with
                   | [] -> sem_bind_parameters ge w e m1 params lv0
                   | _ :: _ -> None)
                | None -> None)
          else None
        | None -> None))

type transition =
| TR of char list * trace * state

(** val expr_final_state :
    coq_function -> cont -> env -> ((expr -> expr) * reduction) -> transition **)

let expr_final_state f k e c_rd =
  match snd c_rd with
  | Lred (rule, a, m) ->
    TR (rule, coq_E0, (ExprState (f, (fst c_rd a), k, e, m)))
  | Rred (rule, a, m, t0) ->
    TR (rule, t0, (ExprState (f, (fst c_rd a), k, e, m)))
  | Callred (rule, fd, vargs, ty, m) ->
    TR (rule, coq_E0, (Callstate (fd, vargs, (Kcall (f, e, (fst c_rd), ty,
      k)), m)))
  | Stuckred ->
    TR
      (('s'::('t'::('e'::('p'::('_'::('s'::('t'::('u'::('c'::('k'::[])))))))))),
      coq_E0, Stuckstate)

(** val ret : char list -> state -> transition list **)

let ret rule s =
  (TR (rule, coq_E0, s)) :: []

(** val do_step :
    genv -> (char list -> signature -> Senv.t -> world -> coq_val list ->
    Mem.mem -> (((world * trace) * coq_val) * Mem.mem) option) -> (char list
    -> signature -> Senv.t -> world -> coq_val list -> Mem.mem ->
    (((world * trace) * coq_val) * Mem.mem) option) -> world -> state ->
    transition list **)

let do_step ge do_external_function do_inline_assembly w = function
| State (f, s0, k, e, m) ->
  (match s0 with
   | Sskip ->
     (match k with
      | Kstop ->
        (match Mem.free_list m (blocks_of_env ge e) with
         | Some m' ->
           ret
             ('s'::('t'::('e'::('p'::('_'::('s'::('k'::('i'::('p'::('_'::('c'::('a'::('l'::('l'::[]))))))))))))))
             (Returnstate (Vundef, k, m'))
         | None -> [])
      | Kseq (s1, k0) ->
        ret
          ('s'::('t'::('e'::('p'::('_'::('s'::('k'::('i'::('p'::('_'::('s'::('e'::('q'::[])))))))))))))
          (State (f, s1, k0, e, m))
      | Kwhile2 (x, s1, k0) ->
        ret
          ('s'::('t'::('e'::('p'::('_'::('s'::('k'::('i'::('p'::('_'::('o'::('r'::('_'::('c'::('o'::('n'::('t'::('i'::('n'::('u'::('e'::('_'::('w'::('h'::('i'::('l'::('e'::[])))))))))))))))))))))))))))
          (State (f, (Swhile (x, s1)), k0, e, m))
      | Kdowhile1 (x, s1, k0) ->
        ret
          ('s'::('t'::('e'::('p'::('_'::('s'::('k'::('i'::('p'::('_'::('o'::('r'::('_'::('c'::('o'::('n'::('t'::('i'::('n'::('u'::('e'::('_'::('d'::('o'::('w'::('h'::('i'::('l'::('e'::[])))))))))))))))))))))))))))))
          (ExprState (f, x, (Kdowhile2 (x, s1, k0)), e, m))
      | Kfor3 (a2, a3, s1, k0) ->
        ret
          ('s'::('t'::('e'::('p'::('_'::('s'::('k'::('i'::('p'::('_'::('o'::('r'::('_'::('c'::('o'::('n'::('t'::('i'::('n'::('u'::('e'::('_'::('f'::('o'::('r'::('3'::[]))))))))))))))))))))))))))
          (State (f, a3, (Kfor4 (a2, a3, s1, k0)), e, m))
      | Kfor4 (a2, a3, s1, k0) ->
        ret
          ('s'::('t'::('e'::('p'::('_'::('s'::('k'::('i'::('p'::('_'::('f'::('o'::('r'::('4'::[]))))))))))))))
          (State (f, (Sfor (Sskip, a2, a3, s1)), k0, e, m))
      | Kswitch2 k0 ->
        ret
          ('s'::('t'::('e'::('p'::('_'::('s'::('k'::('i'::('p'::('_'::('b'::('r'::('e'::('a'::('k'::('_'::('s'::('w'::('i'::('t'::('c'::('h'::[]))))))))))))))))))))))
          (State (f, Sskip, k0, e, m))
      | Kcall (_, _, _, _, _) ->
        (match Mem.free_list m (blocks_of_env ge e) with
         | Some m' ->
           ret
             ('s'::('t'::('e'::('p'::('_'::('s'::('k'::('i'::('p'::('_'::('c'::('a'::('l'::('l'::[]))))))))))))))
             (Returnstate (Vundef, k, m'))
         | None -> [])
      | _ -> [])
   | Sdo x ->
     ret ('s'::('t'::('e'::('p'::('_'::('d'::('o'::('_'::('1'::[])))))))))
       (ExprState (f, x, (Kdo k), e, m))
   | Ssequence (s1, s2) ->
     ret ('s'::('t'::('e'::('p'::('_'::('s'::('e'::('q'::[])))))))) (State
       (f, s1, (Kseq (s2, k)), e, m))
   | Sifthenelse (a, s1, s2) ->
     ret
       ('s'::('t'::('e'::('p'::('_'::('i'::('f'::('t'::('h'::('e'::('n'::('e'::('l'::('s'::('e'::('_'::('1'::[])))))))))))))))))
       (ExprState (f, a, (Kifthenelse (s1, s2, k)), e, m))
   | Swhile (x, s1) ->
     ret
       ('s'::('t'::('e'::('p'::('_'::('w'::('h'::('i'::('l'::('e'::[]))))))))))
       (ExprState (f, x, (Kwhile1 (x, s1, k)), e, m))
   | Sdowhile (a, s1) ->
     ret
       ('s'::('t'::('e'::('p'::('_'::('d'::('o'::('w'::('h'::('i'::('l'::('e'::[]))))))))))))
       (State (f, s1, (Kdowhile1 (a, s1, k)), e, m))
   | Sfor (a1, a2, a3, s1) ->
     if is_skip a1
     then ret ('s'::('t'::('e'::('p'::('_'::('f'::('o'::('r'::[]))))))))
            (ExprState (f, a2, (Kfor2 (a2, a3, s1, k)), e, m))
     else ret
            ('s'::('t'::('e'::('p'::('_'::('f'::('o'::('r'::('_'::('s'::('t'::('a'::('r'::('t'::[]))))))))))))))
            (State (f, a1, (Kseq ((Sfor (Sskip, a2, a3, s1)), k)), e, m))
   | Sbreak ->
     (match k with
      | Kseq (_, k0) ->
        ret
          ('s'::('t'::('e'::('p'::('_'::('b'::('r'::('e'::('a'::('k'::('_'::('s'::('e'::('q'::[]))))))))))))))
          (State (f, Sbreak, k0, e, m))
      | Kwhile2 (_, _, k0) ->
        ret
          ('s'::('t'::('e'::('p'::('_'::('b'::('r'::('e'::('a'::('k'::('_'::('w'::('h'::('i'::('l'::('e'::[]))))))))))))))))
          (State (f, Sskip, k0, e, m))
      | Kdowhile1 (_, _, k0) ->
        ret
          ('s'::('t'::('e'::('p'::('_'::('b'::('r'::('e'::('a'::('k'::('_'::('d'::('o'::('w'::('h'::('i'::('l'::('e'::[]))))))))))))))))))
          (State (f, Sskip, k0, e, m))
      | Kfor3 (_, _, _, k0) ->
        ret
          ('s'::('t'::('e'::('p'::('_'::('b'::('r'::('e'::('a'::('k'::('_'::('f'::('o'::('r'::('3'::[])))))))))))))))
          (State (f, Sskip, k0, e, m))
      | Kswitch2 k0 ->
        ret
          ('s'::('t'::('e'::('p'::('_'::('s'::('k'::('i'::('p'::('_'::('b'::('r'::('e'::('a'::('k'::('_'::('s'::('w'::('i'::('t'::('c'::('h'::[]))))))))))))))))))))))
          (State (f, Sskip, k0, e, m))
      | _ -> [])
   | Scontinue ->
     (match k with
      | Kseq (_, k0) ->
        ret
          ('s'::('t'::('e'::('p'::('_'::('c'::('o'::('n'::('t'::('i'::('n'::('u'::('e'::('_'::('s'::('e'::('q'::[])))))))))))))))))
          (State (f, Scontinue, k0, e, m))
      | Kwhile2 (x, s1, k0) ->
        ret
          ('s'::('t'::('e'::('p'::('_'::('s'::('k'::('i'::('p'::('_'::('o'::('r'::('_'::('c'::('o'::('n'::('t'::('i'::('n'::('u'::('e'::('_'::('w'::('h'::('i'::('l'::('e'::[])))))))))))))))))))))))))))
          (State (f, (Swhile (x, s1)), k0, e, m))
      | Kdowhile1 (x, s1, k0) ->
        ret
          ('s'::('t'::('e'::('p'::('_'::('s'::('k'::('i'::('p'::('_'::('o'::('r'::('_'::('c'::('o'::('n'::('t'::('i'::('n'::('u'::('e'::('_'::('d'::('o'::('w'::('h'::('i'::('l'::('e'::[])))))))))))))))))))))))))))))
          (ExprState (f, x, (Kdowhile2 (x, s1, k0)), e, m))
      | Kfor3 (a2, a3, s1, k0) ->
        ret
          ('s'::('t'::('e'::('p'::('_'::('s'::('k'::('i'::('p'::('_'::('o'::('r'::('_'::('c'::('o'::('n'::('t'::('i'::('n'::('u'::('e'::('_'::('f'::('o'::('r'::('3'::[]))))))))))))))))))))))))))
          (State (f, a3, (Kfor4 (a2, a3, s1, k0)), e, m))
      | Kswitch2 k0 ->
        ret
          ('s'::('t'::('e'::('p'::('_'::('c'::('o'::('n'::('t'::('i'::('n'::('u'::('e'::('_'::('s'::('w'::('i'::('t'::('c'::('h'::[]))))))))))))))))))))
          (State (f, Scontinue, k0, e, m))
      | _ -> [])
   | Sreturn o ->
     (match o with
      | Some x ->
        ret
          ('s'::('t'::('e'::('p'::('_'::('r'::('e'::('t'::('u'::('r'::('n'::('_'::('1'::[])))))))))))))
          (ExprState (f, x, (Kreturn k), e, m))
      | None ->
        (match Mem.free_list m (blocks_of_env ge e) with
         | Some m' ->
           ret
             ('s'::('t'::('e'::('p'::('_'::('r'::('e'::('t'::('u'::('r'::('n'::('_'::('0'::[])))))))))))))
             (Returnstate (Vundef, (call_cont k), m'))
         | None -> []))
   | Sswitch (x, sl) ->
     ret
       ('s'::('t'::('e'::('p'::('_'::('s'::('w'::('i'::('t'::('c'::('h'::[])))))))))))
       (ExprState (f, x, (Kswitch1 (sl, k)), e, m))
   | Slabel (_, s1) ->
     ret
       ('s'::('t'::('e'::('p'::('_'::('l'::('a'::('b'::('e'::('l'::[]))))))))))
       (State (f, s1, k, e, m))
   | Sgoto lbl ->
     (match find_label lbl f.fn_body (call_cont k) with
      | Some p ->
        let (s', k') = p in
        ret ('s'::('t'::('e'::('p'::('_'::('g'::('o'::('t'::('o'::[])))))))))
          (State (f, s', k', e, m))
      | None -> []))
| ExprState (f, a, k, e, m) ->
  (match is_val a with
   | Some p ->
     let (v, ty) = p in
     (match k with
      | Kdo k0 ->
        ret ('s'::('t'::('e'::('p'::('_'::('d'::('o'::('_'::('2'::[])))))))))
          (State (f, Sskip, k0, e, m))
      | Kifthenelse (s1, s2, k0) ->
        (match bool_val v ty m with
         | Some b ->
           ret
             ('s'::('t'::('e'::('p'::('_'::('i'::('f'::('t'::('h'::('e'::('n'::('e'::('l'::('s'::('e'::('_'::('2'::[])))))))))))))))))
             (State (f, (if b then s1 else s2), k0, e, m))
         | None -> [])
      | Kwhile1 (x, s0, k0) ->
        (match bool_val v ty m with
         | Some b ->
           if b
           then ret
                  ('s'::('t'::('e'::('p'::('_'::('w'::('h'::('i'::('l'::('e'::('_'::('t'::('r'::('u'::('e'::[])))))))))))))))
                  (State (f, s0, (Kwhile2 (x, s0, k0)), e, m))
           else ret
                  ('s'::('t'::('e'::('p'::('_'::('w'::('h'::('i'::('l'::('e'::('_'::('f'::('a'::('l'::('s'::('e'::[]))))))))))))))))
                  (State (f, Sskip, k0, e, m))
         | None -> [])
      | Kdowhile2 (x, s0, k0) ->
        (match bool_val v ty m with
         | Some b ->
           if b
           then ret
                  ('s'::('t'::('e'::('p'::('_'::('d'::('o'::('w'::('h'::('i'::('l'::('e'::('_'::('t'::('r'::('u'::('e'::[])))))))))))))))))
                  (State (f, (Sdowhile (x, s0)), k0, e, m))
           else ret
                  ('s'::('t'::('e'::('p'::('_'::('d'::('o'::('w'::('h'::('i'::('l'::('e'::('_'::('f'::('a'::('l'::('s'::('e'::[]))))))))))))))))))
                  (State (f, Sskip, k0, e, m))
         | None -> [])
      | Kfor2 (a2, a3, s0, k0) ->
        (match bool_val v ty m with
         | Some b ->
           if b
           then ret
                  ('s'::('t'::('e'::('p'::('_'::('f'::('o'::('r'::('_'::('t'::('r'::('u'::('e'::[])))))))))))))
                  (State (f, s0, (Kfor3 (a2, a3, s0, k0)), e, m))
           else ret
                  ('s'::('t'::('e'::('p'::('_'::('f'::('o'::('r'::('_'::('f'::('a'::('l'::('s'::('e'::[]))))))))))))))
                  (State (f, Sskip, k0, e, m))
         | None -> [])
      | Kswitch1 (sl, k0) ->
        (match sem_switch_arg v ty with
         | Some n ->
           ret
             ('s'::('t'::('e'::('p'::('_'::('e'::('x'::('p'::('r'::('_'::('s'::('w'::('i'::('t'::('c'::('h'::[]))))))))))))))))
             (State (f, (seq_of_labeled_statement (select_switch n sl)),
             (Kswitch2 k0), e, m))
         | None -> [])
      | Kreturn k0 ->
        (match sem_cast v ty f.fn_return m with
         | Some v' ->
           (match Mem.free_list m (blocks_of_env ge e) with
            | Some m' ->
              ret
                ('s'::('t'::('e'::('p'::('_'::('r'::('e'::('t'::('u'::('r'::('n'::('_'::('2'::[])))))))))))))
                (Returnstate (v', (call_cont k0), m'))
            | None -> [])
         | None -> [])
      | _ -> [])
   | None ->
     map (expr_final_state f k e)
       (step_expr ge do_external_function do_inline_assembly e w RV a m))
| Callstate (fd, vargs, k, m) ->
  (match fd with
   | Internal f ->
     if list_norepet_dec ident_eq
          (app (var_names f.fn_params) (var_names f.fn_vars))
     then let (e, m1) =
            do_alloc_variables ge empty_env m (app f.fn_params f.fn_vars)
          in
          (match sem_bind_parameters ge w e m1 f.fn_params vargs with
           | Some m2 ->
             ret
               ('s'::('t'::('e'::('p'::('_'::('i'::('n'::('t'::('e'::('r'::('n'::('a'::('l'::('_'::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))
               (State (f, f.fn_body, k, e, m2))
           | None -> [])
     else []
   | External (ef, _, _, _) ->
     (match do_external ge do_external_function do_inline_assembly ef w vargs
              m with
      | Some p ->
        let (p0, m') = p in
        let (p1, v) = p0 in
        let (_, t0) = p1 in
        (TR
        (('s'::('t'::('e'::('p'::('_'::('e'::('x'::('t'::('e'::('r'::('n'::('a'::('l'::('_'::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::[])))))))))))))))))))))),
        t0, (Returnstate (v, k, m')))) :: []
      | None -> []))
| Returnstate (v, k0, m) ->
  (match k0 with
   | Kcall (f, e, c, ty, k) ->
     ret
       ('s'::('t'::('e'::('p'::('_'::('r'::('e'::('t'::('u'::('r'::('n'::('s'::('t'::('a'::('t'::('e'::[]))))))))))))))))
       (ExprState (f, (c (Eval (v, ty))), k, e, m))
   | _ -> [])
| Stuckstate -> []

(** val do_initial_state : Csyntax.program -> (genv * state) option **)

let do_initial_state p =
  let ge = globalenv p in
  (match Genv.init_mem (program_of_program p) with
   | Some m0 ->
     (match Genv.find_symbol ge.genv_genv p.prog_main with
      | Some b ->
        (match Genv.find_funct_ptr ge.genv_genv b with
         | Some f ->
           if type_eq (type_of_fundef f) (Tfunction (Tnil, type_int32s,
                cc_default))
           then Some (ge, (Callstate (f, [], Kstop, m0)))
           else None
         | None -> None)
      | None -> None)
   | None -> None)

(** val at_final_state : state -> Int.int option **)

let at_final_state = function
| Returnstate (res, k, _) ->
  (match res with
   | Vint r -> (match k with
                | Kstop -> Some r
                | _ -> None)
   | _ -> None)
| _ -> None
