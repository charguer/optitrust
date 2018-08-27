(**

This file describes transformations of the layout of records and arrays.

Author: Ramon Fernandez I Mir and Arthur Charguéraud.

License: MIT.

*)

Set Implicit Arguments.
Require Export Semantics LibSet LibMap TLCbuffer.

(* ********************************************************************** *)
(* * Definition of the transformation *)

(** This is a special kind of transformation. We need to define new 
    new semantics. Essentially get and set for the concrete pointer.
    It can be included in the general semantics and just check that no
    concrete pointers are used in the other transformations. *)


(* ********************************************************************** *)
(* Contex holding low-level information about structs and their fields. *)

Definition typdefctx_sizes := map typvar size.
Definition typdefctx_fields_offsets := map typvar (map field offset).
Definition typdefctx_fields_order := map typvar (list field).

Record low_level_ctx := make_low_level_ctx {
  sizes : typdefctx_sizes;
  fields_offsets : typdefctx_fields_offsets;
  fields_order : typdefctx_fields_order }.

(* Ensures that the low-level context is correctly defined with respect to
   the type definitions context. *)

Inductive typdefctx_low_level (C:typdefctx) (LLC:low_level_ctx) : Prop :=
  typdefctx_low_level_intros : forall CS CFOff CFOrd,
    LLC = make_low_level_ctx CS CFOff CFOrd ->
    dom C = dom CS ->
    dom C = dom CFOff ->
    dom C = dom CFOrd ->
    (forall Tv Tfs,
      Tv \indom C ->
      C[Tv] = typ_struct Tfs ->
          dom Tfs = dom CFOff[Tv]
      /\  dom Tfs = to_set CFOrd[Tv]) -> 
    typdefctx_low_level C LLC.


(* ********************************************************************** *)
(* Computing the size of a type. Assuming the size of type variables
   are known. Used to transform array accesses. *)

Inductive typ_size (CS:typdefctx_sizes) : typ -> size -> Prop :=
  | typ_size_unit :
      typ_size CS (typ_unit) 1
  | typ_size_int :
      typ_size CS (typ_int) 1
  | typ_size_double :
      typ_size CS (typ_double) 2
  | typ_size_bool :
      typ_size CS (typ_bool) 1
  | typ_size_ptr : forall T',
      typ_size CS (typ_ptr T') 1
  | typ_size_array : forall n T' k,
      typ_size CS T' n ->
      typ_size CS (typ_array T' (Some k)) (n*k)
  | typ_size_struct : forall s n (m:monoid_op int) (g:field->size->size),
      dom s = dom n ->
      (forall f,
        f \indom s ->
        typ_size CS s[f] n[f]) ->
      m = monoid_make (fun a b => a + b) 0 ->
      g = (fun k v => v) ->
      typ_size CS (typ_struct s) (fold m g n)
  | typ_size_var : forall Tv,
      Tv \indom CS ->
      typ_size CS (typ_var Tv) CS[Tv].

(* Coherency between the offsets and the sizes. TODO: Find a better way. *)

Axiom special_map : list size -> map field offset.

Inductive low_level_ctx_ok (C:typdefctx) (LLC:low_level_ctx) : Prop :=
  | low_level_ctx_ok_intros : forall CS CFOrd CFOff,
      LLC = make_low_level_ctx CS CFOff CFOrd ->
      (forall Tv Tfs,
        Tv \indom C ->
        C[Tv] = typ_struct Tfs ->
        (exists CFT CFS,
            CFT = List.map (fun f => Tfs[f]) CFOrd[Tv]
        /\  List.Forall2 (typ_size CS) CFT CFS
        /\  CS[Tv] = fold_right Z.add 0 CFS
        /\  CFOff[Tv] = special_map CFS)) ->
      low_level_ctx_ok C LLC.


(* ********************************************************************** *)
(* Given a list of accesses, computes the offset. Used to translate
   pointer values. *)

Inductive accesses_offset (C:typdefctx) (LLC:low_level_ctx) : accesses -> offset -> Prop :=
  | accesses_offset_nil :
      accesses_offset C LLC nil 0
  | accesses_offset_access_array : forall T T' os πs i n o,
      typing_array C T T' os ->
      typ_size (sizes LLC) T' n ->
      accesses_offset C LLC πs o ->
      accesses_offset C LLC ((access_array T i)::πs) ((i * n) + o)
  | accesses_offset_access_field : forall FO πs Tv f o,
      FO = fields_offsets LLC ->
      Tv \indom FO ->
      f \indom FO[Tv] ->
      accesses_offset C LLC πs o ->
      accesses_offset C LLC ((access_field (typ_var Tv) f)::πs) (FO[Tv][f] + o).


(* ********************************************************************** *)
(* Relates values with a list of words. *)

Inductive val_to_words (C:typdefctx) (LLC:low_level_ctx) : val -> list word -> Prop :=
  | val_to_words_unit :
      val_to_words C LLC (val_basic val_unit) (0%Z::nil)
  | val_to_words_bool : forall b,
      val_to_words C LLC (val_basic (val_bool b)) ((if b then 1 else 0)%Z::nil)
  | val_to_words_int : forall i,
      val_to_words C LLC (val_basic (val_int i)) (i::nil)
  | val_to_words_double : forall d,
      val_to_words C LLC (val_basic (val_double d)) (d::d::nil)
  | val_to_words_abstract_ptr : forall π l o,
      accesses_offset C LLC π o ->
      val_to_words C LLC (val_basic (val_abstract_ptr l π)) (l::o::nil)
  | val_to_words_array : forall T a a',
      List.Forall2 (val_to_words C LLC) a a' ->
      val_to_words C LLC (val_array T a) (List.concat a')
  | val_to_words_struct : forall FCOrd Tv s s',
      FCOrd = fields_order LLC ->
      Tv \indom FCOrd ->
      List.Forall2 (val_to_words C LLC) (List.map (fun f => s[f]) FCOrd[Tv]) s' ->
      val_to_words C LLC (val_struct (typ_var Tv) s) (List.concat s').


(* ********************************************************************** *)
(* Transformation of a term from high-level to low-level. *)

(*
Inductive prim : Type :=
  | prim_binop : binop -> prim
  | prim_get : typ -> prim
  | prim_set : typ -> prim
  | prim_new : typ -> prim
  | prim_new_array : typ -> prim
  | prim_struct_access : typ -> field -> prim
  | prim_array_access : typ -> prim
  | prim_struct_get : typ -> field -> prim
  | prim_array_get : typ -> prim.

Inductive trm : Type :=
  | trm_var : var -> trm
  | trm_val : val -> trm
  | trm_if : trm -> trm -> trm -> trm
  | trm_let : bind -> trm -> trm -> trm
  | trm_app : prim -> list trm -> trm
  | trm_while : trm -> trm -> trm
  | trm_for : var -> val -> val -> trm -> trm.

*)

Inductive tr_trm (C:typdefctx) (LLC:low_level_ctx) : trm -> trm -> Prop :=
  | tr_trm_val : forall v lw,
      val_to_words C LLC v lw ->
      tr_trm C LLC (trm_val v) (trm_val (val_words lw)).

tr_state
tr_stack

Theorem red_tr : forall lw
  red C S m1 t m2 v ->
  tr_trm t t' ->
  tr_state m1 m1' ->
  tr_stack S S' ->
  exists m2' lw,
        val_to_words TC FC v lw
    /\  red C S' m1' t' m2' (val_words lw).
