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

(*
Inductive typ : Type :=
  | typ_unit : typ
  | typ_int : typ
  | typ_double : typ
  | typ_bool : typ
  | typ_ptr : typ -> typ
  | typ_array : typ -> option size -> typ
  | typ_struct : map field typ -> typ
  | typ_fun : list typ -> typ -> typ
  | typ_var : typvar -> typ.
*)

(*
Inductive basic_val : Type :=
  | val_error : basic_val
  | val_unit : basic_val
  | val_uninitialized : basic_val
  | val_bool : bool -> basic_val
  | val_int : int -> basic_val
  | val_double : int -> basic_val
  | val_abstract_ptr : loc -> accesses -> basic_val
  | val_concrete_ptr : loc -> offset -> basic_val.

Inductive val : Type :=
  | val_basic : basic_val -> val
  | val_array : typ -> list val -> val
  | val_struct : typ -> map field val -> val.
*)


(*
Inductive access : Type :=
  | access_array : typ -> int -> access
  | access_field : typ -> field -> access.
*)

(* Trying out some stuff *)


(* Contex holding information about structs and their fields. *)

Definition typdefctx_sizes := map typvar size.
Definition typdefctx_fields_offsets := map typvar (map field offset).
Definition typdefctx_fields_order := map typvar (list field).

Record low_level_ctx := make_fields_ctx {
  sizes : typdefctx_sizes;
  fields_offsets : typdefctx_fields_offsets;
  fields_order : typdefctx_fields_order }.



Definition typdefctx_sizes := map typvar size.

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

Definition word := int.

Inductive accesses_offset (CS:typdefctx_sizes) (FO:fields_offset) : accesses -> offset -> Prop :=
  | accesses_offset_nil :
      accesses_offset CS FO nil 0
  | accesses_offset_access_array : forall T T' os πs i n o,
      typing_array C T T' os ->
      typ_size C T' n ->
      accesses_offset C FO πs o ->
      accesses_offset C FO ((access_array T i)::πs) ((i * n) + o)
  | accesses_offset_access_field : forall πs Tv f o,
      Tv \indom FO ->
      f \indom FO[Tv] ->
      accesses_offset C FO πs o ->
      accesses_offset C FO ((access_field (typ_var Tv) f)::πs) (FO[Tv][f] + o).

Inductive val_to_words (C:typdefctx) (FC:fields_ctx) : 
                    val -> list word -> Prop :=
  | val_to_words_unit :
      val_to_words C FC (val_basic val_unit) (0%Z::nil)
  | val_to_words_bool : forall b,
      val_to_words C FC (val_basic (val_bool b)) ((if b then 1 else 0)%Z::nil)
  | val_to_words_int : forall i,
      val_to_words C FC (val_basic (val_int i)) (i::nil)
  | val_to_words_double : forall d,
      val_to_words C FC (val_basic (val_double d)) (d::d::nil)
  | val_to_words_abstract_ptr : forall FCOff π l o,
      FCOff = fields_ctx_offset FC ->
      accesses_offset C FCOff π o ->
      val_to_words C FC (val_basic (val_abstract_ptr l π)) (l::o::nil)
  | val_to_words_array : forall T a a',
      List.Forall2 (val_words C FC) a a' ->
      val_to_words C FC (val_array T a) (List.concat a')
  | val_to_words_struct : forall FCOrd Tv s s',
      FCOrd = fields_ctx_order FC ->
      Tv \indom FCOrd ->
      List.Forall2 (val_words C FC) (List.map (fun f => s[f]) FCOrd[Tv]) s' ->
      val_to_words C FC (val_struct (typ_var Tv) s) (List.concat s').

(* Some definitions *)




Theorem red_tr :
  red C S m1 t m2 v ->
  tr_trm t t' ->
  exists lw,
        val_words TC FC v lw
    /\  red C S m1 t' m2 (val_words lw)
