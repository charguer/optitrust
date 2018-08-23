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

Definition fields_offset := map typvar (map field offset).
Definition fields_order := map typvar (list field).

Definition word := int.

Inductive accesses_offset (C:typdefctx) (FO:fields_offset) : accesses -> offset -> Prop :=
  | accesses_offset_nil :
      accesses_offset C FO nil 0
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

Inductive val_words (C:typdefctx) (FO:fields_offset) (FO':fields_order) : val -> list word -> Prop :=
  | val_words_unit :
      val_words C FO FO' (val_basic val_unit) (0%Z::nil)
  | val_words_bool : forall b,
      val_words C FO FO' (val_basic (val_bool b)) ((if b then 1%Z else 0%Z)::nil)
  | val_words_int : forall i,
      val_words C FO FO' (val_basic (val_int i)) (i::nil)
  | val_words_double : forall d,
      val_words C FO FO' (val_basic (val_double d)) (d::d::nil)
  | val_words_abstract_ptr : forall π l o,
      accesses_offset C FO π o ->
      val_words C FO FO' (val_basic (val_abstract_ptr l π)) (l::o::nil)
  | val_words_array : forall T a a',
      List.Forall2 (val_words C FO FO') a a' ->
      val_words C FO FO' (val_array T a) (List.concat a')
  | val_words_struct : forall Tv s s',
      Tv \indom FO' ->
      List.Forall2 (val_words C FO FO') (List.map (fun f => s[f]) FO'[Tv]) s' ->
      val_words C FO FO' (val_struct (typ_var Tv) s) (List.concat s').


Fixpoint typ_size' (C:typdefctx) (T:typ) {struct T} : size :=
  let aux := typ_size' C in
  match T with
  | typ_unit => 1
  | typ_int => 1
  | typ_double => 2
  | typ_bool => 1
  | typ_ptr T' => 1
  | typ_array T' (Some k) => k * typ_size C T'
  | typ_array T' None => 0
  | typ_struct s =>
      (*let m : monoid_op size := monoid_make (fun a b => a + b) 0 in
      let g : field -> typ -> size := fun f T' => aux T' in
      fold m g s*)
      fold (monoid_make (fun a b => a + b) 0) (fun f T' => aux T') s
  | typ_fun Ts Tr => 0
  | typ_var Tv => 1 (*typ_size C C[Tv]*)
  end.