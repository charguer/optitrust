(**

This file describes tiling transformation relations as functions.

Author: Ramon Fernandez I Mir and Arthur Charguéraud.

License: MIT.

*)

Set Implicit Arguments.
Require Export Semantics LibSet LibMap TLCbuffer TrTiling.


(* ---------------------------------------------------------------------- *)
(** Explicit definitions *)

Fixpoint fun_tr_accesses (tt:tiling_tr) (π:accesses) : accesses :=
  let aux := fun_tr_accesses tt in
  match π with
  | nil => nil
  | ((access_array T i)::π') => 
      if isTrue(T = typ_var (tiling_tr_array_name tt)) then
        let K := tiling_tr_tile_size tt in
        let Tt := tiling_tr_tile_name tt in
        ((access_array T (i/K))::(access_array (typ_var Tt) (i mod K))::(aux π'))
      else
        ((access_array T i)::(aux π'))
  | ((access_field T f)::π') => ((access_field T f)::(aux π'))
  end.

Fixpoint fun_tr_val_depth (depth:nat) (tt:tiling_tr) (v:val) : val :=
  match depth with
    | O => v
    | S n =>
      let aux := fun_tr_val_depth n tt in
      match v with
      | val_error => val_error
      | val_uninitialized => val_uninitialized
      | val_unit => val_unit
      | val_bool b => val_bool b
      | val_int i => val_int i
      | val_double d => val_double d
      | val_abstract_ptr l π => val_abstract_ptr l (fun_tr_accesses tt π)
      | val_concrete_ptr l o => val_concrete_ptr l o (* TODO: This is not correct. *)
      | val_array T vs => 
          if isTrue(T = typ_var (tiling_tr_array_name tt)) then
            val_array T vs (* TODO: Critical case *)
          else
            (val_array T (LibList.map aux vs)) 
      | val_struct T s =>
          let m : monoid_op (map field val) := monoid_make (fun a b => a \u b) \{} in
          let g : field -> val -> map field val := fun f v => (\{})[f:=(aux v)] in
          val_struct T (fold m g s)
      | val_words lw => val_words lw (* TODO: Not correct either. *)
      end
   end.
