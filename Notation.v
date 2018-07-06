
(* TODO: require export Semantics *)

(* ********************************************************************** *)
(* * Notation for terms *)

(* ---------------------------------------------------------------------- *)
(** Notation for concrete programs *)

Module NotationForTerms.

(** Note: below, many occurences of [x] have type [bind], and not [var] *)

Notation "'()" := val_unit : trm_scope.

Notation "'If_' t0 'Then' t1 'Else' t2" :=
  (trm_if t0 t1 t2)
  (at level 69, t0 at level 0) : trm_scope.

Notation "'If_' t0 'Then' t1 'End'" :=
  (trm_if t0 t1 val_unit)
  (at level 69, t0 at level 0) : trm_scope.

Notation "'Let' x ':=' t1 'in' t2" :=
  (trm_let x t1 t2)
  (at level 69, x at level 0, right associativity,
  format "'[v' '[' 'Let'  x  ':='  t1  'in' ']'  '/'  '[' t2 ']' ']'") : trm_scope.

Notation "t1 ;;; t2" :=
  (trm_seq t1 t2)
  (at level 68, right associativity, only parsing,
   format "'[v' '[' t1 ']'  ;;;  '/'  '[' t2 ']' ']'") : trm_scope.

End NotationForTerms.