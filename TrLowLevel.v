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