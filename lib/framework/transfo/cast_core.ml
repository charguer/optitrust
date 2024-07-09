open Prelude

(** [insert_on ty t]: adds a type in front of [t] to cast its current type to [ty],
      [ty] - the type on which [t] is going to be casted to,
      [t] - any ast node that allows casting. *)
let insert_on (ty : typ) (t : trm) : trm =
  trm_cast ty t
