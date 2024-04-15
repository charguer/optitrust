open Ast
open Apac_const

(** [const_records]: hash table of [const_fun] with an initial size of 10. The
    size of the table will grow automatically if needed. *)
let const_records : const_funs = Var_Hashtbl.create 10
