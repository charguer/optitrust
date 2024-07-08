open Prelude
open Target

(** [fold ~at tg]: expects the target [tg] to point at a typedef, then it will fold its definition,
    [at] - denotes a target where folding is done. If empty
           the folding operation is performed on all the ast nodes in the same
           level as the declaration or deeper. *)
let fold ?(at : target = []) (tg : target) : unit =
  Target.apply_at_target_paths_in_seq (Typedef_core.fold_at at) tg

(** [unfold ~delete ~at tg]: expects the target [tg] to point at a typedef, then it inlines its definition,
    [delete] - denotes a flag for telling if the declaration should be kept or no,
    [at] - denotes a target where inlining is done,
           if empty the inlining operation is performed on all the ast nodes in the
           same level as the declaration or deeper, by default [at] = []. *)
let unfold ?(delete : bool = false) ?(at : target = []) (tg : target) : unit =
  Target.apply_at_target_paths_in_seq (Typedef_core.unfold_at delete at) tg

(** [insert_copy name tg]: expects the target [tg] to point at a typedef, then copies the content
      of the body of typedef at gives to it the name [name]. *)
let insert_copy (name : string) (tg : Target.target) : unit =
  (* FIXME: #advanced-scoping-check , deal with typedef names *)
  Nobrace_transfo.remove_after (fun _ -> Target.apply_at_target_paths (Typedef_core.insert_copy_of name) tg)

(** [insert name td_body]: expects target [tg] to point at a relative location inside a sequence
    then it will insert a typedef declaration on that location.
    [name] - is the new type name while
    [td_body] - is the kind of typedef we are going to declare.
                It can be an alias a product(for struct declarations), a sum type or an enum. *)
let insert (name : string) (td_body : typedef_body) : target -> unit =
  Target.apply_at_target_paths_before (fun t i -> Typedef_core.insert_at name td_body i t)
