open Optitrust
open Prelude

let _ = Flags.check_validity := false
let _ = Flags.preserve_specs_only := true
let _ = Flags.pretty_matrix_notation := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true
let _ = Flags.save_ast_for_steps := Some Flags.Steps_important
(* let _ = Flags.save_ast_for_steps := Some Steps_all *)

let int = trm_int

let _ = Run.script_cpp (fun () ->
  !! Loop.tile (int 32) ~index:"bi" ~bound:TileDivides [cFor "i"];
  !! Variable.local_name ~var:"s" ~local_var:"t" [cFor "i"];
  !! Loop.hoist [cVarDef "t"];
  !! Marks.add "w0" [cWrite ~rhs:[cVar "s"] ()];
  !! Function.elim_infix_ops ~indepth:true [];
  Trace.without_resource_computation_between_steps (fun () ->
    (* !! Show.add_marks_for_target_unit_tests [cFor "bi"; dBody; tFirst]; *)
     !! Accesses.shift_var
      ~array_base:(trm_find_var "t" [])
       ~inv:true
       ~factor:(trm_get (trm_find_var "s" [])) [cMark "w0"];
    (* works but does not produce the intended code
       let address_pattern = Trm.(array_access (trm_find_var "t" []) (pattern_var "i")) in
      !! Accesses.shift ~address_pattern ~inv:true ~factor:(trm_get (trm_find_var "s" [])) [cFor "i"]; *)
    (* needs to simplify the successive write to get the desired form *)

    (* !! Accesses.shift_var ~inv:true ~factor:(trm_get (trm_find_var "s" [])) [cVarDef "t"];
       ==> ideally would work for arrays *)
    !! Arith.(simpl_rec gather_rec) [];
    !! Function.use_infix_ops ~indepth:true [];
    !! Flags.recompute_resources_between_steps := false;
  )
);

(*
> J'arrive maintenant au stade :
>
> s = 0
> for b
>   t[bi] = s
>   for i in block bi
>     t[bi] = t[bi] + a[i] * b[i]
>   s = t[bi]
>
> je lance le shift mais il ne fait pas exactement ce que je veux.
>
> s = 0
> for b
>   t[bi] = s
>   t[bi] = t[bi] - s
>         // alors que je voulais grouper les deux instructions ci-dessus en t = s - s
>   for i in block bi
>      t[bi] = ((t[bi] + s) + a[i]*b[i]) - s   // là je voudrais faire un arith simpl
>   t[bi] = t[bi] + s
>   s = t[bi]
>         // alors que je voulais grouper les deux instructions ci-dessus en s = s + t[bi]
>
> Sur ce code là, qui ne type pas en raison du changement des modèles,
> on peut donc se poser la question de ce qu'il faudrait faire pour que la boucle type.
> (il faut faire le shift sur le contrat, je pense que ça suffit peut être).

Je suis pas sur de ce que tu appelle shift sur le contrat, mais si ça revient transformer la ressource :
&t[MINDEX1(exact_div(n, 32), bi)] ~~> reduce_sum(bi * 32 + i, fun -> A(j) * B(j))
du contrat en
&t[MINDEX1(exact_div(n, 32), bi)] ~~> reduce_sum(bi * 32 + i, fun j -> A(j) * B(j)) - reduce_sum(bi * 32 + 0, fun j -> A(j) * B(j))
, ça ne suffira pas.

Il y aura de toute façon le problème classique de réécriture arithmétique.
Pour l'expression t[bi] + s tu obtiendra
res ~~> reduce_sum(bi * 32 + i, fun j -> A(j) * B(j)) - reduce_sum(bi * 32 + 0, fun j -> A(j) * B(j)) + reduce_sum(bi * 32 + 0, fun j -> A(j) * B(j))
qui ne se simplifie pas tout seul en res ~~> reduce_sum(bi * 32 + i, fun j -> A(j) * B(j)) par exemple.

Si on insère bien la ghost de cette simplification, il faut aussi penser à adapter les arguments inside des deux ghosts qui suivent:
   __ghost(rewrite_float_linear, "inside := fun v -> &t[MINDEX1(exact_div(n, 32), bi)] ~~> v, by := reduce_sum_add_right(bi * 32 + i, fun j -> A(j) * B(j), i_gt_0)");
   __ghost(rewrite_linear, "inside := fun (i: int) -> &t[MINDEX1(exact_div(n, 32), bi)] ~~> reduce_sum(i, fun j -> A(j) * B(j)), by := add_assoc_right(bi * 32, i, 1)");

Pour ajouter le ... - reduce_sum(bi * 32 + 0, fun j -> A(j) * B(j)) manquant.

> En vrai, je voudrais en faire faire un shift_var sur toutes les cases du tableau "t", mais le code ne gère pas ça. Je ne sais pas si ça serait plus simple ou plus compliqué de passer par le code ci-dessus, plutôt que d'aller directement au code idéal avec les instructions regroupées. Un avis ?
*)


  (*  (*  *)
   *)
  (* !! Variable.shift *)
  (* !! Openmp.parallel *)


(*



s = 0
for i
  s += a[i] * b[i]

--- tile

s = 0
for b
  for i in block b
    s += a[i] * b[i]

--- local name

s = 0
for b
  t = s
  // ghost(hide_cell, consume s~>v, produces "hidden(s,t)")
  for i in block b
    t += a[i] * b[i]
  s = t
  // ghost(hide_rev_cell, consume hidden(s,t), consume(t->w), produces "s~>w")


--- shift de t par s

s = 0
for b
  t = s
  for i in block b
    t += a[i] * b[i] // pareil que t = t + a[i] * b[i]
  s = t

--- shift de t par s

s = 0
for b
  t = 0  // en fait s-s
  for i in block b
    t += a[i] * b[i]  // en fait t = ((t + s) + a[i]*b[i]) - s
  s += t  // en fait s = t+s

--- hoist de t

s = 0
alloc t
for b
  t[b] = 0
  for i in block b
    t[b] += a[i] * b[i]
  s += t[b]

--- fission

s = 0
for b
  t[b] = 0
  for i in block b
    t[b] += a[i] * b[i]
for b
  s += t[b]

*)
