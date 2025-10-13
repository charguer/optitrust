open Optitrust
open Prelude

let _ = Flags.check_validity := false
let _ = Flags.preserve_specs_only := true
let _ = Flags.pretty_matrix_notation := false
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true
let _ = Flags.save_ast_for_steps := Some Flags.Steps_all (*Steps_important*)
(* let _ = Flags.save_ast_for_steps := Some Steps_all *)

let int = trm_int

let part = 2 (* Choose which part you want to work on. *)

(* Part 1: until need to fix the ghoston the shift_var *)
let _ = if part = 1 then Run.script_cpp (fun () ->
  !! Function.elim_infix_ops ~indepth:true [];
  !! Loop.tile (int 32) ~index:"bi" ~bound:TileDivides [cFor "i"];
  !! Variable.local_name ~var:"s" ~local_var:"t" [cFor "i"];
  !! Sequence_basic.insert (trm_let (new_var "d", typ_f32) (trm_get (trm_find_var "s" []))) [tFirst; cForBody "bi"];
  (* at this line, the output is the equivalent of vectvectmul0.cpp,
    beware that "==" needs to be replaced with "=." and all the "__is_true" must be removed;
    some +. and + need to be fixed
    ----> todo: tweak display so that the output of _after.cpp is exactly  vectvectmul0.cpp *)
  !! Accesses.shift_var ~factor:(trm_find_var "d" []) [nbMulti; cVarDef "t"];
)

(* "vectvectmul1.cpp" is obtained by applying shift_var by hand on  vectvectmul0.cpp *)
let _ = if part = 2 then Run.script_cpp ~filename:"vectvectmul1.cpp" (fun () ->
  !! ()
)



  (**
  !! Accesses.shift_var ~inv:true ~factor:(trm_get (trm_find_var "s" [])) [cFor "bi"; cVarDef "t"];
  !! Loop.hoist [cVarDef "t"];
  *)


  (**
  !! Marks.add "w0" [cWrite ~rhs:[cVar "s"] ()];
  !! Function.elim_infix_ops ~indepth:true [];
  Trace.without_resource_computation_between_steps (fun () ->
    (* !! Show.add_marks_for_target_unit_tests [cFor "bi"; dBody; tFirst]; *)
     !! Accesses.shift_var
      ~array_base:(trm_find_var "t" [])
       ~inv:true
       ~factor:(trm_get (trm_find_var "s" [])) [cMark "w0"];
       *)
    (* works but does not produce the intended code
       let address_pattern = Trm.(array_access (trm_find_var "t" []) (pattern_var "i")) in
      !! Accesses.shift ~address_pattern ~inv:true ~factor:(trm_get (trm_find_var "s" [])) [cFor "i"]; *)


  (*


  ) *)

  (*
  !! Function.elim_infix_ops ~indepth:true [];

    let address_pattern = Trm.(array_access (trm_find_var "t" []) (pattern_var "i")) in
      !! Accesses.shift ~address_pattern ~inv:true ~factor:(trm_get (trm_find_var "s" [])) [cFor "i"];

    (* !! Accesses.shift_var ~inv:true ~factor:(trm_get (trm_find_var "s" [])) [cVarDef "t"];
       ==> ideally would work for arrays *)
    !! Arith.(simpl_rec gather_rec) [];
    !! Function.use_infix_ops ~indepth:true [];

    !! Flags.recompute_resources_between_steps := false;
  )

    Flags.recompute_resources_between_steps := false;

  )*)



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
__ghost(0 = reduce(0,0,f) à réécrire dans s ~> 0 pour avoir s ~> reduce(0,0,..))
for i
  __invariant "s ~> reduce(0,i,...)"
  s += a[i] * b[i]
  __ghost(reduce(0,i,f) + f(i) = reduce(0,i+1,f) à réécrire dans le contenu de s)

--- unfold infix ops

s = 0
for i
  __invariant "s ~> reduce(0,i,...)"
  s = s + a[i] * b[i]

--- tile
const int NB_BLOCK = a.length/B // to insert! with check divisibility?
s = 0
for b = 0 to NB_BLOCK
  __invariant "s ~> reduce(0,b*B,...)"
  for i = b*B to (b+1)*B
    __invariant "s ~> reduce(0,i,...)"
    s = s + a[i] * b[i]
    __ghost(reduce(0,i,f) + f(i) = reduce(0,i+1,f) à réécrire dans le contenu de s)

--- local name

s = 0
for b
  __invariant "s ~> reduce(0,b*B,...)"
  t = s
  // ghost(hide_cell, consume s~>v, produces "hidden(s,t)")
  for i = b*B to (b+1)*B
    __invariant "t ~> reduce(0,i,...)"
    t = t + a[i] * b[i]
    __ghost(reduce(0,i,f) + f(i) = reduce(0,i+1,f) à réécrire dans le contenu de t)
  s = t
  // ghost(hide_rev_cell, consume hidden(s,t), consume(t->w), produces "s~>w")

--- insert def 'p' for the contents of 's'

s = 0
for b
  __invariant "s ~> reduce(0,b*B,...)"
  const int p = s;  // sum of the prefix upto the block b
  // in ctx, we have an alias: "p := reduce(0,b*B,...)"
  t = s
  for i = b*B to (b+1)*B
    __invariant "t ~> reduce(0,i,...)"
    t = t + a[i] * b[i]
    __ghost(reduce(0,i,f) + f(i) = reduce(0,i+1,f) à réécrire dans le contenu de t)
  s = t

--- shift_var t by p

s = 0
for b
  __invariant "s ~> reduce(0,b*B,...)"
  const int p = s
  // in ctx, we have an alias: "p := reduce(0,b*B,...)"
  t = s - p // -p on write into t
  for i = b*B to (b+1)*B
    __invariant "t ~> reduce(0,i,...) - p" // -p on models of t
    t = ((t + p) + a[i] * b[i]) - p   // +p on reads on t, -p on write into t
    // ICI problème: on avait
    // __ghost(reduce(0,i,f) + f(i) = reduce(0,i+1,f) à réécrire dans le contenu de t)
    // et maintenant il faut
    // __ghost(reduce(0,i,f) - p + p + f(i) - p = reduce(0,i+1,f) - p à réécrire dans le contenu de t)
    // ça je sais pas trop comment on va gérer... peut être sans typer les étapes intermédiaires ?
    // parce que après on retrouve un moment où la ghost d'origine fonctionne telle quelle
  s = t + p  // +p on reads on t


--- unfold def of 'p' in the code (not in formulae), using the fact that s is not modified in the scope

s = 0
for b
  __invariant "s ~> reduce(0,b*B,...)"
  const int p = s
  // in ctx, we have an alias: "p := reduce(0,b*B,...)"
  t = s - s
  // in ctx, we have t ~> reduce(0,b*B,...) - reduce(0,b*B,...)
  for i = b*B to (b+1)*B
    __invariant "t ~> reduce(0,i,...) - p"
    t = ((t + s) + a[i] * b[i]) - s
    // ... ici la ghost qui va bien
  s = t + s

-- remove s from the block-processing by arith_simpl

s = 0
for b
  __invariant "s ~> reduce(0,b*B,...)"
  const int p = s
  // in ctx, we have an alias: "p := reduce(0,b*B,...)"
  t = 0
  // HERE: need to insert a ghost rewriting "0" into "reduce(0,b*B,...) - reduce(0,b*B,...)" in contents of t
  for i = b*B to (b+1)*B
    __invariant "t ~> reduce(0,i,...) - p"
    t = t + a[i] * b[i]
    // la ghost d'accumulation fonctionne de nouveau !
    __ghost(reduce(0,i,f) + f(i) = reduce(0,i+1,f) à réécrire dans le contenu de t)
  s = t + s

--- optional step: turn "p" into ghost def;
--    alternative: inline "p", replacing it with "reduce(0,b*B,...)" in the formulae
--    if we do nothing, it's fine too, but we'll need to duplicate "const int p = s"
      when we do the fusion step later.

s = 0
for b
  __invariant "s ~> reduce(0,b*B,...)"
  // BEFORE:
  //   const int p = s  // where p is not used in the code, only in formulae
  //   in ctx, we have an alias: "p := reduce(0,b*B,...)"
  // AFTER:
  //   __DEF(p, "reduce(0,b*B,...)");
  ...

  ==> in fine, I think inlining 'p' into 'reduce(0,b*B,...)' in formulae would be easiest.


--- hoist de t

s = 0
alloc t as an array of NB_BLOCK
for b
  __xmodifies "t ~> UninitCell" // pas de spécification du modèle d'entrée ou de sortie
  __invariant "s ~> reduce(0,b*B,...)"
  t[b] = 0
  // ghost rewriting "0" into "reduce(0,b*B,...) - reduce(0,b*B,...)" in contents of t
  for i = b*B to (b+1)*B
    __invariant "t[b] ~> reduce(0,i,...) - reduce(0,b*B,...)" // MODIFIED CONTRACT: t becomes t[b]
    t[b] = t[b] + a[i] * b[i]
    __ghost(reduce(0,i,f) + f(i) = reduce(0,i+1,f) à réécrire dans le contenu de t)
  // NOTE: at this point, the ctx stores t[b] ~> reduce(0,i,...) - reduce(0,b*B,...)
  //   the value of t[b] determines the contract for the fission that comes next
  s = t[b] + s


--- fission + parallelization

s = 0
alloc t as an array of NB_BLOCK
parallel for b
  __xmodifies "t[b] ~> t[b] ~> reduce(0,i,...) - reduce(0,b*B,...)" // NEW CONTRACT!
  __DEF(p, "reduce(0,b*B,...)");
  t[b] = 0
  // ghost rewriting "0" into "reduce(0,b*B,...) - reduce(0,b*B,...)" in contents of t
  for i = b*B to (b+1)*B
    __invariant "t ~> reduce(0,i,...) - reduce(0,b*B,...)"
    t[b] = t[b] + a[i] * b[i]
    __ghost(reduce(0,i,f) + f(i) = reduce(0,i+1,f) à réécrire dans le contenu de t)
for b
  // MOVED: contract on 's' is just moved here
  __invariant "s ~> reduce(0,b*B,...)"
  s = t[b] + s


--- cleanup, with infix ops

s = 0
alloc t as an array of NB_BLOCK
parallel for b
  t[b] = 0
  for i = b*B to (b+1)*B
    t[b] += a[i] * b[i]
for b
  s += t[b]




=====================
# remark: it seems that we never need to exploit
   "reduce(0,i,f) - reduce(0,j,f) = reduce(j,i,f)"

*)
