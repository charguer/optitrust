open Optitrust
open Prelude

let _ = Flags.check_validity := true (* FIXME: false *)
let _ = Flags.use_resources_with_models := true
let _ = Flags.preserve_specs_only := true
let _ = Flags.pretty_matrix_notation := false
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true
let _ = Flags.save_ast_for_steps := Some Flags.Steps_all (*Steps_important*)
(* let _ = Flags.save_ast_for_steps := Some Steps_all *)

let int = trm_int

let part = 3 (* Choose which part you want to work on. *)

(* Part 0: *)
let _ = if part = 1 then Run.script_cpp (fun () ->
  (* !! Function.elim_infix_ops ~indepth:true []; *)
  !! Loop.tile (int 32) ~index:"bi" ~bound:TileDivides [cFor "i"];

  (* LATER: !! Variable.local_name ~var:"s" ~local_var:"t" [tSpanSeq [cForBody "bi"]]; *)
  !! Sequence.intro ~mark:"t_scope" ~start:[tFirst; cForBody "bi"] ~stop:[tLast; cForBody "bi"] ();
  !! Variable.local_name ~var:"s" ~local_var:"t" [cMark "t_scope"];
  !! Sequence.elim [cMark "t_scope"];

  (* DEPRECATED? !! Sequence_basic.insert (trm_let (new_var "d", typ_f32) (trm_get (trm_find_var "s" []))) [tFirst; cForBody "bi"]; *)
  !! Variable.insert ~name:"d" ~typ:typ_f32 ~value:(trm_get (trm_find_var "s" [])) [cForBody "bi"; tFirst];

  (* at this line, the output is the equivalent of vectvectmul0_gen.cpp,
    beware that "==" needs to be replaced with "=." and all the "__is_true" must be removed;
    some +. and + need to be fixed
    ----> LATER: tweak display so that the output of _after.cpp is exactly  vectvectmul0.cpp *)
)

(* Part 2: *)
let _ = if part = 2 then Run.script_cpp ~filename:"vv1.cpp" (fun () ->
  (* Why nbMulti? !! Accesses.shift_var ~inv:true ~factor:(trm_find_var "d" []) [nbMulti; cVarDef "t"]; *)
  !! Accesses.shift_var ~inv:true ~factor:(trm_find_var "d" []) [cFor "bi"; cVarDef "t"];
  !! Variable.inline [cVarDef "d"];
  (* FIXME: !! Arith.simpl_surrounding_expr Arith.gather_rec [nbMulti; cVar "s"]; *)
)

(* Part 3: *)
let _ = if part = 3 then Run.script_cpp ~filename:"vv2.cpp" (fun () ->
  !! Resources.loop_minimize [cFor "i"];

  !! Loop.hoist [cVarDef "t"];
  !! Resources.ensure_computed ();
  !! Loop.fission [tBefore; cFor "bi"; cWriteVar "s"];
  (* TODO: need to fix contracts for the loop fission *)
  !! Loop.parallel [cFor "bi" ~body:[cFor "i"]];
  !! Cleanup.std();
  (* includes: !! Function.use_infix_ops ~indepth:true []; *)
)


(*============================HIGH LEVEL VIEW OF THE SCRIPT ==============

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
