open Optitrust
open Prelude

let _ = Flags.check_validity := true
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
  !! Function.elim_infix_ops ~indepth:true [];
  Trace.without_resource_computation_between_steps (fun () ->
    let address_pattern = Trm.(array_access (trm_find_var "t" []) (pattern_var "i")) in
      !! Accesses.shift ~address_pattern ~inv:true ~factor:(trm_get (trm_find_var "s" [])) [cFor "i"];
    (* needs to simplify the successive write to get the desired form *)

    (* !! Accesses.shift_var ~inv:true ~factor:(trm_get (trm_find_var "s" [])) [cVarDef "t"];
       ==> ideally would work for arrays *)
    !! Arith.(simpl_rec gather_rec) []; (* LATER: will simplify s-s into 0 *)
    !! Function.use_infix_ops ~indepth:true [];
    Flags.recompute_resources_between_steps := false;
  )
);
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
