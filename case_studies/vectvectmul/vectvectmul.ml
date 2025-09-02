open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.pretty_matrix_notation := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true
let _ = Flags.save_ast_for_steps := Some Flags.Steps_important

let int = trm_int

let _ = Run.script_cpp (fun () ->
  !! Loop.tile (int 32) ~index:"bi" ~bound:TileDivides [cFor "i"];
  !! Variable.local_name ~var:"s" ~local_var:"t" [cFor "bi"];
  !! Loop.hoist [cVarDef "t"];
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
  for i in block b
    t += a[i] * b[i]
  s = t

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
