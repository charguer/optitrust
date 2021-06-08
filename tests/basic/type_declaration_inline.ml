open Optitrust
open Run
(* Not the expected result :
   - missing a "const" in the printing of type double;
   - mat3d typedef seems to vanish, why? *)

(* TODO: Doesn't work *)

let _ = run_unit_test( fun _ ->
        Declaration.inline_typedef [cTypDef "uint"];
        Declaration.inline_typedef [cTypDef "cdouble"];
        Declaration.inline [cTypDef "mat3d"];
    )