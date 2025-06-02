open Optitrust
open Prelude

let _ = Flags.check_validity := false

let _ =
  Run.script_cpp (fun _ ->
      Rewrite.equiv_at
        "int i;int j; int k ==> (i + j <= k && k < i + j + 1 )  == (j == k-i)"
        [ cIf () ])
