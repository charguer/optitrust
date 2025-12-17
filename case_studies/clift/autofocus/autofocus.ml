open Optitrust
open Ast
open Trm
open Trm_unify
open Matrix_trm
open Typ
open Prelude

let _ = Flags.check_validity := true

let () =
  Run.script_cpp (fun _ ->
      (* run_tests build_focus_list; *)
      !!!()
  )


