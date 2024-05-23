open Optitrust
open Prelude

let _ = Flags.use_member_functions()

(**
let _ = Flags.bypass_cfeatures := true
*)
let _ = Run.script_cpp (fun () ->
  (* !! Show.trm_text (Trace.ast()); **)
  (*!!Show.ast ~var_id:true ();*)
  !! Trace.apply Scope.unique_alpha_rename;
)

(* minimal counter example when ignore_serialized := false
class CC {
public:
    int * i;
    void f() {
        *i = 1;
    }
};
*)
