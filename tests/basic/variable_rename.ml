open Optitrust
open Target

(* TODO: this file and the transfo should be named variable_renames,
   to suggest "variable_rename" multiple time.
   The operation variable_rename should be implemented as a derived transformation,
   with argument ByList [("x","y")] to rename just one variable.

   In variable_rename.ml, the demo should be:
      Variable_basic.rename "x" ~into:"y" [cFunDef "main"]
*)

let _ = Run.doc_script_cpp (fun _ ->
    !! Variable_basic.rename_on_block (ByList [("x","y")]) [cFunDef "main"; dFunBody];
  )
"
int main() {
  int a = 3;
  int x = a;
  int r = x;
}
"

(* LATER: why does [dBody] not work in place of [dFunBody]? *)

let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.(rename_on_block (AddSuffix "2")) [cTopFunDef "main"; dFunBody];
  !! Variable_basic.rename_on_block (ByList [("y","y1");("z","z1")]) [cFunDef "f"; dFunBody];
  !! Variable_basic.(rename_on_block (AddSuffix "2")) [cTopFunDef "main"; dFunBody];
)



(* LATER:
   Also useful could be the renamining with the target pointing at the variable definition
      Variable_basic.rename_def "y" [cVarDef "x"];

   I think "rename_def" should be the one implemented as core transformation.
   Indeed, the "rename x ~into:y tg" operation can be implemented as follows:
     apply_on_target tg (fun p_ctx ->
        let p = resolve_target_one [target_of_path p_ctx; cVarDef x] in
        rename_def y (target_of_path p))

*)
