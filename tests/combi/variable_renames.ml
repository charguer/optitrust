open Optitrust
open Target

(* let _ = Run.doc_script_cpp (fun _ ->
    !! Variable_basic.renames(ByList [("x","y")]) [cFunDef "main"; dBody];
  )
"
int main() {
  int a = 3;
  int x = a;
  int r = x;
}
" *)

(* LATER: why does [dBody] not work in place of [dBody]? *)

let _ = Run.script_cpp (fun _ ->

  !! Variable.(renames(AddSuffix "2")) [cTopFunDef "main"; dBody];
  !! Variable.renames(ByList [("y","y1");("z","z1")]) [cFunDef "f"; dBody];
  !! Variable.(renames(AddSuffix "2")) [cTopFunDef "main"; dBody];
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
