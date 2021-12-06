open Optitrust
open Target
open Ast

(* TODO: make two variants of the function

    Function.use_infix_ops (tg:target)
        => this means use infix ops anywhere below tg
        => one can call "use_infix_ops [cRoot]" for targeting all the file

   Function_basic.use_infix_ops_at ~allow_identity:bool (tg:target)
        => this means use infix ops exactly at one specific function call,
           matching the target, not recursively below;
        => if allow_identity=false, this function should fail if no replacement could be done

    The operation "use_infix_ops" can be implemented at the combi level by calling
      apply_on_target tg (fun p ->
         use_infix_ops_at ~allow_identity:true [target_of_path p; cPrimPredFun is_infix_prim_fun])

    After these changes are done, need to update the unit test below
*)

let _ = Run.doc_script_cpp (fun _ ->
    !! Function_basic.use_infix_ops ();
  )
"
int main() {
  int x = 2;
  x = x + 1;
}
"

let _ = Run.script_cpp (fun _ ->

    !! Function_basic.use_infix_ops ();
)