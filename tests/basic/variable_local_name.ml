open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
    !! Variable_basic.local_name ~var_type:(atyp "int") ~var:"x" ~local_var:"y" [cLabel "sec"];
  )
"
int main() {
  int x = 0;
sec:{
  x = x + 1;
  x = x + 2;
}
  int r = x;
}
"

(* TODO: rename the arguments to:
  !! Variable_basic.local_name "x" ~into:"y" ~typ:(atyp "int") [cLabel "sec"];
*)

(* LATER: in the combi-level version,
   the typ argument should be optional; in that case, the type of "x" is used for "y";
   (in fact isn't it always possible to reuse the type of the previous variable?)

!! Variable_basic.local_name "x" ~into:"y" [cLabel "sec"];
*)


let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.local_name ~mark:"mymark" ~var_type:(atyp "T") ~var:"a"  ~local_var:"x" [cFor "i"];
)
