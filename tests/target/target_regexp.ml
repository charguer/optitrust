(* This file is for testing specifically the [rexp] matching *)

open Optitrust

let regexp_true = true
let regexp_false = false

let substr_true = true
let substr_false = false

(* let string_to_rexp (regexp : bool) (substr : bool) (s : string) (trmKind : trm_kind) : rexp *)
let rexp0 = Target.string_to_rexp regexp_false substr_false "t" Constr.TrmKind_Any
let rexp1 = Target.string_to_rexp regexp_false substr_true "t" Constr.TrmKind_Any
let rexp2 = Target.string_to_rexp regexp_true substr_false "t" Constr.TrmKind_Any
let rexp3 = Target.string_to_rexp regexp_true substr_true "t" Constr.TrmKind_Any


(* LATER: ARTHUR: better factorize the code.
2		-	let rexp0 = Target.string_to_rexp regexp_false substr_false "ab." Constr.TrmKind_Any
13		-	let rexp1 = Target.string_to_rexp regexp_false substr_true "ab." Constr.TrmKind_Any
14		-	let rexp2 = Target.string_to_rexp regexp_true substr_false "ab." Constr.TrmKind_Any
15		-	let rexp3 = Target.string_to_rexp regexp_true substr_true "ab." Constr.TrmKind_Any
*)

let rexps = [rexp0; rexp1; rexp2; rexp3]

(* let str0 = "ab"
let str1 = "ab."
let str2 = "xab.y"
let str3 = "abc"
let str4 = "xabcy" *)

let str0 = "t"
let str1 = "t1"
let str2 = "ut"
let str3 = "time"
let str4 = "tim1"

let strs = [str0; str1; str2; str3; str4]

let _ =
  List.iteri (fun i r ->
    List.iteri (fun j s ->
      let b = Constr.match_regexp_str r s in
      Printf.printf "rexp%d(%s)\tstr%d(%s)\t-> %s\n" i (Constr.rexp_to_string r) j s (if b then "true" else "false");
     ) strs) rexps

