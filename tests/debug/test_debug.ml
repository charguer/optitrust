open Optitrust
open Prelude

(*
let _ = Flags.dump_ast_details := true
let _ = Flags.debug_stringreprs := true
*)

let _ = Run.script_cpp (fun () ->

  !! Sequence_basic.insert (trm_int 3) [cThen; tLast];


  let b = trm_cast (typ_ptr Ptr_kind_mut (typ_int())) (Matrix_trm.alloc [trm_get(var "x")] (expr "sizeof(int)")) in
  let t = trm_let_mut (Trm.new_var "x",Typ.typ_auto()) b in
  !! Sequence_basic.insert ~reparse:true t [tBefore; cReturn];


  let b = trm_cast (typ_ptr Ptr_kind_mut (typ_int())) (Matrix_trm.alloc [var "y"] (expr "sizeof(int)")) in
  let t = trm_let_mut (Trm.new_var "x",Typ.typ_auto()) b in
  !! Sequence_basic.insert ~reparse:true t [tBefore; cReturn];

(**
(* [trm_let_mut ~annot ?ctx typed_var init]: an extension of trm_let for
    creating mutable variable declarations *)
let trm_let_mut ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
  (typed_var : typed_var) (init : trm): trm =
  let var_name, var_type = typed_var in
  let var_type_ptr = typ_ptr_generated var_type in
  let t_let = trm_let ?loc ?ctx Var_mutable (var_name, var_type_ptr) (trm_apps (trm_prim (Prim_new (var_type, []))) [init]) in
  trm_add_cstyle Stackvar t_let

*)

  (* !! Sequence_basic.insert ~reparse:true (stmt "int b = 2;") [tBefore; cReturn]; *)





  (**
let fst_instr = trsi (trm_cast (local_var_type) (alloc ?init dims size )) in
*)
   !! ()
)
