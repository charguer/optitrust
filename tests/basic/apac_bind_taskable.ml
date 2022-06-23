open Optitrust
open Target 
open Ast
open Apac_core

let _ = Run.script_cpp (fun () -> 

  (* let tsk = Hashtbl.create 10 in 
  Hashtbl.add tsk "f" {arg_dep_var = "x"; arg_dep_typ = typ_int();arg_dep_kind = Dep_kind_in;  }; *)
  let tsk = Apac_basic.identify_taskable_functions [] in
  !! Apac_basic.bind_taskable_calls tsk [cFunDef "test"];



)
