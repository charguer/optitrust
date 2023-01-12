open Optitrust
open Target


let _ = Run.script_cpp (fun () ->

  !! Flow_basic.insert_if ~cond:(expr "omp_get_num_threads() != 16") ~no_else_branch:true [cFun "abort"];
  !! Sequence_basic.intro 4 [cIf ()];
     let tg = cSeq ~args_pred:(target_list_one_st [cIf ()]) () in 
  !! Omp.set_dynamic 0 [tBefore; tg];
  !! Omp_basic.set_num_threads 16 [tBefore; tg];
  !! Omp.parallel ~clause:[Shared ["x"; "npoints"]; Private ["iam"; "ipoints"]] [cSeq ~args_pred:(target_list_one_st [cIf ()]) ()];

) 
