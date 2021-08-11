open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Omp.parallel [] [tBefore;cSeq ~args_pred:(Target.target_list_one_st(sInstrRegexp ~substr:true ". \+= 1") ) ()];
  !! Omp.atomic (Some Update) [tBefore; tIndex ~nb:2 0;sInstrRegexp ~substr:true ". \+= ."];
  !! Omp.atomic (Some Update) [tBefore; tIndex ~nb:2 1;sInstrRegexp ~substr:true ". \+= ."];
)
