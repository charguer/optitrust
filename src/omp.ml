open Ast
open Target

include Omp_basic

(* [set_num_threads threadnum tg] sometimes omp_get_num_threads doesn't work with gcc for that we need to apply the following trick
     #pragma omp parallel
     {
       #pragma omp single
       nbThreas = omp_get_num_threads();
     }
 *)
let set_num_threads (threadnum : var) (tg : target) : unit =
  let mark = "threadnum_set_mark" in 
  let instr_to_insert = trm_set ~marks:[mark] (trm_var threadnum) (trm_omp_routine Get_num_threads) in 
  Sequence.insert instr_to_insert tg;
  Sequence.intro_on_instr [cMark mark];
  Omp_basic.parallel [tBefore;cSeq ~args:[[cMark mark]] ()];
  Omp_basic.single [tBefore; cMark mark];
  Marks_basic.clean [cMark mark]


(* [parallel_for ~clause ~collapse tg] when collapse is provided as argument then 
     clause will not be taken into account*)
let parallel_for ?(clause : clause list = []) ?(collapse : int = 0) : Target.Transfo.t = 
  if collapse <> 0 then 
      Omp_basic.parallel_for ~clause:[Collapse(3)]
  else 
      Omp_basic.parallel_for ~clause


(* [header ()] inserts omp.h header at top of the file *)
let header () : unit = 
  !! Sequence.insert (stmt "#include \"omp.h\"") [tFirst; dRoot];

