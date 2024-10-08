open Prelude
open Target

include Omp_basic

let omp_header = "#include \"omp.h\""

(** [ensure_header ()]: insert omp.h header at top of the file *)
let ensure_header () : unit =
  (* Sequence.insert (stmt omp_header) [tFirst; dRoot]; *)
  Trace.ensure_header omp_header

(** [set_num_threads threadnum tg]: sometimes omp_get_num_threads doesn't work with gcc for that we need to apply the following trick
     #pragma omp parallel
     {
       #pragma omp single
       nbThreas = omp_get_num_threads();
     } *)
let set_num_threads (threadnum : var) (tg : target) : unit =
  let mark = "threadnum_set_mark" in
  let instr_to_insert = trm_add_mark mark (trm_set (trm_var threadnum) (trm_omp_routine Get_num_threads)) in
  Sequence.insert instr_to_insert tg;
  Sequence.intro_on_instr [cMark mark];
  Omp_basic.parallel [cSeq ~instrs:[[cMark mark]] ()];
  Omp_basic.single [cMark mark];
  Marks_basic.clean [cMark mark]

(** [parallel_for ~clause ~collapse tg]: when collapse is provided as argument then
     clause will not be taken into account*)
let parallel_for ?(clause : clause list = []) ?(collapse : int = 0) : target -> unit =
  ensure_header ();
  if collapse <> 0
    then Omp_basic.parallel_for ~clause:[Collapse(3)]
    else Omp_basic.parallel_for ~clause

let simd ?(clause : clause list = []) : target -> unit =
  ensure_header ();
  Omp_basic.simd ~clause
