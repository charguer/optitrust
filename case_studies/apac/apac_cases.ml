open Optitrust

(** [setup]: sets up OptiTrust and APAC flags for the automatic parallelization
    of the Barcelona OpenMP Task Suite (= BOTS) as well as the `quicksort' and
    the `molecular_dyn' case studies. At the end, the function returns the path
    to the input sequential implementation of the [case] we want to consider and
    the destination path for the output parallel implementation. The [cutoff]
    parameters selects the mehotd APAC uses to prune parallelizable tasks.
    Possible values are `none', `counters', `model' or `both'. *)
let setup (case : string) (cutoff : string) : string * string =
  let apac = Apac_macros.cwd () ^ "/case_studies/apac/" in
  let bots = apac ^ "bots/" in
  let skip, includes, main, _bots =
    match case with
    | "alignment" ->
       (["init_matrix"; "pairalign_init"; "align_init"; "align_end";
         "align_verify"],
        [bots ^ "common"],
        "align",
        true)       
    | "concom" ->
       (["initialize"; "write_outputs"; "cc_init"; "cc_check"],
        [bots ^ "common"],
        "cc_par",
        true)
    | "fft" ->
       (["test_correctness"], [bots ^ "common"], "fft", true)
    | "fib" ->
       (["fib_verify_value"; "fib_verify"],
        [bots ^ "common"],
        "fib0",
        true)
    | "floorplan" ->
       (["read_inputs"; "write_outputs"; "floorplan_init"; "floorplan_end";
         "floorplan_verify"],
        [bots ^ "common"],
        "compute_floorplan",
        true)
    | "knapsack" ->
       (["read_input"; "knapsack_check"],
        [bots ^ "common"],
        "knapsack_main_par",
        true)
    | "molcular_dyn" ->
       ([], [], "main", false)
    | "nqueens" ->
       (["verify_queens"], [bots ^ "common"], "find_queens", true)
    | "quicksort" ->
       ([], [], "main", false)
    | "sort" ->
       (["sort_init"; "sort_verify"], [bots ^ "common"], "sort_par", true)
    | "sparselu" ->
       (["checkmat"; "genmat"; "print_structure"; "allocate_clean_block";
         "sparselu_init"; "sparselu_fini"; "sparselu_check"],
        [bots ^ "common"],
        "sparselu_par_call",
        true)
    | "strassen" ->
       (["init_matrix"; "compare_matrix"; "alloc_matrix"],
        [bots ^ "common"],
        "strassen_main_par",
        true)
    | "uts" ->
       (["uts_read_file"; "uts_show_stats"; "uts_check_result"],
        [bots ^ "common"],
        "parallel_uts",
        true)
    | _ -> failwith ("[Apac_cases] error: unknown case study `" ^ case ^ "'!")
  in
  let cutoff =
    match cutoff with
    | "none" ->
       Apac_flags.profile := false;
       Apac_flags.cutoff_count_and_depth := false;
       cutoff
    | "counters" ->
       Apac_flags.profile := false;
       Apac_flags.cutoff_count_and_depth := true;
       cutoff
    | "model" ->
       Apac_flags.profile := true;
       Apac_flags.cutoff_count_and_depth := false;
       cutoff
    | "both" ->
       Apac_flags.profile := true;
       Apac_flags.cutoff_count_and_depth := true;
       cutoff
    | _ ->
       failwith
         ("[Apac_cases] error: unknown cut-off strategy `" ^ cutoff ^ "'!")
  in
  Apac_flags.constify := true;
  Flags.c_parser_includes := includes;
  Apac_macros.skip skip;
  Apac_flags.main := main;
  let path, ext =
    if _bots then (bots ^ "omp-apac/" ^ case ^ "/", ".c")
    else (apac ^ case ^ "/", ".cpp")
  in
  (path ^ case ^ ext, path ^ case ^ "_" ^ cutoff ^ ext)

let () =
  let argc = Array.length Sys.argv in
  let which = if argc < 2 then "all" else Sys.argv.(1) in
  let how = if argc < 3 then "none" else Sys.argv.(2) in
  let input, output = setup which how in
  Run.apac input output
