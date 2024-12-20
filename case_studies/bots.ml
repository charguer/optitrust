open Optitrust

(** [setup]: sets up OptiTrust and APAC flags for the automatic parallelization
    of the Barcelona OpenMP Task Suite (= BOTS) as well as the `quicksort' and
    the `molecular_dyn' case studies. At the end, the function returns the path
    to the input sequential implementation of the [case] we want to consider and
    the destination path for the output parallel implementation. The [cutoff]
    parameters selects the mehotd APAC uses to prune parallelizable tasks.
    Possible values are `none', `counters', `model' or `both'. *)
let setup (case : string) (cutoff : string) : string * string =
  let bots = Apac_macros.cwd () ^ "/case_studies/bots/" in
  let skip, main =
    match case with
    | "alignment" ->
       (["init_matrix"; "pairalign_init"; "align_init"; "align_end";
         "align_verify"], "align")       
    | "concom" ->
       (["initialize"; "write_outputs"; "cc_init"; "cc_check"], "cc_par")
    | "fft" ->
       (["test_correctness"], "fft")
    | "fib" ->
       (["fib_verify_value"; "fib_verify"], "fib0")
    | "floorplan" ->
       (["read_integer"; "read_inputs"; "write_outputs"; "floorplan_init";
         "floorplan_end"; "floorplan_verify"], "compute_floorplan")
    | "knapsack" ->
       (["read_input"; "knapsack_check"], "knapsack_main_par")
    | "molecular_dyn" ->
       (["check"; "check_symb"; "check_force"], "compute")
    | "nqueens" ->
       (["verify_queens"], "find_queens")
    | "quicksort" ->
       (["check"], "sort")
    | "sort" ->
       (["sort_init"; "sort_verify"], "sort_par")
    | "sparselu" ->
       (["checkmat"; "genmat"; "print_structure"; "allocate_clean_block";
         "sparselu_init"; "sparselu_fini"; "sparselu_check"],
        "sparselu_par_call")
    | "strassen" ->
       (["init_matrix"; "compare_matrix"; "alloc_matrix"], "strassen_main_par")
    | "uts" ->
       (["uts_read_file"; "uts_show_stats"; "uts_check_result"], "parallel_uts")
    | _ -> failwith ("[bots] error: unknown case study `" ^ case ^ "'!")
  in
  let cutoff =
    match cutoff with
    | "none" ->
       Apac_flags.profile := false;
       Apac_flags.cutoff_count_and_depth := false;
       ""
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
         ("[bots] error: unknown cut-off strategy `" ^ cutoff ^ "'!")
  in
  Apac_flags.constify := true;
  Flags.c_parser_includes := [bots ^ "common"];
  Apac_flags.omit := ".*_se[qr]$";
  Apac_macros.skip skip;
  Apac_flags.main := main;
  Apac_flags.verbose := true;
  let path, ext = bots ^ "omp-apac/" ^ case ^ "/", ".cpp" in
  let cutoff = if cutoff <> "" then ("-" ^ cutoff) else "" in
  (path ^ case ^ ".in" ^ ext, path ^ case ^ cutoff ^ ext)

let () =
  let argc = Array.length Sys.argv in
  let which = if argc < 2 then "all" else Sys.argv.(1) in
  let how = if argc < 3 then "none" else Sys.argv.(2) in
  let input, output = setup which how in
  Run.apac input output
