open Optitrust

let () =
  Flags.code_print_width := 1024;
  Apac_flags.cutoff_count_and_depth := true;
  Run.apac
    "./case_studies/quicksort/quicksort.cpp"
    "./case_studies/quicksort/quicksort_parallel.cpp"
