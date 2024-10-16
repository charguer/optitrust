open Optitrust

let () =
  Flags.code_print_width := 1024;
  Apac_flags.cutoff_count_and_depth := true;
  Run.apac
    "./case_studies/fibonacci/fibonacci.cpp"
    "./case_studies/fibonacci/fibonacci_parallel.cpp"
