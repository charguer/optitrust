open Optitrust

let () =
  Flags.code_print_width := 1024;
  Run.apac
    "./case_studies/quicksort/quicksort.cpp"
    "./case_studies/quicksort/quicksort_parallel.cpp"
