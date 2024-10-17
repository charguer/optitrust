open Optitrust

let () =
  Flags.code_print_width := 1024;
  Apac_flags.constify := true;
  Apac_flags.profile := true;
  Run.apac
    "./case_studies/sparselu/sparselu.cpp"
    "./case_studies/sparselu/sparselu_parallel.cpp"
