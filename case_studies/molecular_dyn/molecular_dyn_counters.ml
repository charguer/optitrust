open Optitrust

let () =
  Flags.code_print_width := 1024;
  Apac_flags.constify := true;
  Apac_flags.cutoff_count_and_depth := true;
  Run.apac
    "./case_studies/molecular_dyn/molecular_dyn.cpp"
    "./case_studies/molecular_dyn/molecular_dyn_parallel.cpp"
