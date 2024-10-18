open Optitrust

let () =
  Flags.code_print_width := 1024;
  Apac_flags.constify := true;
  Apac_flags.compile_with := (
    Apac_flags.Gnu,
    "./case_studies/molecular_dyn/tools.cpp -Wall -Wno-unused-label"
  );
  Apac_flags.profile := true;
  Run.apac
    "./case_studies/molecular_dyn/molecular_dyn.cpp"
    "./case_studies/molecular_dyn/molecular_dyn_parallel.cpp"
