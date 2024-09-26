open Optitrust
open Ast
open Target

let _ = Run.script_cpp (fun () ->
            !! Apac_preprocessing.record_globals [
                nbAny;
                cStrict;
                cVarDef ""
              ];
            Var_set.iter (fun v ->
                !! Marks_basic.add "global" [
                    cVarDef v.name
                  ];
              ) !Apac_records.globals
          );
        Apac_reset.tnt_blast ()
