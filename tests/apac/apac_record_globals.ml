open Optitrust
open Ast
open Target

let _ = Run.script_cpp (fun () ->
            !! Apac_preprocessing.record_globals [
                nbAny;
                cStrict;
                cVarDef ""
              ];
            Var_map.iter (fun v _ ->
                !! Marks_basic.add "global" [
                    cVarDef v.name
                  ];
              ) !Apac_records.globals
          );
        Apac_reset.tnt_blast ()
