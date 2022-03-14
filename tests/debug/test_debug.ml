open Optitrust
open Target

let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun _ ->

      
   !! Instr.inline_last_write  [cCellRead ~base:[cFieldRead ~field:("itemsPos" ^ "X") ()]()];
      Instr.inline_last_write  [cCellRead ~base:[cFieldRead ~field:("itemsPos" ^ "Y") ()]()];
      Instr.inline_last_write  [cCellRead ~base:[cFieldRead ~field:("itemsPos" ^ "Z") ()]()];
   !! Instr.inline_last_write  [cCellRead ~base:[cFieldRead ~field:("itemsSpeed" ^ "X") ()]()];
      Instr.inline_last_write  [cCellRead ~base:[cFieldRead ~field:("itemsSpeed" ^ "Y") ()]()];
      Instr.inline_last_write  [cCellRead ~base:[cFieldRead ~field:("itemsSpeed" ^ "Z") ()]()];
)

(* LATER: at the combi level, combine struct_inline with struct-renaming-field *)
