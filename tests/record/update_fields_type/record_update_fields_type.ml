open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Record_basic.update_fields_type "x" typ_f32 [cTypDef "vect"];
  !! Record_basic.update_fields_type "y" typ_f32 [cTypDef "vect"];

)
