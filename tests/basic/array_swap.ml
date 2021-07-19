open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    !! Arrays.swap [cTypDef "T"];
    (* LATER: figure out the high-level interface for generating copies,
       but it's not specific to swap at all.
       we want to introduce  T' as an alias for T, but as a "non convertible type" *)
)