open Optitrust
open Run
(* Not the expected result :
   - missing a "const" in the printing of type double;
   - mat3d typedef seems to vanish, why? *)

let _ =
    run
    ( fun _ ->
        set_init_source"inline_typ.cpp";
        Declaration.inline_typedef false ~decl_path:[cTypDef "uint"] ();
        Declaration.inline_typedef false ~decl_path:[cTypDef "cdouble"] ();
        Declaration.inline_decl false ~decl_path:[cTypDef "mat3d"] ();
        dump()
    )