open Optitrust
(* Not the expected result :
   - missing a "const" in the printing of type double;
   - mat3d typedef seems to vanish, why? *)

let _ =
    run
    ( fun _ ->
        set_init_source"inline_typ.cpp";
        inline_decl ~delete_decl:false ~decl_path:[cTypDef "uint"] ();
        inline_decl ~delete_decl:false ~decl_path:[cTypDef "cdouble"] ();
        inline_decl ~delete_decl:false ~decl_path:[cTypDef "mat3d"] ();
        dump()
    )