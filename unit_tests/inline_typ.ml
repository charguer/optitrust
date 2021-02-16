open ScriptTools
(* Not the expected result :
   - missing a "const" in the printing of type double;
   - mat3d typedef seems to vanish, why? *)

let _ =
    run
    ( fun _ ->
        set_init_source"inline_typ.cpp";
        inline_decl ~delete_decl:false ~decl_path:[cType ~name:"uint" ()] ();
        inline_decl ~delete_decl:false ~decl_path:[cType ~name:"cdouble" ()] ();
        inline_decl ~delete_decl:false ~decl_path:[cType ~name:"mat3d" ()] ();
        dump()
    )