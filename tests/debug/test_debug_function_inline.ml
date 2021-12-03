open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

    !! Function_basic.inline ~body_mark:"foobd" [nbMulti; cFun "foo"];
    !! Accesses.intro [nbMulti; cMark "foobd"];
    !!!();
    !! Function_basic.inline [cFun "vect3_mul"];
    !!! ();

  (* TODO:

      after inlining body   Accesses.intro [cMark "body_of_inlining"]

      Accesses.intro tg

        using trm_map

        struct_get ( get( t ), f )  -> get ( struct_access ( t, f ) )

  *)
)