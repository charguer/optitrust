open Optitrust
open Prelude

let _ = Flags.check_validity := true
(* let _ = Flags.pretty_matrix_notation := true *)

let _ = Run.script_cpp (fun _ ->
  !! Resources.ensure_computed ();
  let range a b = (trm_int a, trm_int b) in

  (* uninit_pre *)
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    !! Matrix_basic.local_name_tile
      ~alloc_instr:[cFunBody "malloc_uninit_pre"; cVarDef "a"] ~local_var:"x"
      ~uninit_pre:false ~uninit_post:false
      [range 0 10; range 2 10; range 0 4]
      [cFunBody "malloc_uninit_pre"; cFor ~body:[cArrayWrite "a"] "i"];
  );
  !! Matrix_basic.local_name_tile
    ~alloc_instr:[cFunBody "malloc_uninit_pre"; cVarDef "a"] ~local_var:"x"
    ~uninit_pre:true ~uninit_post:false
    [range 0 10; range 2 10; range 0 4]
    [cFunBody "malloc_uninit_pre"; cFor ~body:[cArrayWrite "a"] "i"];

  (* uninit_post *)
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Matrix_basic.local_name_tile
      ~alloc_instr:[cFunBody "malloc_uninit_post"; cVarDef "a"] ~local_var:"x"
      ~uninit_pre:true ~uninit_post:false
      [range 2 10]
      [cFunBody "malloc_uninit_post"; cFor ~body:[cArrayWrite "a"] "i"];
  );
  (* FIXME:
  !! Matrix_basic.local_name_tile
    ~alloc_instr:[cFunBody "malloc_uninit_post"; cVarDef "a"] ~local_var:"x"
    ~uninit_pre:false ~uninit_post:true
    [range 2 10]
    [cFunBody "malloc_uninit_post"; cFor ~body:[cArrayWrite "a"] "i"];

  (* uninit_pre + post *)
  !! Matrix_basic.local_name_tile
    ~alloc_instr:[cFunBody "malloc_uninit_prepost"; cVarDef "a"] ~local_var:"x"
    ~uninit_pre:true ~uninit_post:true
    [range 0 10; range 2 10; range 0 4]
    [cFunBody "malloc_uninit_prepost"; cFor ~body:[cArrayWrite "a"] "i"];
    *)

  (* !! Arith.default_simpl []; *)

  (* FIXME? non const =
  !! Matrix_basic.local_name_tile "b" ~into:"y" [tile 0 10; tile 2 8; tile 0 4] ~alloc_instr:[cWriteVar "b"] [cFor ~body:[cArrayWrite "b"] "j"]; *)
  !!! ();
)
