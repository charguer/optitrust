open Optitrust
open Prelude

let _ = Flags.check_validity := false (* TODO *)
(* let _ = Flags.pretty_matrix_notation := true *)

let _ = Run.script_cpp (fun _ ->
  !! Resources.ensure_computed ();
  let tile offset size = (trm_int offset, trm_int size) in
  !! Trace.failure_expected (fun () ->
    !! Matrix_basic.local_name_tile ~alloc_instr:[cVarDef "a"] ~local_var:"x"
      ~uninit_pre:false ~uninit_post:false
      [tile 0 10; tile 2 8; tile 0 4] [cFor ~body:[cArrayWrite "a"] "i"];
    !! Arith.default_simpl [];
    !! Resources.ensure_computed ();
  );
  !! Matrix_basic.local_name_tile ~alloc_instr:[cVarDef "a"] ~local_var:"x"
    ~uninit_pre:true ~uninit_post:false
    [tile 0 10; tile 2 8; tile 0 4] [cFor ~body:[cArrayWrite "a"] "i"];
  (* !! Resources.ensure_computed (); *)
  !! Arith.default_simpl [];
  !! Resources.ensure_computed ();
  (* FIXME? non const =
  !! Matrix_basic.local_name_tile "b" ~into:"y" [tile 0 10; tile 2 8; tile 0 4] ~alloc_instr:[cWriteVar "b"] [cFor ~body:[cArrayWrite "b"] "j"]; *)
  !!! ();

  (* TODO: need many more unit tests *)
)
