open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true


let insert_max_tab_on (tab : typed_var) ( size : trm ) (i :int )(t: trm) : trm  =
  let v,typ = tab in
  let seq,res = trm_inv trm_seq_inv t in
  let lbefore,t, lafter = Mlist.get_item_and_its_relatives i seq in
  let call_max = (trm_apps (trm_var (toplevel_var "get_max"))  [trm_var v;size] ) in
  let div = trm_exact_div ~typ:typ (trm_float ~typ:typ_f32 1.0) call_max in
  let insert_max = trm_let (new_var "sum", typ) div  in
  trm_seq_helper ?result:res [TrmMlist lbefore ; Trm t; Trm insert_max; TrmMlist lafter ]

let insert_max_tab ~(tab : typed_var) ~(size : trm )  (tg:target) : unit  =
apply_at_target_paths_in_seq (insert_max_tab_on tab size) tg


let _  = Run.script_cpp ( fun _ ->
  let f = cFunDef "accesses" in
  let tab = (find_typ_var "v" [f]) in
  let typed_tab = (tab,typ_f32) in
  let size = (trm_find_var "n" [cFunDefAndDecl "accesses"]) in
  let f = f in
  !! insert_max_tab ~tab:typed_tab ~size:(size)   [f; cVarDef "b" ];
  (* !! Matrix_basic.local_name tab ~into:"w" ~type_and_dims:(typ_f32, [size]) [cFunBody "accesses"; cFor "i"]; *)
  !! Matrix.local_name_tile ~uninit_post:true ~var:"v" ~local_var:"w" [f; cFor "i"];
  let w = trm_find_var "w" [f] in
  let address_pattern = Trm.(array_access w (pattern_var "i")) in

  !! Accesses.scale ~factor:(trm_find_var "sum" [f] ) ~address_pattern [cFor "i" ];

  !!Accesses.scale_var ~factor:(trm_find_var "sum" [f])  [cVarDef "a"];
(* TODO  : Function.inline [cCall "get_max"]; *)
  );

