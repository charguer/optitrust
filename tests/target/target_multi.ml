open Optitrust
open Target

let _ = Run.script_cpp (fun () ->

  (* This is a demo for the [nb*] constraints, so we are purposely not precise about the number of occurrences *)

  (* One (ExpectedOne is the default) *)
  show [ cFor "j" ];

  (* Multi *)
  show [ nbMulti; cFor "i" ];
  show [ nbMulti; cCall "f" ];
  show [ nbMulti; cFunDef "main"; cFor "i" ];

  (* NbExact*)
  show [ nbExact 0; cFunDef "main"; cFor "j" ]; (* zero match *)
  show [ nbExact 1; cFunDef "main"; cFor "i" ];
  show [ nbExact 2; cFor "i" ];

  (* Any *)
  show [ nbAny; cFunDef "main"; cFor "j" ]; (* zero match *)
  show [ nbAny; cFunDef "main"; cFor "i" ];
  show [ nbAny; cFor "i" ];
  show [nbMulti;cAnd [[cReturn];[cTypDef "vect"];[cFunDef "main"; cFor "i"]]];

    (* Testing the [target_of_path] function *)
  !! let paths = ref [] in
  Target.apply_on_targets (fun t p -> paths := p :: !paths; t) [ nbMulti; cFor "i" ];
  let targets = List.map target_of_path !paths in
  let tg1,tg2 =
    match targets with
    | target1:: target2 :: [] -> target1,target2
    | _ ->  assert false
    in
  show tg1;
  show tg2;
  
)


