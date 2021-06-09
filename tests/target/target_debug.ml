open Optitrust open Run

let _ = run_unit_test (fun () ->
  (** There should be exactly one result to each of the commands;
      if it is not the case, we'll get an error. *)
  (* let show = Tr.target_show in *)
  let show = Generic.target_show in

  
  
  
(* only one of the two should work *)

   (* is ; part of it or not? *)

  show [cExprRegexp  "vect v2" ];
  show [cNb 0; cExpr "vect v2" ];

  show [cNb 0; cExpr "int r = 3"];(* using int r = 3; resolve to the main function!!!! *)
  show [cInstr "int r = 3"];(* using int r = 3; resolve to the main function!!!! *)

  show [cInstr "i++" ]; (*Works, in general but fails here because there are more then one occurrences of i++ *)
  show [cExprRegexp "f\\(.\\)" ]; (* Finds all the occurrences of the f function call, somehow it matches the for loop!!*)
  

  

)

  (* LATER: Implement cDef constructor *)
  (* show [ cDef "f" ]; *)
  (* show [ cDef "s" ]; *)
  (* show [ cDef "p2" ]; *)


(* LATER: smart constructors for checking calls to builtin operations such as get/set/compare/incr, etc *)

(* LATER: show [ cFunDefDef ~args:[[cTrue]; [cOfTyp "vect*"]] "" ]; *)

(* LATER: match typedef using a function over the body of the type definition *)
(* LATER: match a typedef struct using of a function over the list fields [(var*typ)list->bool] *)

(* LATER: match types using a function of their list of fields *)
