open Optitrust
open Prelude

let _ =
  Flags.pretty_matrix_notation := true;
  Flags.print_optitrust_syntax := true;
  Flags.check_validity := false

(** [local_accumulate_on] : expect t to point at the overall sequence and i to be the index which
    correpsonds to a for loop Expects the for loops to accumulate results inside Transform : *)
let local_accumulate_on ~(var_name : string) ~(typ : typ) (t : trm) (i : int) : trm =
  let seq, _res = trm_inv trm_seq_inv t in
  let lbefore, t_for, lafter = Mlist.get_item_and_its_relatives i seq in
  let range, instrs, contract =
    trm_inv ~error:"local_accumulate_on: expected a for trm" trm_for_inv_instrs t_for
  in
  let init = ref Mlist.empty in
  let ret = ref Mlist.empty in
  let new_for =
    trm_for_instrs ~contract range
      (Mlist.map
         (fun t ->
           match trm_compound_assign_inv Binop_add t with
           | Some (lhs, rhs) ->
               let accumulator = new_var var_name in
               init := Mlist.push_back (trm_let_mut (accumulator, typ) (trm_int 0)) !init;
               let accumulation = trm_compound_assign ~typ Binop_add (trm_var accumulator) rhs in
               ret := Mlist.push_back (trm_set lhs (trm_var_get accumulator)) !ret ;
               accumulation
           | _ -> t)
         instrs)
  in
  let new_lbefore = Mlist.merge lbefore !init in
  let new_lafter = Mlist.merge !ret lafter in
  trm_seq (Mlist.merge (Mlist.merge new_lbefore (Mlist.of_list [ new_for ])) new_lafter)

let local_accumulate ?(var_name = "acc") ~(typ : typ) (tg : target) : unit =
  apply_at_target_paths_before (local_accumulate_on ~var_name ~typ) tg


let help_printer tg =
  apply_at_target_paths
    (fun x ->
      Printf.printf "%s \n " (Ast_to_text.ast_to_string x);
      x)
    tg

let _ =
  Run.script_cpp (fun () ->
      (* !!Loop.tile (trm_find_var "GS" []) ~index:"j" ~bound:TileBoundMin ~iter:TileIterLocal
        [ cFunBody "matvec"; cFor "k" ]; *)
      !!help_printer [ cFunBody "matvec"; cFor "k" ];
      !!local_accumulate ~typ:typ_f32 [ cFunBody "matvec"; cFor "k"; tBefore ];
      !!Loop.tile ~index:"j" (trm_find_var "GS" []) [ cFunBody "matvec"; cFor "k" ];
      !!local_accumulate ~var_name:"ilocal" ~typ:typ_f32 [ cFunBody "matvec"; cFor "k"; tBefore ])
