open Optitrust
open Prelude

let _ =
  Flags.pretty_matrix_notation := true;
  Flags.print_optitrust_syntax := true;
  Flags.check_validity := false

let reconstruct_seq (lbefore : trm mlist) (t : trm) (lafter : trm mlist) : trm =
  let new_lbefore = Mlist.push_back t lbefore in
  trm_seq (Mlist.merge new_lbefore lafter)

(** [local_accumulate_on] : expect t to point at the overall sequence and i to be the index which corresponds to a for loop. : *)
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
               ret := Mlist.push_back (trm_set lhs (trm_var_get accumulator)) !ret;
               accumulation
           | _ -> t)
         instrs)
  in
  let new_lbefore = Mlist.merge lbefore !init in
  let new_lafter = Mlist.merge !ret lafter in
  trm_seq (Mlist.merge (Mlist.merge new_lbefore (Mlist.of_list [ new_for ])) new_lafter)

let local_accumulate ?(var_name = "acc") ~(typ : typ) (tg : target) : unit =
  apply_at_target_paths_before (local_accumulate_on ~var_name ~typ) tg

let quantized_var var = let (q_var,_)  = find_var ("q_" ^ var.name) [] in q_var

let to_quantize_var (t : trm) : trm =
  match trm_array_access_inv t with
| Some (base,index) -> ( match trm_var_inv base with
  | Some x ->  trm_cast ~ty_from:typ_f32  typ_i32  (trm_get_array_access (trm_var (quantized_var x)) index)
  | _ -> t )
| _ -> t

(* There are three things to change when quantizing :
- var def : should be casted to a new var with int* type
- accumulation : should be transformed with quantized accumulation
- de-quantiation : should happens on a set operation; we need to add multiplication *)
let quantize_accumulate_on ~group_size (i : int) (t : trm) =
  let seq, res = trm_inv trm_seq_inv t in
  let lbefore, t, lafter = Mlist.get_item_and_its_relatives i seq in
  let new_t =
    match trm_let_inv t with
    | Some (x, tx, init) ->
        trm_let_mut (x, typ_i8) (trm_int 0)
    | _ -> (
        match trm_compound_assign_inv Binop_add t with
        | Some (lhs, rhs) -> trm_bottom_up to_quantize_var t
        | _ -> (match trm_var_get_inv t with
        | Some (x) -> t
        | _ -> t ))
  in
  reconstruct_seq lbefore new_t lafter

let quantize_accumulate ~(group_size : var) (tg : target) =
  apply_at_target_paths_in_seq (quantize_accumulate_on ~group_size) tg

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
      (* !!help_printer [ cFunBody "matvec"; cFor "k" ]; *)
      !!local_accumulate ~typ:typ_f32 [ cFunBody "matvec"; cFor "k"; tBefore ];
      !!Loop.tile ~index:"j" (trm_find_var "GS" []) [ cFunBody "matvec"; cFor "k" ];
      !!local_accumulate ~var_name:"ilocal" ~typ:typ_f32 [ cFunBody "matvec"; cFor "k"; tBefore ];
      !!quantize_accumulate ~group_size:(find_typ_var "GS" [])
        [ nbMulti; cFunBody "matvec"; cVarDef "ilocal" ];
       !!quantize_accumulate ~group_size:(find_typ_var "GS" [])
        [ nbMulti; cFunBody "matvec"; cWriteVar "ilocal" ];
     (* !!quantize_accumulate ~group_size:(find_typ_var "GS" [])
        [ nbMulti; cFunBody "matvec"; cReadVar "ilocal" ]) *)
  )
