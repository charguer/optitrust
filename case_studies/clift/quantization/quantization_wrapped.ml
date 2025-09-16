open Optitrust
open Prelude

(*
let quantize ~(q: trm) ~(f_q:trm) ~(nb_groups:int) (t:trm) : trm =
  (* Step to perform:
  - Find variables used in f call :
  - create variables needed for quantization
  - call quantize_aux  *)

  let t,args = trm_inv trm_apps_inv  t in
  let (x_out,x_in,w,dims) = match args with
  | [x_out;x_in;w] -> x_out,x_in,w,dims
  | _ > None,None,None,None in
  List.iter find_argument args; *)

let arg_map (tl : trm list) : trm list =
  match tl with
  | [ xout; x; w; n; d ] ->
      [ xout; x; trm_find_var "qw" [ cFunDef "main" ]; trm_find_var "s_w" [ cFunDef "main" ]; n; d ]
  | _ -> []

(** [handle_mallocs vars ] : Create allocation trms from the entry [vars] *)
let handle_mallocs (vars : (typed_var * typ) list) =
  let mlist_malloc =
    Mlist.map
      (fun ((var, typ), size) ->
        let malloc_in = Matrix_trm.alloc ~zero_init:true (get_inner_ptr_type typ) [ size ] in
        trm_let (var, typ) malloc_in)
      (Mlist.of_list vars)
  in
  trm_seq_nobrace mlist_malloc

(** [handle_calls f_in f args_in args] : Helper function for project_on, takes as input the f_in and
    f functions and theirs arguments to create list of calls*)
let handle_calls ~(f_in : var) ~(f : var) ~(args_in : trms) ~(args : trms) : trm =
  let list_seq = [ trm_apps (trm_var f_in) args_in; trm_apps (trm_var f) args ] in
  trm_seq_nobrace (Mlist.of_list list_seq)

(** [handle_frees vars ] : Helper function for project_on, free previously temporary variables *)
let handle_frees (vars : (typed_var * typ) list) =
  trm_seq_nobrace
    (Mlist.map (fun ((var, typ), _size) -> Matrix_trm.free (trm_var var)) (Mlist.of_list vars))

(** [project_on f_in f args_in extra_args t] : The aim of this function is to try to do a simple
    transformation where we transform the entry to project : f(x_out,x_in) -> f_in(x_in_bis,x_in);
    f(x_out;x_in_bis) Requirements : x_out is always first, x_out and x_in are pointers, size_of
    each vector is known Also we need the extra args of f_in and f, and they are quiet the same. *)
let project_on ~(f_in : trm) ~(f : trm) ~(args_in : (typed_var * trm) list) ~(extra_args : trm list)
    (t : trm) : trm =
  let f_in_var, _ret, _args_in, _body, _fun_spec = trm_inv trm_let_fun_inv f_in in
  let f_var, _ret, _f_args, _body, _fun_spec = trm_inv trm_let_fun_inv f in
  let f_before, args = trm_inv trm_apps_inv t in
  let x_in = List.nth args 1 in
  let x_out = List.nth args 0 in
  let to_malloc = List.filter (fun (x, t) -> t != trm_dummy) args_in in
  let allocs = handle_mallocs to_malloc in
  let trms_in = List.map (fun ((x, typ), t) -> trm_var x) args_in in
  let trms_f_in = List.map (fun ((x, t), _t) -> trm_var x) to_malloc in
  let calls =
    handle_calls ~f_in:f_in_var ~args_in:(trms_in @ [ x_in ]) ~f:f_var
      ~args:([ x_out ] @ trms_f_in @ extra_args)
  in
  let frees = handle_frees to_malloc in
  trm_seq_nobrace (Mlist.of_list [ allocs; calls; frees ])

let project ~(f_in : target) ~(f : target) ~(args_in : (typed_var * trm) list)
    ~(extra_args : trm list) (tg : target) : unit =
  let t_fin = get_trm_at_exn f_in in
  let t_f = get_trm_at_exn f in
  Nobrace_transfo.remove_after (fun x ->
      apply_at_target_paths (fun x -> project_on ~f_in:t_fin ~f:t_f ~args_in ~extra_args x) tg)

(* let _ =
  Run.script_cpp (fun x ->
      let mat_wrapper, _ = find_var "matvec_quantized_wrapper" [] in
      !!Function.replace_with_change_args mat_wrapper arg_map [ cCall "matvec" ]) *)
let help_printer tg =
  apply_at_target_paths
    (fun x ->
      Printf.printf "%s \n " (Ast_to_text.ast_to_string x);
      x)
    tg

let try_get_array_size (t : trm) : trm =
  let _test, _size =
    match t.typ with
    | Some x -> (
        match typ_array_inv x with
        | Some (y, Some z) -> (y, z)
        | _ -> (trm_dummy, trm_dummy))
    | _ -> (trm_dummy, trm_dummy)
  in
  t

let _ =
  Run.script_cpp (fun x ->
      (* !!project
        ~f_in:[ cFunDefAndDecl "test_f_in" ]
        ~f:[ cFunDefAndDecl "test_f" ]
        ~args_in:[ ((new_var "x_in_tmp", typ_ptr typ_int), trm_find_var "n" [ cFunDef "main" ]) ]
        ~extra_args:[]
        [ cCall "test_f_before" ]; *)
      let n_var = find_typ_var "n" [ cFunDef "main" ] in
      let n = trm_find_var "n" [ cFunDef "main" ] in
      let d = trm_find_var "d" [ cFunDef "main" ] in
      !!project
        ~f_in:[ cFunDefAndDecl "quantize" ]
        ~f:[ cFunDefAndDecl "matvec_quantized" ]
        ~args_in:
          [
            ((new_var "x_in_tmp", typ_ptr typ_i8), n);
            ((new_var "s_in_tmp", typ_ptr typ_f32), n);
            ((n_var, typ_int), trm_dummy);
          ]
        ~extra_args:
          [ trm_find_var "qw" [ cFunDef "main" ]; trm_find_var "s_w" [ cFunDef "main" ]; n; d ]
        [ cCall "matvec" ];
      !!!())
