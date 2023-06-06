open Ast
open Target
include Matrix_basic


(* [intro_calloc tg]: expects the target [tg] to point at a variable declaration
    then it will check its body for a call to calloc. On this extended path it will call
    the [Matrix_basic.intro_calloc] transformation *)
let%transfo intro_calloc (tg : target) : unit =
  iter_on_targets ( fun t p ->
    let tg_trm,_ = Path.resolve_path_and_ctx p t in
    match tg_trm.desc with
    | Trm_let (_, (x,_), init) ->
      begin match get_init_val init with
      | Some t1 ->
        begin match t1.desc with
        | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_cast _)));_},[calloc_trm]) ->
          begin match calloc_trm.desc with
          | Trm_apps ({desc = Trm_var (_, f);_}, _) when (is_qvar_var f "calloc") ->
            Matrix_basic.intro_calloc ((target_of_path p) @ [cFun "calloc"])
          | _ -> fail t1.loc "intro_calloc: couldn't find the call to calloc function"
          end
        | Trm_apps ({desc = Trm_var (_, f);_},_) when (is_qvar_var f "calloc") ->
          Matrix_basic.intro_calloc ((target_of_path p) @ [cFun "calloc"])
        | _ -> try Matrix_basic.intro_calloc [cWriteVar x; cFun "calloc"]
          with | TransfoError _ -> fail tg_trm.loc "intro_calloc: couldn't find the calloc
            opertion on the targeted variable"
        end

      | _ ->
         try Matrix_basic.intro_calloc [cWriteVar x; cFun "calloc"]
          with | TransfoError _ -> fail tg_trm.loc "intro_calloc: couldn't find the calloc
            opertion on the targeted variable"
      end

    | _ -> fail None "intro_calloc: the target should be a variable declarartion allocated with alloc"
  ) tg


(* [intro_mindex dim]; expects the target [tg] to point at at a matrix declaration, then it will change
     all its occurrence accesses into Optitrust MINDEX accesses. *)
let%transfo intro_mindex (dim : trm) (tg : target) : unit =
  iter_on_targets (fun t p ->
    let tg_trm = Path.get_trm_at_path p t in
    let error = "Matrix.intro_mindex: the target should point at matrix declaration." in
    let (_, x, _, _) = trm_inv ~error trm_let_inv tg_trm in
    Matrix_basic.intro_mindex dim [nbAny; cCellAccess ~base:[cVar x] ()]
  ) tg

(* [intro_malloc tg]: expects the target [tg] to point at a variable declaration
    then it will check its body for a call to malloc. On this extended path it will call
    the [Matrix_basic.intro_malloc] transformation. *)
let%transfo intro_malloc (tg : target) : unit =
  iter_on_targets ( fun t p ->
    let tg_trm,_ = Path.resolve_path_and_ctx p t in
    match tg_trm.desc with
    | Trm_let (_, (x,_), init) ->
      begin match get_init_val init with
      | Some t1 ->
        begin match t1.desc with
        | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_cast _)));_},[malloc_trm]) ->
          begin match malloc_trm.desc with
          | Trm_apps ({desc = Trm_var (_, f);_}, _) when (is_qvar_var f "malloc") ->
            Matrix_basic.intro_malloc ((target_of_path p) @ [cFun "malloc"])
          | _ -> fail t1.loc "intro_malloc: couldn't find the call to malloc function"
          end
        | Trm_apps ({desc = Trm_var (_, f);_},_) when  (is_qvar_var f "malloc") ->
          Matrix_basic.intro_malloc ((target_of_path p) @ [cFun "malloc"])
        | _ ->
         try Matrix_basic.intro_malloc [cWriteVar x; cFun "malloc"]
          with | TransfoError _ -> fail tg_trm.loc "intro_malloc: couldn't find the malloc
            operation on the targeted variable"
        end

      | _ ->
         try Matrix_basic.intro_malloc [cWriteVar x; cFun "malloc"]
          with | TransfoError _ -> fail tg_trm.loc "intro_malloc: couldn't find the malloc
            opertion on the targeted variable"
      end
    | _ -> fail None "intro_malloc: the target should be a variable declarartion allocated with alloc"
  ) tg


(* [biject fun_bij tg]: expects the target [tg] to point at at a matrix declaration , then it will search for all its
    acccesses and replace MINDEX with  [fun_bij]. *)
let%transfo biject (fun_bij : string) (tg : target) : unit =
  iter_on_targets (fun t p ->
    let tg_trm = Path.resolve_path p t in
    let path_to_seq, _ = Internal.isolate_last_dir_in_seq p in
    match tg_trm.desc with
    | Trm_let (_, (p, _), _) ->
      Expr.replace_fun fun_bij [nbAny; cCellAccess ~base:[cVar p] ~index:[cFun ""] (); cFun ~regexp:true "MINDEX."]
    | Trm_apps (_, [{desc = Trm_var (_, p)}; _])  when is_set_operation tg_trm ->
      Expr.replace_fun fun_bij ((target_of_path path_to_seq) @ [nbAny; cCellAccess ~base:[cVar p.qvar_var] ~index:[cFun ""] (); cFun ~regexp:true "MINDEX."])
    | _ -> fail tg_trm.loc "biject: expected a variable declaration"
  ) tg

(* [intro_mops dims]: expects the target [tg] to point at an array declaration allocated with
      calloc or malloc, then it will apply intro_calloc or intor_mmaloc based on the type of
      the current allocation used. Then it will search for all accesses and apply intro_mindex. *)
let%transfo intro_mops (dim : trm) (tg : target) : unit =
  iter_on_targets (fun t p ->
    let tg_trm = Path.get_trm_at_path p t in
    let error = "Matrix.intro_mops: the target should be pointing at a matrix declaration" in
    let _ = trm_inv ~error trm_let_inv tg_trm in
    intro_mindex dim (target_of_path p);
    match Trace.backtrack_on_failure (fun () ->
      intro_malloc (target_of_path p)
    ) with
    | Success -> ()
    | Failure _ -> begin
      match Trace.backtrack_on_failure (fun () ->
        intro_calloc (target_of_path p)
      ) with
      | Success -> ()
      | Failure _ -> fail tg_trm.loc "intro_mops: the targeted matrix was not allocated with malloc or calloc"
    end
  ) tg


(* [elim_mops]: expects the target [tg] to point at a subterm and
  eliminates all MINDEX macros in that subterm.

  TODO:
  - eliminate MALLOC2 into malloc(sizeof(T[n][m]))?
  - ~simpl
*)
let%transfo elim_mops (tg : target): unit =
  Trace.step_valid_by_composition ();
  let targets = ref [] in
  Target.iter (fun _ p ->
    targets := (target_of_path p) :: !targets;
  ) tg;
  !targets |> List.iter (fun tg ->
    elim_mindex (tg @ [nbAny; cMindex ()]);
    (* TODO: more precise target ? *)
    Arith.(simpl_rec gather_rec) tg;
    Arith.(simpl_rec compute) tg
  )

(* [delocalize ~mark ~init_zero ~acc_in_place ~acc ~last ~var ~into ~dim ~index ~indices ~ops tg]: this is a combi
   varsion of [Matrix_basic.delocalize], this transformation first calls Matrix_basi.local_name to create the isolated
    environment where the delocalizing transformatino is going to be performed *)
let%transfo delocalize ?(mark : mark option) ?(init_zero : bool = false) ?(acc_in_place : bool = false) ?(acc : string option)
  ?(last : bool = false)  ?(use : trm option) (var : var) ~into:(into : var) ~dim:(dim : trm)  ~index:(index : string)
  ?(indices : string list = []) ~ops:(ops : local_ops) ?(alloc_instr : target option) ?(labels : label list = []) ?(dealloc_tg : target option) (tg : target) : unit =

    let indices = match indices with | [] -> [] | _ as s_l -> s_l  in
    let middle_mark = match mark with | None -> Mark.next() | Some m -> m in
    let acc = match acc with | Some s -> s | _ -> "" in  Matrix_basic.local_name ~my_mark:middle_mark ?alloc_instr ~into ~indices ~local_ops:ops var tg;

    let any_mark = begin match use with | Some _ -> "any_mark_deloc" | _ -> "" end in
    Matrix_basic.delocalize ~init_zero ~acc_in_place ~acc ~any_mark ~dim ~index ~ops ~labels [cMark middle_mark];

    let tg_decl_access = cOr [[cVarDef into];[cWriteVar into]; [cCellAccess ~base:[cVar into] ()]] in
    if last then Matrix_basic.reorder_dims ~rotate_n:1 [nbAny; tg_decl_access; cFun ~regexp:true "M\\(.NDEX\\|ALLOC\\)."] ;
    begin match use with
      | Some e ->   Specialize.any e [nbAny; cMark any_mark]
      | None -> ()
    end;
    begin match labels with
    | [] -> () (* labels argument was not used by the user *)
    | _ ->
      begin match alloc_instr with
      | Some alloc  ->
         let nb_labels = List.length labels in
         if nb_labels <> 3 then ();
         let label_alloc = List.nth labels 0 in
         if label_alloc <> "" then begin Instr.move ~dest:[tAfter;cTarget alloc] [cLabel label_alloc]; Instr.move ~dest:[tBefore; cFunDef "" ~body:[cLabel label_alloc]] [cLabel label_alloc; cVarDef into] end;
         let label_dealloc = List.nth labels 2 in
         if label_dealloc <> "" then begin match dealloc_tg with
          | Some da_tg -> Instr.move ~dest:[tAfter; cTarget da_tg] [cLabel label_dealloc]
          | None -> ()
          end
          else ();
         List.iter (fun l -> if l <> "" then Label.remove [cLabel l]) labels
      | None -> () (* No need to move allocation trms because the allocation trm blongs to the same sequence as [tg] *)

      end
    end;
    begin match mark with | None -> Marks.remove middle_mark [cMark middle_mark] | _ -> () end


(* [reorder_dims ~rotate_n ~order tg] expects the target [tg] to point at at a matrix declaration, then it will find the occurrences of ALLOC and INDEX functions
      and apply the reordering of the dimensions. *)
let%transfo reorder_dims ?(rotate_n : int option) ?(order : int list = []) (tg : target) : unit =
  let rotate_n = match rotate_n with Some n -> n | None -> 0  in
  iter_on_targets (fun t p ->
    let path_to_seq,_ = Internal.isolate_last_dir_in_seq p in
    let tg_trm = Path.resolve_path p t in
    let error = "Matrix.reorder_dims: expected a target to a variable declaration." in
    let (_, x, _, _) = trm_inv ~error trm_let_inv tg_trm in
    Matrix_basic.reorder_dims ~rotate_n ~order ((target_of_path path_to_seq) @ [cOr [[cVarDef x; cFun ~regexp:true "M.ALLOC."];[cCellAccess ~base:[cVar x] (); cFun ~regexp:true "MINDEX."]]])
  ) tg

(* FIXME:
  - (1) should be defined elsewhere;
  - (2) should start from replaced bottom leaf instead of top scope target? *)
let simpl_void_loops = Loop.delete_all_void

(* [elim]: eliminates the matrix [var] defined in at the declaration targeted by [tg].
  All reads from [var] must be eliminated The values of [var] must only be read locally, i.e. directly after being written.
  *)
let%transfo elim ?(simpl : Transfo.t = simpl_void_loops) (tg : target) : unit =
  Target.iter (fun t p_def ->
    let t_def = Path.resolve_path p_def t in
    let (_, x, _, _) = trm_inv ~error:"expected variable definition" trm_let_inv t_def in
    let (_, p_seq) = Path.index_in_seq p_def in
    let tg_seq = target_of_path p_seq in
    read_last_write ~write:(tg_seq @ [cArrayWrite x]) (tg_seq @ [nbAny; cArrayRead x]);
    (* FIXME: dangerous transformation? *)
    (* TODO: Matrix.delete_not_read *)
    Instr.delete (tg_seq @ [nbAny; cArrayWrite x]);
    delete ~var:x tg_seq;
    simpl tg_seq
  ) tg

(* TODO: local_name_tile ~shift_to_zero *)
(* + shift_to_zero ~nest_of *)

(* [elim_constant]: expects [tg] to target a matrix definition,
   then first uses [Matrix.elim_mops] on all reads before attempting
   to use [Arrays.elim_constant].
   *)
let%transfo elim_constant ?(simpl : Transfo.t = Arith.default_simpl) (tg : target) : unit =
  Target.iter (fun t p_def -> Marks.with_fresh_mark (fun mark_accesses ->
    let t_def = Path.resolve_path p_def t in
    let (_, x, _, _) = trm_inv ~error:"expected variable definition" trm_let_inv t_def in
    let (_, p_seq) = Path.index_in_seq p_def in
    (* TODO: use simpl there as well? *)
    elim_mops ((target_of_path p_seq) @ [nbAny; cArrayRead x]);
    Arrays.elim_constant ~mark_accesses (target_of_path p_def);
    simpl [nbAny; cMark mark_accesses];
  )) tg

(* [delete] expects target [tg] to point to a definition of matrix [var], and deletes it.
  Both allocation and de-allocation instructions are deleted.
  Checks that [var] is not used anywhere in the visible scope.
   *)
let%transfo delete (tg : target) : unit =
  Target.iter (fun t p ->
    let t_local = Path.get_trm_at_path p t in
    let error = "Matrix.delete: expected target on variable definition" in
    let (_, var, _, _) = trm_inv ~error trm_let_inv t_local in
    let (_, p_seq) = Path.index_in_seq p in
    Matrix_basic.delete ~var (target_of_path p_seq)
  ) tg

let delete_alias = delete

(* [local_name_tile]: like the basic transfo, but deletes the
   original matrix if [delete] is true or if [into] is empty.
   *)
let%transfo local_name_tile ?(delete: bool = false) ?(indices : (var list) = []) ?(alloc_instr : target option) (v : var) ?(into : var = "") (tile : Matrix_core.nd_tile) ?(local_ops : local_ops = Local_arith (Lit_int 0, Binop_add)) ?(simpl : Transfo.t = Arith.default_simpl) (tg : target) : unit =
  let (delete, rename, into) = if into = ""
    then (true, true, fresh_var ())
    else (delete, false, into)
  in
  Marks.with_fresh_mark (fun mark_accesses -> Target.iter (fun t p ->
    Matrix_basic.local_name_tile ~mark_accesses ~indices ?alloc_instr v ~into tile ~local_ops (target_of_path p);
    simpl [cMark mark_accesses];
    if delete then begin
      let (_, surrounding_seq) = Path.index_in_seq p in
      let surrounding_tg = target_of_path surrounding_seq in
      (* FIXME: dangerous transformation, replace with:
        - Matrix.delete_dead_writes [cArrayWrite v]
        - Matrix.delete_dead_writes [cArrayRead v] / [cMark mark; dSeqNth 0]
          *)
      Instr.delete (surrounding_tg @ [nbMulti; cFor "" ~body:[cOr [[cArrayRead v]; [cArrayWrite v]]]]);
      Loop.delete_all_void surrounding_tg;
      Marks.with_fresh_mark_on p (fun m ->
        delete_alias (Option.value ~default:(surrounding_tg @ [cVarDef v]) alloc_instr);
        if rename then
          Variable_basic.rename ~into:v [cMark m; cVarDef into];
      )
    end
  ) tg)