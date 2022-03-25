open Ast
open Target
include Matrix_basic


(* [intro_mcalloc tg] expects the target [tg] pointing to a variable declaration
    then it will check its body for a call to calloc. On this extended path it will call
    the basic intro_mcalloc transformation
*)
let intro_mcalloc : Target.Transfo.t =
  Target.iter_on_targets ( fun t p ->
    let tg_trm,_ = Path.resolve_path_and_ctx p t in
    match tg_trm.desc with
    | Trm_let (_, (_,_), init) ->
      begin match get_init_val init with
      | Some t1 ->
        begin match t1.desc with
        | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_cast _)));_},[calloc_trm]) ->
          begin match calloc_trm.desc with
          | Trm_apps ({desc = Trm_var (_, "calloc");_}, _) ->
            Matrix_basic.intro_mcalloc ((Target.target_of_path p) @ [Target.cFun "calloc"])
          | _ -> fail t1.loc "intro_mcalloc: couldn't find the call to calloc function"
          end
        | Trm_apps ({desc = Trm_var (_, "calloc");_},_) ->
          Matrix_basic.intro_mcalloc ((Target.target_of_path p) @ [Target.cFun "calloc"])
        | _ -> fail t1.loc "intro_mcalloc: couldn't find the call to calloc function'"
        end
      | _ -> fail None "intro_mcalloc: the targeted variable should be initialized"
      end

    | _ -> fail None "intro_mcalloc: the target should be a variable declarartion allocated with alloc")

(* [intro_mmalloc tg] expects the target [tg] pointing to a variable declaration
    then it will check its body for a call to malloc. On this extended path it will call
    the basic intro_mmalloc transformation
*)
let intro_mmalloc : Target.Transfo.t =
  Target.iter_on_targets ( fun t p ->
    let tg_trm,_ = Path.resolve_path_and_ctx p t in
    match tg_trm.desc with
    | Trm_let (_, (_,_), init) ->
      begin match get_init_val init with
      | Some t1 ->
        begin match t1.desc with
        | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_cast _)));_},[malloc_trm]) ->
          begin match malloc_trm.desc with
          | Trm_apps ({desc = Trm_var (_, "malloc");_}, _) ->
            Matrix_basic.intro_mmalloc ((Target.target_of_path p) @ [Target.cFun "malloc"])
          | _ -> fail t1.loc "intro_mmalloc: could not find a call to malloc function"
          end
        | Trm_apps ({desc = Trm_var (_, "malloc");_},_) ->
          Matrix_basic.intro_mmalloc ((Target.target_of_path p) @ [Target.cFun "malloc"])
        | _ -> fail t1.loc "intro_mmalloc: couldn't find a call to malloc function"
        end
      | _ -> fail None "intro_mmalloc: the targeted variable should be initialized"
      end
    | Trm_apps (_, [_lhs; rhs]) when is_set_operation tg_trm ->
         begin match rhs.desc with
        | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_cast _)));_},[malloc_trm]) ->
          begin match malloc_trm.desc with
          | Trm_apps ({desc = Trm_var (_, "malloc");_}, _) ->
            Matrix_basic.intro_mmalloc ((Target.target_of_path p) @ [Target.cFun "malloc"])
          | _ -> fail rhs.loc "intro_mmalloc: could not find a call to malloc function"
          end
        | Trm_apps ({desc = Trm_var (_, "malloc");_},_) ->
          Matrix_basic.intro_mmalloc ((Target.target_of_path p) @ [Target.cFun "malloc"])
        | _ -> fail rhs.loc "intro_mmalloc: couldn't find a call to malloc function"
        end
    | _ -> fail None "intro_mmalloc: the target should be a variable declarartion allocated with alloc")


(* [biject fun_bij tg] expects the target [tg] to be pointing at a matrix declaration , then it will search for all the occurrences
    of the matrix access, and replace MINDEX function with [fun_bij]
*)
let biject (fun_bij : string) : Target.Transfo.t =
  Target.iter_on_targets (fun t p ->
    let tg_trm = Path.resolve_path p t in
    let path_to_seq, _ = Internal.isolate_last_dir_in_seq p in
    match tg_trm.desc with
    | Trm_let (_, (p, _), _) ->
      Expr.replace_fun fun_bij [Target.nbAny; Target.cCellAccess ~base:[Target.cVar p] ~index:[Target.cFun ""] (); Target.cFun ~regexp:true "MINDEX."]
    | Trm_apps (_, [{desc = Trm_var (_, p)}; _])  when is_set_operation tg_trm -> 
      Expr.replace_fun fun_bij ((Target.target_of_path path_to_seq) @ [Target.nbAny; Target.cCellAccess ~base:[Target.cVar p] ~index:[Target.cFun ""] (); Target.cFun ~regexp:true "MINDEX."])
    | _ -> fail tg_trm.loc "biject: expected a variable declaration"

)

(* [intro_mops dims] expects the target [tg] pointing to an array declaration allocated with
      calloc or malloc, then it will apply intro_mcalloc or intor_mmaloc based on the type of
      the current allocation used. Then it will search for all accesses and apply intro_mindex
*)
let intro_mops (dim : trm) : Target.Transfo.t =
  Target.iter_on_targets (fun t p ->
    let path_to_seq,_ = Internal.isolate_last_dir_in_seq p in
    let tg_trm = Path.resolve_path p t in
    match tg_trm.desc with
    | Trm_let (_, (x,_), init) ->
      let tg_occs = [Target.nbAny] @ (Target.target_of_path path_to_seq) @ [Target.cCellAccess ~base:[Target.cVar x] ~index:[] ()] in
      begin match get_init_val init with
      | Some t1 ->
        begin match t1.desc with
        | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_cast _)));_},[malloc_trm]) ->
          begin match malloc_trm.desc with
          | Trm_apps ({desc = Trm_var (_, "calloc");_}, _) ->
            intro_mcalloc [Target.cVarDef x];
            Matrix_basic.intro_mindex dim tg_occs
          | Trm_apps ({desc = Trm_var (_, "malloc");_}, _) ->
            Matrix_basic.intro_mmalloc ((Target.target_of_path p) @ [Target.cFun "malloc"]);
            Matrix_basic.intro_mindex dim tg_occs
          | _ -> fail t1.loc "intro_mops: couldn't find a call to calloc/malloc function"
          end
        | Trm_apps ({desc = Trm_var (_, "calloc");_}, _) ->
            Matrix_basic.intro_mcalloc ((Target.target_of_path p) @ [Target.cFun "calloc"]);
            Matrix_basic.intro_mindex dim tg_occs
        | Trm_apps ({desc = Trm_var (_, "malloc");_}, _) ->
            Matrix_basic.intro_mmalloc ((Target.target_of_path p) @ [Target.cFun "malloc"]);
            Matrix_basic.intro_mindex dim tg_occs
        | _ -> fail t1.loc "intro_mmalloc:"
        end
      | _ -> fail None "intro_mmalloc: the targeted variable should be initialized"
      end
    | _ -> fail None "intro_mmalloc: the target should be a variable declarartion allocated with alloc")


(* [delocalize ~mark ~init_zero ~acc_in_place ~acc ~last ~var ~into ~dim ~index ~indices ~ops tg] this is a combi varsion of
  matrix_delocalize, this transformation first calls Matrix_basi.local_name to create the isolated environment where the delocalizing transformatino
  is going to be performed
*)
let delocalize ?(mark : mark option) ?(init_zero : bool = false) ?(acc_in_place : bool = false) ?(acc : string option)
  ?(last : bool = false)  ?(use : trm option = None) (var : var) ~into:(into : var) ~dim:(dim : trm)  ~index:(index : string)
  ?(indices : string list = []) ~ops:(ops : local_ops) ?(alloc_instr : Target.target option) ?(labels : label list = []) ?(dealloc_tg : target option = None) (tg : Target.target) : unit =

    let indices = match indices with | [] -> [] | _ as s_l -> s_l  in
    let middle_mark = match mark with | None -> Mark.next() | Some m -> m in
    let acc = match acc with | Some s -> s | _ -> "" in  Matrix_basic.local_name ~my_mark:middle_mark ~alloc_instr ~into ~indices ~local_ops:ops var tg;

    let any_mark = begin match use with | Some _ -> "any_mark_deloc" | _ -> "" end in
    Matrix_basic.delocalize ~init_zero ~acc_in_place ~acc ~any_mark ~dim ~index ~ops ~labels [Target.cMark middle_mark];

    let tg_decl_access = Target.cOr [[Target.cVarDef into];[Target.cCellAccess ~base:[Target.cVar into] ()]] in
    if last then Matrix_basic.reorder_dims ~rotate_n:1 () [Target.nbAny; Target.cMark middle_mark; tg_decl_access ;Target.cFun ~regexp:true "M.\\(NDEX\\|ALLOC\\)."] ;
    begin match use with
      | Some e ->   Specialize.any e [Target.nbAny; Target.cMark any_mark]
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
         if label_alloc <> "" then begin Instr.move_out ~dest:[tAfter;cChain alloc] [cLabel label_alloc]; Instr.move_out ~dest:[tBefore; cFunDef "" ~body:[cLabel label_alloc]] [cLabel label_alloc; cVarDef into] end;
         let label_dealloc = List.nth labels 2 in
         if label_dealloc <> "" then begin match dealloc_tg with
          | Some da_tg -> Instr.move_out ~dest:[tAfter; cChain da_tg] [cLabel label_dealloc]
          | None -> ()
          end
          else ();
         List.iter (fun l -> if l <> "" then Label.remove [cLabel l]) labels
      | None -> () (* No need to move allocation trms because the allocation trm blongs to the same sequence as [tg] *)

      end
    end;
    begin match mark with | None -> Marks.remove middle_mark [Target.cMark middle_mark] | _ -> () end


(* [reorder_dims ~rotate_n ~order tg] expects the target [tg] to be pointing at a matrix declaration, then it will find the occurrences of ALLOC and INDEX functions
      and apply the reordering of the dimensions.
*)
let reorder_dims ?(rotate_n : int option) ?(order : int list = []) () (tg : Target.target) : unit =
  let rotate_n = match rotate_n with Some n -> n | None -> 0  in
  Target.iter_on_targets (fun t p ->
    let path_to_seq,_ = Internal.isolate_last_dir_in_seq p in
    let tg_trm = Path.resolve_path p t in
    match tg_trm.desc with
    | Trm_let (_, (x, _), _) ->
        Matrix_basic.reorder_dims ~rotate_n ~order () ((Target.target_of_path path_to_seq) @ [Target.cOr [[Target.cVarDef x; Target.cFun ~regexp:true "M.ALLOC."];[Target.cCellAccess ~base:[Target.cVar x] (); Target.cFun ~regexp:true "MINDEX."]]])
    | _ -> fail tg_trm.loc "reorder_dims: expected a target to variable declaration"
  ) tg
