open Ast
open Target
include Matrix_basic


(* [intro_calloc tg] expects the target [tg] pointing to a variable declaration
    then it will check its body for a call to calloc. On this extended path it will call
    the basic intro_calloc transformation
*)

let intro_calloc : Transfo.t =
  iter_on_targets ( fun t p ->
    let tg_trm,_ = Path.resolve_path_and_ctx p t in
    match tg_trm.desc with
    | Trm_let (_, (x,_), init) ->
      begin match get_init_val init with
      | Some t1 ->
        begin match t1.desc with
        | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_cast _)));_},[calloc_trm]) ->
          begin match calloc_trm.desc with
          | Trm_apps ({desc = Trm_var (_, "calloc");_}, _) ->
            Matrix_basic.intro_calloc ((target_of_path p) @ [cFun "calloc"])
          | _ -> fail t1.loc "intro_calloc: couldn't find the call to calloc function"
          end
        | Trm_apps ({desc = Trm_var (_, "calloc");_},_) ->
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

    | _ -> fail None "intro_calloc: the target should be a variable declarartion allocated with alloc")


(* [intro_mindex dim] expects the target [tg] to be pointing at a matrix declaration, then it will change
     all its occurrence accesses into Optitrust MINDEX accesses *)
let intro_mindex (dim : trm) : Target.Transfo.t = 
  iter_on_targets (fun t p ->
    let tg_trm = Path.get_trm_at_path p t in 
    match tg_trm.desc with 
    | Trm_let (_, (x, _), _) -> 
      Matrix_basic.intro_mindex dim [nbAny; cCellAccess ~base:[cVar x] ()]
    | _ -> fail tg_trm.loc "intro_mindex: the target should point to a  matrix declaration" 
  )



(* [intro_malloc tg] expects the target [tg] pointing to a variable declaration
    then it will check its body for a call to malloc. On this extended path it will call
    the basic intro_malloc transformation
*)
let intro_malloc : Transfo.t =
  iter_on_targets ( fun t p ->
    let tg_trm,_ = Path.resolve_path_and_ctx p t in
    match tg_trm.desc with
    | Trm_let (_, (x,_), init) ->
      begin match get_init_val init with
      | Some t1 ->
        begin match t1.desc with
        | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_cast _)));_},[malloc_trm]) ->
          begin match malloc_trm.desc with
          | Trm_apps ({desc = Trm_var (_, "malloc");_}, _) ->
            Matrix_basic.intro_malloc ((target_of_path p) @ [cFun "malloc"])
          | _ -> fail t1.loc "intro_malloc: couldn't find the call to malloc function"
          end
        | Trm_apps ({desc = Trm_var (_, "malloc");_},_) ->
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
    | _ -> fail None "intro_malloc: the target should be a variable declarartion allocated with alloc")


(* [biject fun_bij tg] expects the target [tg] to be pointing at a matrix declaration , then it will search for all the occurrences
    of the matrix access, and replace MINDEX function with [fun_bij]
*)
let biject (fun_bij : string) : Transfo.t =
  iter_on_targets (fun t p ->
    let tg_trm = Path.resolve_path p t in
    let path_to_seq, _ = Internal.isolate_last_dir_in_seq p in
    match tg_trm.desc with
    | Trm_let (_, (p, _), _) ->
      Expr.replace_fun fun_bij [nbAny; cCellAccess ~base:[cVar p] ~index:[cFun ""] (); cFun ~regexp:true "MINDEX."]
    | Trm_apps (_, [{desc = Trm_var (_, p)}; _])  when is_set_operation tg_trm -> 
      Expr.replace_fun fun_bij ((target_of_path path_to_seq) @ [nbAny; cCellAccess ~base:[cVar p] ~index:[cFun ""] (); cFun ~regexp:true "MINDEX."])
    | _ -> fail tg_trm.loc "biject: expected a variable declaration"

)

(* [intro_mops dims] expects the target [tg] pointing to an array declaration allocated with
      calloc or malloc, then it will apply intro_calloc or intor_mmaloc based on the type of
      the current allocation used. Then it will search for all accesses and apply intro_mindex
*)
let intro_mops (dim : trm) : Transfo.t =
  iter_on_targets (fun t p ->
    let tg_trm = Path.get_trm_at_path p t in
    match tg_trm.desc with 
    | Trm_let (_, (x, _), _) -> begin
        intro_mindex dim (target_of_path p);
        try intro_malloc (target_of_path p) with | TransfoError _ -> 
          begin 
            try intro_calloc (target_of_path p) with | TransfoError _ -> fail tg_trm.loc "intro_mops: the targeted matrix was not allocated with malloc or calloc" 
          end
        end
      
    | _ -> fail tg_trm.loc "intro_mops: the target should be pointing at a matrix declaration"
  
  )

(* [delocalize ~mark ~init_zero ~acc_in_place ~acc ~last ~var ~into ~dim ~index ~indices ~ops tg] this is a combi varsion of
  matrix_delocalize, this transformation first calls Matrix_basi.local_name to create the isolated environment where the delocalizing transformatino
  is going to be performed
*)
let delocalize ?(mark : mark option) ?(init_zero : bool = false) ?(acc_in_place : bool = false) ?(acc : string option)
  ?(last : bool = false)  ?(use : trm option = None) (var : var) ~into:(into : var) ~dim:(dim : trm)  ~index:(index : string)
  ?(indices : string list = []) ~ops:(ops : local_ops) ?(alloc_instr : target option) ?(labels : label list = []) ?(dealloc_tg : target option = None) (tg : target) : unit =

    let indices = match indices with | [] -> [] | _ as s_l -> s_l  in
    let middle_mark = match mark with | None -> Mark.next() | Some m -> m in
    let acc = match acc with | Some s -> s | _ -> "" in  Matrix_basic.local_name ~my_mark:middle_mark ~alloc_instr ~into ~indices ~local_ops:ops var tg;

    let any_mark = begin match use with | Some _ -> "any_mark_deloc" | _ -> "" end in
    Matrix_basic.delocalize ~init_zero ~acc_in_place ~acc ~any_mark ~dim ~index ~ops ~labels [cMark middle_mark];

    let tg_decl_access = cOr [[cVarDef into];[cCellAccess ~base:[cVar into] ()]] in
    if last then Matrix_basic.reorder_dims ~rotate_n:1 () [nbAny; cMark middle_mark; tg_decl_access ;cFun ~regexp:true "M\\(.NDEX\\|ALLOC\\)."] ;
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
    begin match mark with | None -> Marks.remove middle_mark [cMark middle_mark] | _ -> () end


(* [reorder_dims ~rotate_n ~order tg] expects the target [tg] to be pointing at a matrix declaration, then it will find the occurrences of ALLOC and INDEX functions
      and apply the reordering of the dimensions.
*)
let reorder_dims ?(rotate_n : int option) ?(order : int list = []) () (tg : target) : unit =
  let rotate_n = match rotate_n with Some n -> n | None -> 0  in
  iter_on_targets (fun t p ->
    let path_to_seq,_ = Internal.isolate_last_dir_in_seq p in
    let tg_trm = Path.resolve_path p t in
    match tg_trm.desc with
    | Trm_let (_, (x, _), _) ->
        Matrix_basic.reorder_dims ~rotate_n ~order () ((target_of_path path_to_seq) @ [cOr [[cVarDef x; cFun ~regexp:true "M.ALLOC."];[cCellAccess ~base:[cVar x] (); cFun ~regexp:true "MINDEX."]]])
    | _ -> fail tg_trm.loc "reorder_dims: expected a target to variable declaration"
  ) tg
