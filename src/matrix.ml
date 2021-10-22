open Ast
include Matrix_basic


(* [intro_mcalloc tg] expects the target [tg] pointing to a variable declaration  
    then it will check its body for a call to calloc. On this extended path it will call
    the basic intro_mcalloc transformation
*)
let intro_mcalloc : Target.Transfo.t = 
  Target.iter_on_targets ( fun t p ->
    let tg_trm,_ = Path.resolve_path p t in
    match tg_trm.desc with 
    | Trm_let (_, (_,_), init) ->
      begin match get_init_val init with
      | Some t1 ->
        begin match t1.desc with 
        | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_cast _)));_},[calloc_trm]) ->
          begin match calloc_trm.desc with
          | Trm_apps ({desc = Trm_var "calloc";_}, _) ->
            Matrix_basic.intro_mcalloc ((Target.target_of_path p) @ [Target.cFun "calloc"])
          | _ -> fail t1.loc "intro_mcalloc: couldn't find the call to calloc function"
          end
        | Trm_apps ({desc = Trm_var "calloc";_},_) ->
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
    let tg_trm,_ = Path.resolve_path p t in
    match tg_trm.desc with 
    | Trm_let (_, (_,_), init) ->
      begin match get_init_val init with
      | Some t1 ->
        begin match t1.desc with 
        | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_cast _)));_},[malloc_trm]) ->
          begin match malloc_trm.desc with
          | Trm_apps ({desc = Trm_var "malloc";_}, _) ->
            Matrix_basic.intro_mmalloc ((Target.target_of_path p) @ [Target.cFun "malloc"])
          | _ -> fail t1.loc "intro_mmalloc: could not find a call to malloc function"
          end
        | Trm_apps ({desc = Trm_var "malloc";_},_) ->
          Matrix_basic.intro_mmalloc ((Target.target_of_path p) @ [Target.cFun "malloc"])
        | _ -> fail t1.loc "intro_mmalloc: couldn't find a call to malloc function"
        end
      | _ -> fail None "intro_mmalloc: the targeted variable should be initialized"
      end
    | _ -> fail None "intro_mmalloc: the target should be a variable declarartion allocated with alloc")


(* [biject fun_bij tg] expects the target [tg] to point to a function call to MINDEX 
      then it will replace its name with [fun_bij]
*)
let biject (fun_bij : string) : Target.Transfo.t = 
  Instr_basic.replace_fun fun_bij



(* [intro_mops dims] expects the target [tg] pointing to an array declaration allocated with
      calloc or malloc, then it will apply intro_mcalloc or intor_mmaloc based on the type of 
      the current allocation used. Then it will search for all accesses and apply intro_mindex 
*)
let intro_mops (dim : trm) : Target.Transfo.t =
  Target.iter_on_targets (fun t p ->
    let tg_trm, _ = Path.resolve_path p t in
    match tg_trm.desc with
    | Trm_let (_, (x,_), init) ->
      begin match get_init_val init with
      | Some t1 ->
        begin match t1.desc with 
        | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_cast _)));_},[malloc_trm]) ->
          begin match malloc_trm.desc with
          | Trm_apps ({desc = Trm_var "calloc";_}, _) ->
            intro_mcalloc [Target.cVarDef x];
            Matrix_basic.intro_mindex dim [Target.nbAny;Target.cCellAccess ~base:[Target.cVar x] ~index:[]]
          | Trm_apps ({desc = Trm_var "malloc";_}, _) ->
            Matrix_basic.intro_mmalloc ((Target.target_of_path p) @ [Target.cFun "malloc"]);
            Matrix_basic.intro_mindex dim [Target.nbAny;Target.cCellAccess ~base:[Target.cVar x] ~index:[]]
          | _ -> fail t1.loc "intro_mops: couldn't find a call to calloc/malloc function"
          end
        | Trm_apps ({desc = Trm_var "calloc";_}, _) ->
            Matrix_basic.intro_mcalloc ((Target.target_of_path p) @ [Target.cFun "calloc"]);
            Matrix_basic.intro_mindex dim [Target.nbAny;Target.cCellAccess ~base:[Target.cVar x] ~index:[]]
        | Trm_apps ({desc = Trm_var "malloc";_}, _) ->
            Matrix_basic.intro_mmalloc ((Target.target_of_path p) @ [Target.cFun "malloc"]);
            Matrix_basic.intro_mindex dim [Target.nbAny;Target.cCellAccess ~base:[Target.cVar x] ~index:[]]
        | _ -> fail t1.loc "intro_mmalloc:"
        end
      | _ -> fail None "intro_mmalloc: the targeted variable should be initialized"
      end
    | _ -> fail None "intro_mmalloc: the target should be a variable declarartion allocated with alloc")

