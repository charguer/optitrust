open Ast
include Variable_core.Rename
include Variable_basic

(* [fold ~as_reference ~at ~nonconst tg] expects [tg] to point to a variable declaration
    [as_reference] - denotes a flag whether the declaration initialization contains a
      variable reference or not.
    [at] - denotes a list of targets where the fold_lefting is done. If empty the
      fold_lefting operation is performed on all the ast nodes in the same level as the
      declaration or deeper, by default [at] = [].
    [nonconst] - denotes a flag to decide if fold_lefting should be done for variables which are
        not mutable, in general is not safe to fold variables which are not declared as const.
        But if the users know what they're doing then  they can use this flag to use fold_lefting
        also for mutable variables.
    This transformation
*)
let fold ?(as_reference : bool = false) ?(at : Target.target = []) ?(nonconst : bool = false) (tg : Target.target) : unit =
  Target.iter_on_targets (fun t p ->
    let tg_trm = Path.resolve_path p t in
    match tg_trm.desc with
    | Trm_let (vk, (_, tx), _) ->
      let ty = get_inner_ptr_type tx in
      begin match ty.typ_desc with
      (* If the declared variable has a refernce type checking its mutability is not needed*)
      | Typ_ptr {ptr_kind = Ptr_kind_ref;_} ->
        Variable_basic.fold ~as_reference ~at (Target.target_of_path p)
      (* In other cases we need to check the mutability of the variable *)
      | _ -> begin match vk with
            | Var_immutable -> Variable_basic.fold ~as_reference ~at (Target.target_of_path p)
            | _ -> if nonconst = true
                then Variable_basic.fold ~as_reference ~at (Target.target_of_path p)
                else
                  fail tg_trm.loc "fold: if you want to use fold_lefting for mutable variables you should set
                            ~nonconst to true when calling this transformation"
            end
      end

    | _ -> fail tg_trm.loc "fold: expected a variable declaration"
) tg

(* [insert_and_fold] expects [tg] to point to relative location, then it inserts a new variable declaration at that location.
    The new declared variable is [name] with typ [typ] and value [value]. This variable will be folded everywhere on the ast nodes
    which come after the declared variable.
*)
let insert_and_fold ~name:(name : string) ~typ:(typ : string) ~value:(value : trm) (tg : Target.target) : unit =
  Variable_basic.insert ~reparse:true ~name ~typ ~value tg;
  Variable_basic.fold [Target.cVarDef name]

(* [delocalize ~var_type ~var ~local_var ~mark ~arr_size ~neutral_element fold_operation tg]
    expects the target [tg] to point to a for loop. Then it will surround this loop with a @nobrace
    sequence. After that it will apply another transformation called local other name. Which as the name
    suggests it will declare a new variable inside the targeted block and replace the current one with t he new one.
    Finally a last instruction is added to save all the changes to the old variable. Now the stage is
    ready to apply delocalize which basically declares a new array oof size [array_size]. Then it will
    transform the old loop into a parallel loop. And finally a reduction is done to save the result into
    the old variable.

    Ex:
    int ANY(int maxValue){ return 0;}

    const int N = 2;
    typedef int T;

    int main(){
      T a;
      for (int i = 0; i < N; i++){
         a++;
      }
      int y = 0;
      return 0;
    }
    STEP1:
      Introduce a new local name x for variable a and replace all its occurrences inside
      the section of interest which can be any ast node. In this particular case it is
      a for loop.

    int main() {
      T a;
      /*@section_of_interest*/ T x = a;
      for (int i = 0; (i < N); i++) {
        x++;
      }
      a = x; /*section_of_interest@*/
      int y = 0;
      return 0;
    }
    STEP 2:
      Apply_ the basic delocalize transformation
     int main() {
        T a;
        /*@section_of_interest*/ T x[N];
        x[0] = a;
        for (int dl_k = 1; (dl_k < N); dl_k++) {
          x[dl_k] = 0;
        }
        for (int i = 0; (i < N); i++) {
          x[ANY(N)]++;
        }
        a = x[0];
        for (int dl_k = 1; (dl_k < N); dl_k++) {
          a += x[dl_k];
        } /*section_of_interest@*/
        int y = 0;
        return 0;
    }
*)

let delocalize ?(index : string = "dl_i") ?(mark : mark option) ?(ops : delocalize_ops = Delocalize_arith (Lit_int 0, Binop_add) )
   ~var:(ov : var) ~local_var:(nv : var)  ~var_type:(vt : typ)
  ~array_size:(arrs : string) (tg : Target.target) : unit =
  let middle_mark = match mark with | None -> Mark.next () | Some m -> m in
  Variable_basic.local_name ~mark:middle_mark ~var_type:vt ~var:ov ~local_var:nv tg;
  Variable_basic.delocalize ~index ~array_size:arrs ~ops [Target.cMark middle_mark];
  begin
   match mark with | None -> Marks.remove middle_mark [Target.cMark middle_mark] | _ -> ()
  end

(* [delocalize ~index ~mark ~ops ~var ~local_var ~var_type ~array_size ~local_vars tg]
    It's a continuation to the delocalize transformation which will unroll all the introduced loops
    from the basic delocalize transformation and convert the newly declared array to a list of variables
    namely for each index on variable, this variables should be given by the user through the labelled
    argument [vars].
*)
let delocalize_in_vars ?(index : string = "dl_i") ?(mark : mark = "section_of_interest") ?(ops : delocalize_ops = Delocalize_arith (Lit_int 0, Binop_add) )
   ~var:(ov : var) ~local_var:(nv : var)  ~var_type:(vt : typ)
  ~array_size:(arrs : string) ~local_vars:(lv : vars) (tg : Target.target) : unit =
  Variable_basic.local_name ~mark ~var_type:vt ~var:ov ~local_var:nv tg;
  Variable_basic.delocalize ~index ~array_size:arrs ~ops [Target.cMark mark];
  Variable_basic.inline_at [Target.cFor index] [Target.nbAny;Target.cVarDef arrs];
  Loop_basic.unroll ~braces:false [Target.nbMulti ;Target.cFor index];
  Arrays.to_variables  lv [Target.cVarDef nv];
  Marks.remove "section_of_interest" [Target.cMark "section_of_interest"]

let intro_pattern_array ?(pattern_aux_vars : string = "") ~pattern_vars:(pattern_vars : string ) ~pattern:(pattern : string) (tg : Target.target) : unit =
  Trace.call (fun t ->
  (* Temporary hack till Arthur enables the usage of the new parser *)
  let str = pattern_vars ^ " ==>" ^ pattern_aux_vars ^ " ==> " ^ pattern in
  let (pattern_vars, pattern_aux_vars, pattern_instr) = Rewrite_core.parse_pattern str in
  let path_to_surrounding_seq = ref [] in
  let paths = Target.resolve_target tg t in
  List.iteri (fun _i p ->
    let path_to_seq, _ , _index  = Internal.get_instruction_in_surrounding_sequence p in
    if !path_to_surrounding_seq = [] then path_to_surrounding_seq := path_to_seq
      else if !path_to_surrounding_seq <> path_to_seq then fail None "intro_patter_array: all the targeted instuctions should belong to the same englobing sequence";
  ) paths;
  let nb_paths = List.length paths in
  let nb_vars = List.length pattern_vars in
  let all_values = Array.make_matrix nb_vars nb_paths (trm_unit ()) in
  Target.iteri_on_targets (fun id_path _ p ->
    let inst = Rewrite_core.rule_match (pattern_vars @ pattern_aux_vars) pattern_instr (Target.get_trm_at (Target.target_of_path p)) in
    let values = Rewrite_core.tmap_to_list pattern_vars (Rewrite_core.tmap_filter_keys pattern_vars inst) in
    List.iteri (fun id_var v -> all_values.(id_var).(id_path) <- v) values;
    let inst = List.map (fun  x -> trm_apps (trm_binop (Binop_array_cell_addr))[trm_var x; trm_int id_path]) pattern_vars in
    let new_inst = Trm_map.empty in
    let new_inst = List.fold_left2 (fun acc x y -> Trm_map.add x y acc) new_inst pattern_vars inst in
    let new_t = Internal.subst new_inst pattern_instr in
    Target.apply_on_targets (fun t p -> Target.apply_on_path (fun _ -> new_t) t p) (Target.target_of_path p)
  ) tg;
  let instrs_to_insert = List.mapi (fun id_var x -> trm_let Var_mutable (x, typ_ptr Ptr_kind_mut (typ_array (typ_double ()) (Const nb_paths)) ~typ_attributes:[GeneratedStar])
  (trm_apps (trm_prim (Prim_new (typ_array (typ_double ()) (Const nb_paths)))) [trm_array (Mlist.of_list (Array.to_list all_values.(id_var)))])) pattern_vars in
  Internal.nobrace_remove_after (fun _ ->
    Sequence_basic.insert (trm_seq_no_brace instrs_to_insert) ([Target.tFirst] @ (Target.target_of_path !path_to_surrounding_seq))))


let detach_if_needed (tg : Target.target) : unit =
  Target.iter_on_targets (fun t p ->
    let decl_t  = Path.resolve_path p t in
    match decl_t.desc with
    | Trm_let(vk,(_, _), init) ->
      begin match vk with
      | Var_immutable -> ()
      | _ ->
        begin match init.desc with
        | Trm_apps (_,[_init]) ->
          Variable_basic.init_detach (Target.target_of_path p)
        | _ -> ()
        end
      end
    | _ -> fail t.loc "init_detach_aux: variable could not be matched, make sure your path is correct"
  ) tg

(* [reuse ~reparse space tg] expects the target [tg] to be poiting to a variable declaration, then it will 
    remove that declaration and replace all its occurrences with [space]

*)
let reuse ~space:(space : trm) ?(reparse : bool = false) : Target.Transfo.t = 
  Target.reparse_after ~reparse (Target.iter_on_targets (fun t p -> 
      let decl_t = Path.resolve_path p t in
      begin match decl_name decl_t with
      | Some x ->
        let _path_to_seq, _,_ = Internal.get_instruction_in_surrounding_sequence p in
        Marks.add "reuse_mark" (Target.target_of_path p);
        detach_if_needed [Target.cMark "reuse_mark"];
        Instr_basic.delete [Target.cMark "reuse_mark"];
        Variable_basic.replace_occurrences ~subst:x ~put:space (Target.target_of_path _path_to_seq);
      | None -> fail decl_t.loc "reuse: could not match the declaration"
      end
      )) 

(* [reverse_fold tg] expects the target [tg] poiting to a variable declaration with an initial value
    being another variable occurrence. Then it will inline y on all its occurrenes which belong to the
    same scope. Then it will rename the variable x to y, both in the declaration and its occurrences
*)

let reverse_fold : Target.Transfo.t =
  Target.iter_on_targets( fun t p ->
    let tg_trm  = Path.resolve_path p t in
    let path_to_seq, _ = Internal.isolate_last_dir_in_seq p in
    match tg_trm.desc with
    | Trm_let (_, (y,_), init) ->
      begin match get_init_val init with
      | Some v ->
        let x =
        begin match v.desc with
        | Trm_var x -> x
        | Trm_apps (_, [v1]) when is_get_operation v ->
          begin match v1.desc with
          | Trm_var x -> x
          | _ -> ""
          end
        | _ -> ""
        end in
         if x <> "" then
          Variable_basic.inline [Target.cVarDef y];
          Variable_basic.rename_on_block (ByList [(x,y)]) (Target.target_of_path path_to_seq)
      | _ -> fail init.loc "reverse_fold: expected an initialized variable declaration"
      end
    | _ -> fail t.loc "reverse_fold: expected the declaration of the variable which is goingg to be reverse folded"
)

(* [elim_redundant ~source tg] expets the target [tg] pointing to a variable declaration with an initial value being
    the same as the variable declaration which [source] points to. Then it will fold the variable in [source] into
    the varibale declaration [tg] and inline the declaration in [tg]
   NOTE: This version works only if source was provided otherwise it will throw an error *)

let elim_redundant ?(source : Target.target = []) : Target.Transfo.t = 
  Target.iteri_on_targets (fun i t p -> 
    let tg_trm = Path.resolve_path p t in
    let path_to_seq, index = Internal.isolate_last_dir_in_seq p in
    let seq_trm = Path.resolve_path path_to_seq t in
    match tg_trm.desc with 
    | Trm_let (_, (x, _), init_x) -> 
      let source_var = ref "" in
      if source = [] 
        then
          begin match seq_trm.desc with 
          | Trm_seq tl -> 
            Mlist.iteri (fun i t1 -> 
            if i >= index then () 
                else 
                begin match t1.desc with 
                | Trm_let (_, (y, _), init_y) when Internal.same_trm init_x init_y -> 
                  source_var := y
                | _ -> ()
                end
            ) tl
          | _ -> fail t.loc "elim_redundant: couldn't find the englobing sequence, try providing the source argument to solve this problem"
          end
       else 
        begin 
          let source_paths = Target.resolve_target source t in
          let source_decl_trm = match List.nth_opt source_paths i with
            | Some p -> Path.resolve_path p t
            | None -> fail t.loc "elim_redundant: the number of source targets  should be equal to the number of the main targets" in
          match source_decl_trm.desc with 
          | Trm_let (_, (y, _), init_y) when Internal.same_trm init_x init_y -> 
            source_var := y
          | _ -> fail source_decl_trm.loc "elim_redundant: the soource target should point to a variable declaration"
        end;
        Variable_basic.fold ~at:([Target.cVarDef x]) ((Target.target_of_path path_to_seq) @ [Target.cVarDef !source_var]);
        Variable_basic.inline ((Target.target_of_path path_to_seq) @ [Target.cVarDef x])
    | _ -> fail tg_trm.loc "elim_redundant: the main target should point to the declaration of the variable you want to eliminate"
  
  )


(* [insert_list ~const names typ values tg] expects the target [tg] to be poiting to a location in a sequence
    then it wil insert a new variable declaration with name [name] type [typ] and initialization value [value]
*)
let insert_list ?(reparse : bool = false) ~defs:(defs : (string * string * string ) list) : Target.Transfo.t =
  Target.reparse_after ~reparse (fun tg -> 
    List.iter (fun (typ, name, value) -> Variable_basic.insert ~name ~typ ~value:(Target.lit value) tg) defs
)

