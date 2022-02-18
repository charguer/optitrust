open Ast

include Variable_basic

(* This type is used for variable renaming, the user can choose between renaming all the variables
    on one block, by giving the suffix to add after or he can also give the list of variables to
    be renamed where the list should be a a list of string pairs ex. (current_name, new_name).
*)
module Rename = struct
  type t =
  | AddSuffix of string
  | ByList of (string * string) list
  | Renamefn of (string -> string)

  let add_sufix (s : string) =  Renamefn (fun x -> x ^ s)

  let apply (f : string -> string) = Renamefn f 

  let bylist (l : (string * string) list) =  ByList l

end
type rename = Rename.t

let map f = function
| Rename.AddSuffix v -> Rename.AddSuffix (f v)
| Rename.ByList kvs -> Rename.ByList (List.map (fun (k,v) -> (k, f v)) kvs)
| Rename.Renamefn g -> Rename.Renamefn (fun x -> f (g x))


(* [fold ~at ~nonconst tg] expects [tg] to point to a variable declaration
    [at] - denotes a list of targets where the fold_lefting is done. If empty the
      fold_lefting operation is performed on all the ast nodes in the same level as the
      declaration or deeper, by default [at] = [].
    [nonconst] - denotes a flag to decide if fold_lefting should be done for variables which are
        not mutable, in general is not safe to fold variables which are not declared as const.
        But if the users know what they're doing then  they can use this flag to use fold_lefting
        also for mutable variables.
    This transformation
*)
let fold ?(at : Target.target = []) ?(nonconst : bool = false) (tg : Target.target) : unit =
  Target.iter_on_targets (fun t p ->
    let tg_trm = Path.resolve_path p t in
    match tg_trm.desc with
    | Trm_let (vk, (_, tx), _) ->
      let ty = get_inner_ptr_type tx in
      begin match ty.typ_desc with
      (* If the declared variable has a refernce type checking its mutability is not needed*)
      | Typ_ptr _ when trm_annot_has Reference tg_trm ->
        Variable_basic.fold ~at (Target.target_of_path p)
      (* In other cases we need to check the mutability of the variable *)
      | _ -> begin match vk with
            | Var_immutable -> Variable_basic.fold ~at (Target.target_of_path p)
            | _ -> if nonconst = true
                then Variable_basic.fold ~at (Target.target_of_path p)
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
let insert_and_fold ~name:(name : string) ~typ:(typ : typ) ~value:(value : trm) (tg : Target.target) : unit =
  Variable_basic.insert ~reparse:true ~name ~typ ~value tg;
  Variable_basic.fold [Target.cVarDef name]

(* [delocalize var ~into ~mark ~arr_size ~neutral_element fold_operation tg]
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
   (ov : var) ~into:(nv : var)
  ~array_size:(arrs : string) (tg : Target.target) : unit =
  let middle_mark = match mark with | None -> Mark.next () | Some m -> m in
  Variable_basic.local_name ~mark:middle_mark ov ~into:nv tg;
  Variable_basic.delocalize ~index ~array_size:arrs ~ops [Target.cMark middle_mark];
  begin
   match mark with | None -> Marks.remove middle_mark [Target.cMark middle_mark] | _ -> ()
  end

(* [delocalize ~var ~into ~index ~mark ~ops ~array_size ~intos tg]
    It's a continuation to the delocalize transformation which will unroll all the introduced loops
    from the basic delocalize transformation and convert the newly declared array to a list of variables
    namely for each index on variable, this variables should be given by the user through the labelled
    argument [vars].
*)
let delocalize_in_vars ?(index : string = "dl_i") ?(mark : mark = "section_of_interest") ?(ops : delocalize_ops = Delocalize_arith (Lit_int 0, Binop_add) )
   (ov : var) ~into:(nv : var)  ~array_size:(arrs : string)
  ~local_vars:(lv : vars) (tg : Target.target) : unit =
  Variable_basic.local_name ~mark ov ~into:nv tg;
  Variable_basic.delocalize ~index ~array_size:arrs ~ops [Target.cMark mark];
  Variable_basic.unfold ~at:[Target.cFor index] [Target.nbAny;Target.cVarDef arrs];
  Loop_basic.unroll ~braces:false [Target.nbMulti ;Target.cFor index];
  Arrays.to_variables  lv [Target.cVarDef nv];
  Marks.remove "section_of_interest" [Target.cMark "section_of_interest"]

let intro_pattern_array ?(pattern_aux_vars : string = "") ?(const : bool = false) ~pattern_vars:(pattern_vars : string ) ~pattern:(pattern : string) (tg : Target.target) : unit =
  Trace.call (fun t ->
  (* Temporary hack till Arthur enables the usage of the new parser *)
  let str = pattern_vars ^ " ==>" ^ pattern_aux_vars ^ " ==> " ^ pattern in
  let minimal_index = ref 10000 in
  let (pattern_vars, pattern_aux_vars, pattern_instr) = Trm_matching.parse_pattern str in
  let path_to_surrounding_seq = ref [] in
  let paths = Target.resolve_target tg t in
  List.iteri (fun _i p ->
    let path_to_seq, _ , index  = Internal.get_instruction_in_surrounding_sequence p in
    if !path_to_surrounding_seq = [] then path_to_surrounding_seq := path_to_seq
      else if !path_to_surrounding_seq <> path_to_seq then fail None "intro_patter_array: all the targeted instuctions should belong to the same englobing sequence";
    if index < !minimal_index then minimal_index := index;
  ) paths;
  let nb_paths = List.length paths in
  let nb_vars = List.length pattern_vars in
  let all_values = Array.make_matrix nb_vars nb_paths (trm_unit ()) in
  Target.iteri_on_targets (fun id_path _ p ->
    let inst = Trm_matching.rule_match (pattern_vars @ pattern_aux_vars) pattern_instr (Target.get_trm_at (Target.target_of_path p)) in
    let values = Trm_matching.tmap_to_list pattern_vars (Trm_matching.tmap_filter_keys pattern_vars inst) in
    List.iteri (fun id_var v -> all_values.(id_var).(id_path) <- v) values;
    let inst = List.map (fun (x, _) -> get_array_access (trm_var_get x) (trm_int id_path)) pattern_vars in 
    let new_inst = Trm_map.empty in
    let new_inst = List.fold_left2 (fun acc (x, _) y -> Trm_map.add x y acc) new_inst pattern_vars inst in
    let new_t = Internal.subst new_inst pattern_instr in
    Target.apply_on_targets (fun t p -> Target.apply_on_path (fun _ -> new_t) t p) (Target.target_of_path p)
  ) tg;
  let vk = if const then Var_immutable else Var_mutable in
  let instrs_to_insert = List.mapi (fun id_var (x, _) -> trm_let_array vk (x, typ_double ()) (Const nb_paths) (trm_array (Mlist.of_list (Array.to_list all_values.(id_var))))) pattern_vars in
  Internal.nobrace_remove_after (fun _ ->
    Sequence_basic.insert (trm_seq_no_brace instrs_to_insert) ([Target.tBefore] @ (Target.target_of_path !path_to_surrounding_seq) @ [Target.dSeqNth !minimal_index])))

(* [detach_if_needed tg] expects the target [tg] to be pointing at a variable declaration, then it will check if it s
    already initialized or not, if that is the case than it will deatch that declaration, otherwise no change is applied*)
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
        Variable_basic.subst ~subst:x ~put:space (Target.target_of_path _path_to_seq)
      | None -> fail decl_t.loc "reuse: could not match the declaration"
      end
      ))
(* [renames rename tg] expects [tg] to point to a sequence.
    [rename] can be either ByList l where l denotes a list of pairs where
    each pair has the current variable and the one which is going to replace it.
    Or AddSuffix s, if this is the case then all the variable declared inside the targeted sequence
     are going to be renamed by adding the suffix at the end of its current name.
*)
let renames (rename : rename) : Target.Transfo.t = 
  Target.iter_on_targets (fun t p -> 
    let tg_trm = Path.resolve_path p t in 
    match tg_trm.desc with 
    | Trm_seq tl -> 
      let decl_vars = List.map decl_name (Mlist.to_list tl) in 
      let decl_vars = List.filter_map (fun d -> d) decl_vars in 
      let new_decl_vars = begin match rename with 
      | AddSuffix s -> 
        List.map (fun d -> d ^ s) decl_vars
      | Renamefn g -> 
        List.map g decl_vars
      | ByList l -> 
        List.map (fun d -> 
          begin match List.assoc_opt d l with 
          | Some d1 -> d1
          | _ -> d
          end) decl_vars 
      
      end in 
      List.iter2 (fun d into -> Variable_basic.rename ~into ((Target.target_of_path p) @  [Target.cVarDef d])) decl_vars new_decl_vars

    | _ -> fail tg_trm.loc "renames: the target should be pointing at a sequence" )


(* VERY MUCH LATER: do some checks
       at the basic level: apply inlining
       at the high level:
       - always ok to inline arithmetic expressions (possibly hidden behind functions)
       - operations with potential side effects should not be duplicated
         except if there is a single occurrence of the variable
         AND this occurence is not beyond other interacting side effects

      const int a = x++;
      int b = a;
      int c = a;
      => not safe to inline a

      const int a = x++;
      int b = a;
      => safe to inline a

      const int a = x++;
      int c = y;
      int b = a;
      => safe to inline a

      const int a = x++;
      int c = x++;
      int b = a;
      => not safe to inline a

      const int a = x++;
      int b = f(x++,a);
      => not safe to inline a
    *)
(* [inline ~accept_functions ~simple_deref ~delete tg] expects the target [tg] to be pointing at a variable declaration.
    If the variable has a struct type then a mark is created and passed as an argument to Variable_basic.unfold
      on the next step, otherwise no mark needs to be created.
      We consider the following cases:
      1) If the targeted variable is mutable then we try to make it immutable by calling Variable.to_const.
         WARNING: This step will fail in the case when there are any write operations on the targetd varibles.
      2) If the transformation didn't fail in the first step we are sure that we are trying to inline a const variable
         and we can call safely Variable_basic.unfold
      3) If the targeted variable is a struct type then call Struct_basic.simpl_proj to remove all the occurrences
          of struct initialization list, by projecting them on the field they are accessed.
          Ex: int v = {0,1} if we had v.x then Variable_basic.inline will transform it to {0, 1}.x which is non valid C code.
          After calling Struct_basic.simpl_proj {0, 1}.x becomes 0 .
          Finally, if simple_deref is set to true then we will seach for all the occurrences of *& and &* and simplify them.
*)
let inline ?(accept_functions : bool = false) ?(simpl_deref : bool = false) ?(delete : bool = true): Target.Transfo.t =
  Target.iter_on_targets (fun t p ->
    let tg_seq_path, _ = Internal.isolate_last_dir_in_seq p in
    let seq = Target.target_of_path tg_seq_path in
    let tg_trm = Path.resolve_path p t in
    let mark = Mark.next () in
    match tg_trm.desc with
    | Trm_let (vk, (x, _tx), _init) ->

      (* LATER: this test is incorrect; we need to check if the body is a trm_struct or trm_array, not if it has struct type ;
        for the moment, let's use the mark all the time

        let var_type = get_inner_ptr_type tx in
        let mark = if (Internal.is_struct_type var_type) then mark else "" in *)
      begin match vk with
      | Var_immutable ->
        if delete
          then Variable_basic.inline ~mark ~accept_functions (seq @ [Target.cVarDef x])
          else Variable_basic.unfold ~mark ~accept_functions (seq @ [Target.cVarDef x])
      | Var_mutable ->
        Variable_basic.to_const (seq @ [Target.cVarDef x]);
        if delete
          then Variable_basic.inline ~mark ~accept_functions (seq @ [Target.cVarDef x])
          else Variable_basic.unfold ~mark ~accept_functions (seq @ [Target.cVarDef x])
      end;
     if mark <> "" then begin
       Struct_basic.simpl_proj [Target.nbAny; Target.cFieldAccess ~base:[Target.cMark mark] ()];
       if simpl_deref
         then Variable_basic.simpl_deref [Target.nbAny; Target.cRead ~addr:[Target.cMark mark] ()];
       Marks.remove mark [Target.nbAny; Target.cMark mark]
     end
    | _ -> fail t.loc "inline: expected a target to a variable declaration"
  )


(* [inline_and_rename tg] expects the target [tg] to be poiting at a variable declaration with an initial value
    being another variable occurrence. Then it will inline y on all its occurrenes which belong to the
    same scope. Then it will rename the variable x to y, both in the declaration and its occurrences


    Assumption:
      if the target [tg] points to the following instruction int y = x; then
      no occurrence of x appears after that instruction
*)

let inline_and_rename : Target.Transfo.t =
  Target.iter_on_targets( fun t p ->
    let tg_trm  = Path.resolve_path p t in
    let path_to_seq, _ = Internal.isolate_last_dir_in_seq p in
    match tg_trm.desc with
    | Trm_let (_, (y,_), init) ->
      begin match get_init_val init with
      | Some v ->
        let x =
        begin match v.desc with
        | Trm_var (_, x) -> x
        | Trm_apps (_, [v1]) when is_get_operation v ->
          begin match v1.desc with
          | Trm_var (_, x) -> x
          | _ -> ""
          end
        | _ -> ""
        end in
         if x <> "" then
          inline ~delete:true [Target.cVarDef y];
          renames (ByList [(x,y)]) (Target.target_of_path path_to_seq)
      | _ -> fail init.loc "inline_and_rename: expected an initialized variable declaration"
      end
    | _ -> fail t.loc "inline_and_rename: expected the declaration of the variable which is goingg to be reverse folded"
)

(* [elim_redundant ~source tg] expets the target [tg] to be pointing to a variable declaration with an initial value being
    the same as the variable declaration which [source] points to. Then it will fold the variable in [source] into
    the varibale declaration [tg] and inline the declaration in [tg]
*)
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
        inline ~delete:true ((Target.target_of_path path_to_seq) @ [Target.cVarDef x])
    | _ -> fail tg_trm.loc "elim_redundant: the main target should point to the declaration of the variable you want to eliminate"

  )


(* [insert ~constr name typ value tg] expects the target [tg] to point to a location in a sequence
    then it wil insert a new variable declaration with name [name] type [typ] and initialization value [value].
    This transformation is basically the same as the basic one except that this has a default value for the type argument.
*)
let insert ?(const : bool = false) ?(reparse : bool = false) ?(typ : typ = typ_auto ()) ~name:(name : string) ~value:(value : trm) : Target.Transfo.t =
 Variable_basic.insert ~const ~reparse ~name ~typ ~value

(* [insert_list ~const names typ values tg] expects the target [tg] to be poiting to a location in a sequence
    then it wil insert a new variable declaration with name [name] type [typ] and initialization value [value]
*)
let insert_list ?(reparse : bool = false) ~defs:(defs : (string * string * trm ) list) : Target.Transfo.t =
  let defs = List.rev defs in 
  Target.reparse_after ~reparse (fun tg ->
    List.iter (fun (typ, name, value) ->
      (* This check is needed to avoid the parentheses in the case when the value of the vairbale is a simple expression  *)
      insert ~name ~typ:(AstParser.atyp typ) ~value tg) defs
)
