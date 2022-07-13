open Ast
open Target

include Variable_basic

(* [Rename]: this type is used for variable renaming, the user can choose between renaming all the variables
    on one block, by giving the suffix to add or he can also give the list of variables to
    be renamed where the list should be a a list of string pairs ex. (current_name, new_name). *)
module Rename = struct
  type t =
  | AddSuffix of string
  | ByList of (string * string) list
  | Renamefn of (string -> string)

  let add_sufix (s : string) =  Renamefn (fun x -> x ^ s)

  let apply (f : string -> string) = Renamefn f

  let bylist (l : (string * string) list) =  ByList l

end

(* [rename]: instantiation of [Rename] *)
type rename = Rename.t

(* [map_f]: applies function [f ) inside the rename type *)
let map f = function
| Rename.AddSuffix v -> Rename.AddSuffix (f v)
| Rename.ByList kvs -> Rename.ByList (List.map (fun (k,v) -> (k, f v)) kvs)
| Rename.Renamefn g -> Rename.Renamefn (fun x -> f (g x))


(* [fold ~at ~nonconst tg]: similar to [Variable_basic.fold] but this one can be forced
     to non-const variables by setting [nonconst] flag to true.
    @correctness: The folded expression should have no observable side-effect.
    Moreover, the expression should produce the same value as when it was
    evaluated the first time.
    Exists r such that for initialization and all replacement points we have
    { H } expr { fun r' => [r' = r] * H } with H beeing the local invariant

    NOTE: if applied on non const variable, the variable must additionaly not have
    been mutated between the replacement point and its initialization. *)
let fold ?(at : target = []) ?(nonconst : bool = false) (tg : target) : unit =
  iter_on_targets (fun t p ->
    let tg_trm = Path.resolve_path p t in
    match tg_trm.desc with
    | Trm_let (vk, (_, tx), _) ->
      let ty = get_inner_ptr_type tx in
      begin match ty.typ_desc with
      (* If the declared variable has a refernce type checking its mutability is not needed*)
      | Typ_ptr _ when trm_has_cstyle Reference tg_trm ->
        Variable_basic.fold ~at (target_of_path p)
      (* In other cases we need to check the mutability of the variable *)
      | _ -> begin match vk with
            | Var_immutable -> Variable_basic.fold ~at (target_of_path p)
            | _ -> if nonconst = true
                then Variable_basic.fold ~at (target_of_path p)
                else
                  fail tg_trm.loc "Variable.fold: if you want to use fold for mutable variables you should set
                            ~nonconst to true when calling this transformation"
            end
      end
    | _ -> fail tg_trm.loc "Variable.fold: expected a variable declaration"
) tg

(* [insert_and_fold]: expects the target [tg] to point at a relative location, then it inserts a new variable
     declaration at that location. The new declared variable is [name] with [typ] and [value].
     This variable will be folded on all the ast nodes that come after the declared variable. *)
let insert_and_fold ~name:(name : string) ~typ:(typ : typ) ~value:(value : trm) (tg : target) : unit =
  Variable_basic.insert ~reparse:true ~name ~typ ~value tg;
  Variable_basic.fold [cVarDef name]

(* [delocalize var ~into ~mark ~arr_size ~neutral_element fold_operation tg]:
    expects the target [tg] to point at a for loop. Then it will surround this loop with a @nobrace
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
    } *)

let delocalize ?(index : string = "dl_i") ?(mark : mark option) ?(ops : local_ops = Local_arith (Lit_int 0, Binop_add) )
   (ov : var) ~into:(nv : var)
  ~array_size:(arrs : string) (tg : target) : unit =
  let middle_mark = match mark with | None -> Mark.next () | Some m -> m in
  Variable_basic.local_name ~mark:middle_mark ov ~into:nv tg;
  Variable_basic.delocalize ~index ~array_size:arrs ~ops [cMark middle_mark];
  begin
   match mark with | None -> Marks.remove middle_mark [cMark middle_mark] | _ -> ()
  end

(* [delocalize ~var ~into ~index ~mark ~ops ~array_size ~intos tg]: it's a continuation to the [delocalize] transformation
    that will unroll all the introduced loops from the basic delocalize transformation and convert the newly declared array
    to a list of variables namely for each index on variable, this variables should be given by the user through the labelled
    argument [vars]. *)
let delocalize_in_vars ?(index : string = "dl_i") ?(mark : mark = "section_of_interest") ?(ops : local_ops = Local_arith (Lit_int 0, Binop_add) )
   (ov : var) ~into:(nv : var)  ~array_size:(arrs : string)
  ~local_vars:(lv : vars) (tg : target) : unit =
  Variable_basic.local_name ~mark ov ~into:nv tg;
  Variable_basic.delocalize ~index ~array_size:arrs ~ops [cMark mark];
  Variable_basic.unfold ~at:[cFor index] [nbAny;cVarDef arrs];
  Loop_basic.unroll ~braces:false [nbMulti ;cFor index];
  Arrays.to_variables  lv [cVarDef nv];
  Marks.remove "section_of_interest" [cMark "section_of_interest"]


(* [intro_pattern_array ~pattern_aux_vars ~const ~pattern_vars ~pattern tg]: expects the target [tg] to be
     pointing to expressions of the form [pattern], then it will create an array of coefficients for each
    [pattern_vars] and replace the current coefficients with array accesses. *)
let intro_pattern_array ?(pattern_aux_vars : string = "") ?(const : bool = false) ~pattern_vars:(pattern_vars : string ) ~pattern:(pattern : string) (tg : target) : unit =
  Trace.call (fun t ->
  (* Temporary hack till Arthur enables the usage of the new parser *)
  let str = pattern_vars ^ "==>" ^ pattern_aux_vars ^ "==> " ^ pattern in
  let minimal_index = ref 10000 in (* 10000 is considered as infinity, we assume here that there will no block of code with 10k instructions *)
  let (pattern_vars, pattern_aux_vars, pattern_instr) = Trm_matching.parse_pattern str in
  let path_to_surrounding_seq = ref [] in
  let paths = resolve_target_with_stringreprs_available tg t in
  List.iteri (fun _i p ->
    let path_to_seq, _ , index  = Internal.get_instruction_in_surrounding_sequence p in
    if !path_to_surrounding_seq = [] then path_to_surrounding_seq := path_to_seq
      else if !path_to_surrounding_seq <> path_to_seq then fail None "Variable.intro_patter_array: all the targeted instuctions should belong to the same englobing sequence";
    if index < !minimal_index then minimal_index := index;
  ) paths;
  let nb_paths = List.length paths in
  let nb_vars = List.length pattern_vars in
  let all_values = Array.make_matrix nb_vars nb_paths (trm_unit ()) in
  iteri_on_targets (fun id_path _ p ->
    let inst = Trm_matching.rule_match (pattern_vars @ pattern_aux_vars) pattern_instr (Tools.unsome (get_trm_at (target_of_path p))) in
    let values = Trm_matching.tmap_to_list pattern_vars (Trm_matching.tmap_filter_keys pattern_vars inst) in
    List.iteri (fun id_var v -> all_values.(id_var).(id_path) <- v) values;
    let inst = List.map (fun (x, _) -> get_array_access (trm_var_possibly_mut ~const x) (trm_int id_path)) pattern_vars in
    let new_inst = Trm_map.empty in
    let new_inst = List.fold_left2 (fun acc (x, _) y -> Trm_map.add x y acc) new_inst pattern_vars inst in
    let new_t = Internal.subst new_inst pattern_instr in
    apply_on_targets (fun t p -> apply_on_path (fun _ -> new_t) t p) (target_of_path p)
  ) tg;
  let vk = if const then Var_immutable else Var_mutable in
  let instrs_to_insert = List.mapi (fun id_var (x, _) -> trm_let_array vk (x, typ_double ()) (Const nb_paths) (trm_array (Mlist.of_list (Array.to_list all_values.(id_var))))) pattern_vars in
  Internal.nobrace_remove_after (fun _ ->
    Sequence_basic.insert (trm_seq_no_brace instrs_to_insert) ([tBefore] @ (target_of_path !path_to_surrounding_seq) @ [dSeqNth !minimal_index]))
  )

(* [detach_if_needed tg]: expects the target [tg] to be pointing at a variable declaration, then it will
    check if that declaration was already initialized or not, if that's the case than it will deatch that
    declaration, otherwise no change is applied. *)
let detach_if_needed (tg : target) : unit =
  iter_on_targets (fun t p ->
    let decl_t  = Path.resolve_path p t in
    match decl_t.desc with
    | Trm_let(vk,(_, _), init) ->
      begin match vk with
      | Var_immutable -> ()
      | _ ->
        begin match init.desc with
        | Trm_apps (_,[_init]) ->
          Variable_basic.init_detach (target_of_path p)
        | _ -> ()
        end
      end
    | _ -> fail t.loc "Variable.init_detach_aux: variable could not be matched, make sure your path is correct"
  ) tg

(* [reuse ~reparse space tg] expects the target [tg] to be poiting to a variable declaration, then it will
    remove that declaration and replace all its occurrences with [space]

   @correctness: correct if the previous variable space was never read after the reuse point. *)
let reuse ?(reparse : bool = false) (space : trm) : Transfo.t =
  reparse_after ~reparse (iter_on_targets (fun t p ->
      let decl_t = Path.resolve_path p t in
      begin match decl_name decl_t with
      | Some x ->
        let _path_to_seq, _,_ = Internal.get_instruction_in_surrounding_sequence p in
        Marks.add "reuse_mark" (target_of_path p);
        detach_if_needed [cMark "reuse_mark"];
        Instr_basic.delete [cMark "reuse_mark"];
        Variable_basic.subst ~subst:x ~put:space (target_of_path _path_to_seq)
      | None -> fail decl_t.loc "Variable.reuse: could not match the declaration"
      end
      ))
(* [renames rename tg: expects [tg] to point at a sequence.
    [rename] can be either ByList l where l denotes a list of pairs on which
    each pair consists the current variable and the one that is going to replace it.
    Or AddSuffix s, in this case all the variables declared inside the targeted sequence
     are going to be renamed by adding the suffix [s] at the end of its current name. *)
let renames (rename : rename) : Transfo.t =
  iter_on_targets (fun t p ->
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
      List.iter2 (fun d into -> Variable_basic.rename ~into ((target_of_path p) @  [cVarDef d])) decl_vars new_decl_vars
    | _ -> fail tg_trm.loc "Variable.renames: the target should be pointing at a sequence" )


(*
  Documentation at basic level of Variable.unfold
    Applies to a definition [T a = e;]
    Fails if the definition is not a const or a reference.
    Replaces [a] with [e] at the desired targets (or everywhere).


  Documentation at combi level of Variable.unfold

  1)   T& a = e;
       Replace all occurences of a with e  (in the scope).
       (This transformation is always correct, no matter
       if T is const or not.)

  2)   T const a = e;
       Replace all occurences of a with e  (in the scope).
       (This transformation is correct if e makes no side effects,
        else it's more complicated; it's user responsability.)

  3)   T a = e;
       We call Variable.to_const beforehand, to make it "T const a = e"
       (This works if variable a is not modified after its definition,
       else conversion to const would fail.)

  Documentation at combi level of Variable.inline
    It is Variable.unfold + delete declaration
*)


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
(* [unfold ~accept_functions ~simple_deref ~delete tg] expects the target [tg] to be pointing at a variable declaration.
    If the variable has a struct type then a mark is created and passed as an argument to Variable_basic.unfold
      on the next step, otherwise no mark needs to be created.
      We consider the following cases:
      1) If the targeted variable is mutable then we try to make it immutable by calling Variable.to_const.
         WARNING: This step will fail in the case when there are any write operations on the targeted varibles.
      2) If the transformation didn't fail in the first step we are sure that we are trying to inline a const variable
         and we can call safely Variable_basic.unfold
      3) If the targeted variable is a struct type then call Record_basic.simpl_proj to remove all the occurrences
          of struct initialization list, by projecting them on the field they are accessed.
          Ex: int v = {0,1} if we had v.x then Variable_basic.inline will transform it to {0, 1}.x which is non valid C code.
          After calling Record_basic.simpl_proj {0, 1}.x becomes 0 .
          Finally, if simple_deref is set to true then we will seach for all the occurrences of *& and &* and simplify them. *)
let unfold ?(accept_functions : bool = false) ?(simpl_deref : bool = false) ?(delete : bool = true) ?(at : target = []): Transfo.t =
  iter_on_targets (fun t p ->
    let tg_trm = Path.resolve_path p t in
    let tg_decl = target_of_path p in
    match tg_trm.desc with
    | Trm_let (vk, (x, _tx), init) ->

      let mark = begin match get_init_val init with
      | Some init ->
          if is_struct_init init then Mark.next() else ""
      | _ -> fail tg_trm.loc "Variable.unfold: you should never try to inline uninitialized variables"
      end in
      begin match vk with
      | Var_immutable ->
        if delete
          then Variable_basic.inline ~mark ~accept_functions tg_decl
          else Variable_basic.unfold ~mark ~accept_functions ~at tg_decl
      | Var_mutable ->
        if not (trm_has_cstyle Reference tg_trm) then Variable_basic.to_const tg_decl;
        if trm_has_cstyle Reference tg_trm
          then Variable_basic.inline ~mark ~accept_functions tg_decl
          else if delete
            then Variable_basic.inline ~mark ~accept_functions tg_decl
          else Variable_basic.unfold ~mark ~accept_functions ~at tg_decl
      end;
     if mark <> "" then begin
       Record_basic.simpl_proj [nbAny; cFieldAccess ~base:[cMark mark] ()];
       if simpl_deref
         then Variable_basic.simpl_deref [nbAny; cRead ~addr:[cMark mark] ()];
       Marks.remove mark [nbAny; cMark mark]
     end
    | _ -> fail t.loc "Variable.unfold: expected a target to a variable declaration"
  )

(* [inline ~accept_functions ~simpl_deref tg]: similar to [unfold] except that this transformation
     deletes the targeted declaration by default. *)
let inline ?(accept_functions : bool = false) ?(simpl_deref : bool = false) : Transfo.t =
  unfold ~accept_functions ~simpl_deref ~delete:true



(* [inline_and_rename]: expects the target [tg] to point at a variable declaration with an initial value
    being another variable. Then it will inline the targeted variable. And rename variable that was the
    initialization value to the one we inlined

    Assumption:
      if the target [tg] points to the following instruction int y = x; then
      no occurrence of x appears after that instruction *)
let inline_and_rename : Transfo.t =
  iter_on_targets (fun t p ->
    let tg_trm = Path.resolve_path p t in
    let path_to_seq, _ = Internal.isolate_last_dir_in_seq p in
    let tg_scope = target_of_path path_to_seq in
    match tg_trm.desc with
    | Trm_let (vk, (y, ty), init) ->
        let spec_target = tg_scope @ [cVarDef y] in
        begin match get_init_val init with
        | Some v ->
          begin match v.desc with
          | Trm_var (_, x) ->
            if is_typ_const ty then begin
                inline spec_target;
                renames (ByList [(x.qvar_var,y)]) tg_scope
              end
             else begin
              Variable_basic.to_const spec_target;
              inline spec_target;
              renames (ByList [(x.qvar_var,y)]) tg_scope
              end
          | Trm_apps (_, [{desc = Trm_var (_, x);_}]) when is_get_operation v ->
             if is_typ_const ty then begin
                inline spec_target;
                renames (ByList [(x.qvar_var,y)]) tg_scope
              end
             else begin
              Variable_basic.to_const spec_target;
              inline spec_target;
              renames (ByList [(x.qvar_var,y)]) tg_scope
              end
          | _ ->
            (* DEBUG: *)
            (* Printf.printf "For variable %s\n got value %s\n" y (Ast_to_text.ast_to_string v); *)
            fail tg_trm.loc "Variable.inline_and_rename: expected a target of the form int x = get(r), int x = r, const int x = r or const int x = get(r)"
          end
        | _ -> fail init.loc "Variable.inline_and_rename: please try targeting initialized variable declarations"
        end
    | _ -> fail t.loc "Variable.inline_and_rename: expected the declaration of the variable which is going to be inlined"
  )

(* [elim_redundant ~source tg]: expects the target [tg] to be point at a variable declaration with an initial value being
    the same as the variable declaration where [source] points to. Then it will fold the variable at [source] into
    the variable declaration [tg] and inline the declaration in [tg]. *)
let elim_redundant ?(source : target = []) : Transfo.t =
  iteri_on_targets (fun i t p ->
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
          | _ -> fail t.loc "Variable.elim_redundant: couldn't find the englobing sequence, try providing the source argument to solve this problem"
          end
       else
        begin
          let source_paths = resolve_target source t in
          let source_decl_trm = match List.nth_opt source_paths i with
            | Some p -> Path.resolve_path p t
            | None -> fail t.loc "Variable.elim_redundant: the number of source targets  should be equal to the number of the main targets" in
          match source_decl_trm.desc with
          | Trm_let (_, (y, _), init_y) when Internal.same_trm init_x init_y ->
            source_var := y
          | _ -> fail source_decl_trm.loc "Variable.elim_redundant: the soource target should point to a variable declaration"
        end;
        Variable_basic.fold ~at:([cVarDef x]) ((target_of_path path_to_seq) @ [cVarDef !source_var]);
        inline ((target_of_path path_to_seq) @ [cVarDef x])
    | _ -> fail tg_trm.loc "Variable.elim_redundant: the main target should point to the declaration of the variable you want to eliminate"

  )

(* [insert ~constr name typ value tg]: expects the target [tg] to point at a location in a sequence then it wil insert a
    new variable declaration with name: [name] and type:[typ] and initialization value [value].
    This transformation is basically the same as the basic one except that this has a default value for the type argument. *)
let insert ?(const : bool = true) ?(reparse : bool = false) ?(typ : typ = typ_auto ()) ~name:(name : string) ?(value : trm = trm_uninitialized()) : Transfo.t =
 Variable_basic.insert ~const ~reparse ~name ~typ ~value

(* [insert_list ~const names typ values tg]: expects the target [tg] to be poiting to a location in a sequence
    then it wil insert a new variable declaration with [name], [typ] and initialization [value] *)
let insert_list ?(const : bool = false) ?(reparse : bool = false) ~defs:(defs : (string * string * trm ) list) : Transfo.t =
  let defs = List.rev defs in
  reparse_after ~reparse (fun tg ->
    List.iter (fun (typ, name, value) ->
      (* This check is needed to avoid the parentheses in the case when the value of the vairbale is a simple expression  *)
      insert ~const ~name ~typ:(AstParser.ty typ) ~value tg) (List.rev defs)
)

(* [insert_list_same_type typ name_vals tg]: inserts a list of variables with type [typ] and name and value give as argument in [name_vals]. *)
let insert_list_same_type ?(reparse : bool = false) (typ : typ) (name_vals : (string * trm) list) : Transfo.t =
  let const = false in
  reparse_after ~reparse (fun tg ->
    List.iter (fun (name, value) ->
      insert ~const ~name ~typ ~value tg) name_vals)
