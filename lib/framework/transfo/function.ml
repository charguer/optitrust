open Prelude
open Target
open Path
include Function_basic

(** [rename]: instantiation of Rename module *)
type rename = Variable.Rename.t

(** [bind_args fresh_names tg]: expects the target [tg] to point at a function call.
      Then it takes [fresh_names] which is a list of strings where the string
      at index i represents the variable going to be binded to the argument i
      of the function call. If one doesn't want to bind the argument at index i
      then it just leaves it as an empty string "". Basically this transformation is
      just an aplication of bind_intro n times. Where n is the numer of strings inside
      [fresh_names] different from "".
      If [inline_impure_mark] is set, forces every impure argument to be bound before
      but add the [inline_impure_mark] to those that should be inlined later because they
      were given no name. *)
let%transfo bind_args ?(inline_impure_mark: mark = no_mark) (fresh_names : string list) (tg : target) : unit =
  Target.iter (fun p ->
    Marks.with_fresh_mark_on p (fun call_mark ->
      let call_trm = Target.resolve_path p in
      let nb_fresh_names = List.length fresh_names in
      let error = "expected a target to a function call." in
      let _, effective_args = trm_inv ~error trm_apps_inv call_trm in
      let fresh_names = if nb_fresh_names = 0
        then List.init (List.length effective_args) (fun _ -> "")
        else if List.length effective_args <> nb_fresh_names then
          trm_fail call_trm "each argument should be binded to a variable or the empty string. "
        else fresh_names
      in
      let ind = ref 0 in
      List.iter2 (fun fresh_name effective_arg ->
        begin if fresh_name = "" then begin
          if inline_impure_mark <> no_mark && not (Resources.trm_is_pure effective_arg) then
            Variable_basic.bind ~mark_let:inline_impure_mark "arg" ~const:true [cMark call_mark; dArg !ind]
          else ()
        end else
          Variable_basic.bind fresh_name ~const:true [cMark call_mark; dArg !ind]
        end;
        incr ind
      ) fresh_names effective_args
    )
  ) tg

(** [elim_body ~vars tg]: expects the target [tg] to point at a marked sequence.
     Then it will change all the declaraed variables inside that sequence based on [vars]
     Either the user can give a list of variables together with their new names, or he can give the postifx
     that's going to be assigned to all the declared vairables. *)
let%transfo elim_body ?(resname: string = "") ?(vars : rename = AddSuffix "") (tg : target) : unit =
  Trace.tag_valid_by_composition ();
  Target.iter (fun p ->
    let tg_trm = Stats.comp_stats "elim_body_resolve" (fun () -> Target.resolve_path p) in
    let error = "Function.elim_body: the given target should point at a sequence." in
    let _ = trm_inv ~error trm_seq_inv tg_trm in
    Stats.comp_stats "elim_body_renames" (fun () ->
      Variable.renames vars (target_of_path p));
    Stats.comp_stats "elim_body_elim" (fun () ->
      Sequence.elim ~resname (target_of_path p));
  ) tg

(** [inline ?resname ?vars ?args ?delete ?recurse ?simpl tg]: expects the target [tg] to point at a function call
    Then it will try to inline that function call. If it's possible this transformation tries to
    perform as many simplifications as possible.
    - [resname]: if present the result of the function call will be bound with this name.
    - [vars]: renaming scheme for local variables that occur in the body of the inlined function
    - [args]: names for binding the function arguments before the call, any empty string corresponds to an argument that should not be bound outside and inlined instead (by default we consider that each argument should be inlined inside).
    - [delete]: if true, the function definition should be removed as well
    - [recurse]: inlines functions recursively within the inlined bodies
    - [simpl]: simplifications to perform around subtituted arguments (by default [Variable.default_inline_simpl]).
*)
let%transfo inline ?(resname : string = "")
  ?(vars : rename = AddSuffix "") ?(args : string list = [])
  ?(delete : bool = false) ?(recurse : bool = false)
  ?(simpl : target -> unit = Variable.default_inline_simpl) (tg : target) : unit
  =
  Marks.with_marks (fun next_mark ->
    let subst_mark = next_mark () in
    (* variable for storing the function names, in case if [delete] is true it will use this name to target the declarations and delete them, also used for [recurse]. *)
    let function_names = ref Var_set.empty in
    let still_to_process = ref [] in
    let process i p =
      let vars = Variable.map (fun x -> Tools.string_subst "${occ}" (string_of_int i) x) vars in

      let call_trm = Target.resolve_path p in
      begin match call_trm.desc with
        | Trm_apps ({desc = Trm_var f}, xs, _) ->
          function_names := Var_set.add f !function_names
        | _ ->  trm_fail call_trm "Function.get_function_name_from_call: couldn't get the name of the called function"
      end;

      let call_mark = next_mark () in
      Marks.add call_mark (target_of_path p);
      let new_target = cMark call_mark in

      let inline_mark = if !Flags.check_validity then next_mark () else no_mark in
      bind_args ~inline_impure_mark:inline_mark args [new_target];

      let body_mark = "__TEMP_BODY" ^ (string_of_int i) in
      Stats.comp_stats "inline" (fun () ->
        Function_basic.inline ~body_mark ~subst_mark [new_target]);
      if recurse then begin
        let path_to_seq, _, _ = Internal.get_instruction_in_surrounding_sequence p in
        let m = Marks.add_next_mark_on next_mark path_to_seq in
        still_to_process := m :: !still_to_process;
      end;
      (* TODO: improvement for [recurse] ?
          currently imprecise as it will inline in all the surrounding sequence

      if recurse then begin
        let (start, stop) = span_marks (next_mark ()) in
        Marks.add start [tBefore; cMark body_mark];
        Marks.add stop [tAfter; cMark body_mark];
        still_to_process := (start, stop) :: !still_to_process;
      end; *)
      Stats.comp_stats "intro" (fun () -> Accesses_basic.intro [cMark body_mark]);
      Record_basic.simpl_proj [cMark body_mark];
      Stats.comp_stats "elim_body" (fun () -> elim_body ~resname ~vars [cMark body_mark]);
      Variable.inline ~simpl [nbAny; cMark inline_mark];
    in
    Target.iteri process tg;
    while !still_to_process <> [] do
      let m = List.hd !still_to_process in
      still_to_process := List.tl !still_to_process;
      Target.iteri process [nbAny; cMark m; cOr
        (List.map (fun f -> [cCall f.name]) (Var_set.elements !function_names))];
    done;
    if delete then Function_basic.delete [cOr
      (List.map (fun f -> [cTopFunDef f.name]) (Var_set.elements !function_names))];
    simpl [cMark subst_mark];
  )

(* TODO: remove ~recurse from inline and implement here? *)
let inline_multi = inline ~recurse:true

(** [inline_def]: like [inline], but with [tg] targeting the function definition.
   All function calls are inlined, with [delete = true] as default. *)
let%transfo inline_def ?(vars : rename = AddSuffix "") ?(args : string list = [])
  ?(delete : bool = true) ?(simpl : target -> unit = Variable.default_inline_simpl) (tg : target) : unit
  =
  Trace.tag_valid_by_composition ();
  Target.iter (fun p ->
    let def_trm = Target.resolve_path p in
    let error = "Function.inline_def: expected function definition" in
    let (qvar, _, _, _, _) = trm_inv ~error trm_let_fun_inv def_trm in
    (* FIXME: deal with qvar *)
    inline ~vars ~args ~delete ~simpl [nbAny; cCall qvar.name];
  ) tg

(** [beta ~indepth tg]: expects the target [tg] to be pointing at a function call or a function declaration whose
     parent trm is a function call. If its the first case then it will just call Function_basic.beta.
     If its the second case then this transformation will just redirect the target to the parent function call
     and then call Function_basic.beta.
     [indepth]: if true it will apply the beta reduction to all the descendants of [tg].
     [body_mark]: mark left in case this transformation is used as an intermediate step of an another transformation.


     Note: If [tg] points to a function call then similar to Function_basic.beta, this transformation can be considered as an
     alias of Function_basic.inline. If that's not the case then transformation will do something as the similar to the following:
     int a = (void f(int x) {return x})(3) --> int a = 3;.*)

(** [beta ~indepth tg]: applies beta-reduction on candidate function calls that appear
    either "exactly at" or "anywhere in depth" in the target [tg], depending on the value of ~indepth. *)
let%transfo beta ?(indepth : bool = false) ?(body_mark : mark = no_mark) (tg : target) : unit =
  let tg = if indepth
    then tg @ [cCall ~fun_:[cFun ()] ""]
    else tg in
  Target.iter (fun p ->
    let tg_trm = Target.resolve_path p in
    match tg_trm.desc with
    | Trm_apps _ ->
      Function_basic.beta ~body_mark tg
    | Trm_fun _ ->
      let parent_path, _ = List.unlast p in
      let parent_node = Target.resolve_path parent_path in
      begin match parent_node.desc with
      | Trm_apps _ -> Function_basic.beta ~body_mark (target_of_path parent_path)
      | _ -> ()
      end
    | _ -> trm_fail tg_trm "Function.beta: this transformation expects a target to a function call"
  ) tg

(** [use_infix_ops ~tg_ops]: expects the target [tg] to be pointing at an instruction that can be converted to
     an infix form, for example x = x + 1 can be converted to x += 1,
    [indepth]: if true then it will check all the descendants of [t] if there are any write operations to be transformed
    [allow_identity]: if true it stops the transformation from failing when it finds nodes that can't be transformed.*)
let%transfo use_infix_ops ?(indepth : bool = false) ?(allow_identity : bool = true) (tg : target) : unit =
  let tg = if indepth
    then [nbMulti] @ tg @ [cWrite ~rhs:[cPrimPredFun is_infix_prim_fun] ()] else tg in
  Function_basic.use_infix_ops_at ~allow_identity tg


(** [uninline ~fxt tg]: expects the target [tg] to be pointing at an instruction that is similar to the first instruction
    of the body of the function declared in [fct]. Let nb be the number of instruction on the body of [fct]. The transformation
    will put the targeted instruction together with the following (nb -1) instructions into a sequence marked with a mark.
    Now the stage is ready for applying the basic version of uninline. After calling that transformation and assuming that
    everything went fine we can now eliminate the introduced sequence. The arg [with_for_loop] should be set to true if the
    original function declaration contains a for loop.*)
let%transfo uninline ?(contains_for_loop : bool = false) ~fct:(fct : target) (tg : target) : unit =
  let tg_fun_def = match get_trm_at fct with
  | Some td -> td
  | None -> failwith "Function.uninline: fct target does point to any node" in
  Target.iter (fun p ->
    let mark = Mark.next () in
    match trm_let_fun_inv tg_fun_def with
    | Some (_, _, _, body, _) ->
      begin match body.desc with
      | Trm_seq (tl, _) ->
        let nb = Mlist.length tl in
        Sequence_basic.intro nb ~mark (target_of_path p);
        if contains_for_loop then Sequence_basic.intro_on_instr [cMark mark; cFor_c "";dBody];
        Function_basic.uninline ~fct [cMark mark]
      | _ -> trm_fail tg_fun_def "Function.uninline: weird function declaration "
      end
    | _ -> trm_fail tg_fun_def "Function.uinline: fct arg should point to a a function declaration"
  ) tg

(** [insert ~reparse decl tg]: expects the relative target [t] to point before or after an instruction,
     then it will insert the function declaration [decl] on that location.
     To integrate the new declaration with the current AST [reparse] should be set to true. *)
let%transfo insert ?(reparse : bool = false) (decl : string) (tg : target) : unit =
  Sequence.insert ~reparse (stmt decl) tg
