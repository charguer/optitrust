open Syntax
open Target
open Path
include Function_basic

(* [rename]: instantiation of Rename module *)
type rename = Variable.Rename.t

(* [bind_args fresh_names tg]: expects the target [tg] to point at a function call.
      Then it takes [fresh_names] which is a list of strings where the string
      at index i represents the variable going to be binded to the argument i
      of the function call. If one doesn't want to bind the argument at index i
      then it just leaves it as an empty string "". Basically this transformation is
      just an aplication of bind_intro n times. Where n is the numer of strings inside
      [fresh_names] different from "". *)
let%transfo bind_args (fresh_names : string list) (tg : target) : unit =
  iter_on_targets (fun t p ->
    let call_trm = get_trm_at_path p t in
    let call_mark = "bind_args_mark" in
    let nb_fresh_names = List.length fresh_names in
    let error = "Function.bind_args: expected a target to a function call." in
    let _, tl = trm_inv ~error trm_apps_inv call_trm in
    if nb_fresh_names = 0
      then ()
      else if List.length tl <> nb_fresh_names then
        fail call_trm.loc "Function.bind_args: each argument should be binded to a variable or the empty string. "
      else begin
        Marks.add call_mark (target_of_path p);
        List.iteri (fun ind fresh_name ->
          if fresh_name = ""
            then ()
            else Function_basic.bind_intro ~fresh_name ~const:false [cMark call_mark; dArg ind]
        ) fresh_names;
        Marks.remove call_mark [cMark call_mark] end
) tg

(* [elim_body ~vars tg]: expects the target [tg] to point at a marked sequence.
     Then it will change all the declaraed variables inside that sequence  based on [vars]
     Either the user can give a list of variables together with their new names, or he can give the postifx
     that's going to be assigned to all the declared vairables. *)
let%transfo elim_body ?(vars : rename = AddSuffix "") (tg : target) : unit =
  Trace.tag_valid_by_composition ();
  iter_on_targets (fun t p ->
    let tg_trm = Stats.comp_stats "elim_body_resolve" (fun () -> Path.resolve_path p t) in
    let error = "Function.elim_body: the given target should point at a sequence." in
    let _ = trm_inv ~error trm_seq_inv tg_trm in
    Stats.comp_stats "elim_body_renames" (fun () ->
      Variable.renames vars (target_of_path p));
    Stats.comp_stats "elim_body_elim" (fun () ->
      Sequence_basic.elim (target_of_path p));
  ) tg

(* [bind ~fresh_name ~args tg]: expects the target [tg] to point at a function call,
    Then it will just call bind args and bind_intro.
    Basically this tranasformation just binds a variable to the targeted function call
    and its arguments.*)
let%transfo bind ?(fresh_name : string = "res") ?(args : string list = []) (tg : target) : unit =
  bind_args args tg;
  Function_basic.bind_intro ~const:false ~fresh_name tg

(* [inline ~resname ~vars ~args ~keep_res ~delete ~debug tg]: expects the target [ŧg] to point at a function call
    Then it will try to inline that function call. If it's possible this transformation tries to
    perform as many simplifications as possible.


    -------------------------------------------------------------------------------------------------------------
    This transformation handles the following cases:

    Case 1: Function call belongs to a variable declaration:
      Ex:
      int f(int x){
        return x + 1;
      }
      int a = 10;
      int b = f(a);
    Case 2: Function call belongs to a write operation
      Ex:
      int f(int x){
        return x + 1;
      }
      int a = 10;
      a = f(a);
    Case 3: Function call is does not return any value(a call to a function of void type)
      Ex:
        void f(int& x){
          x = x + 1;
        }
        int& a = 10;
        f(a);
    Case 4: Function call belongs to a for loop component
      Ex:
        int f(int x){
          return x + 1;
        }
        int a = 10;
        for(int i = f(a); i < 20; i ++){
          ..
        }
    -------------------------------------------------------------------------------------------------------------
      STEPS:

      Step 1(Only for case 1):
        Mark the instruction that contains the function call as "__inline_instruction"

      Step 2:
        Bind [resname] variable to the function call, if [resname] was not provided by the user then "__TEMP_Optitrust" is going to be
        used as a temporary variable.

      Step 3:
        Mark the function call for easier targeting in case it hasn't been marked by previous transformation.

      Step 4:
        Create a special mark for the inline body of the function [body_mark = "__TEMP_BODY" ^ (string_of_int i)] for easy targeting that inilin
        function body.

      Step 5:
        Call [Function_basic.inline] with target being the marked function call.
        Note: This step detaches the binded declaration.

      Step 6:
        Function arguments are encoded as const variables and that's different from the encodings of declared variables.
        This introduces wrong struct and array accesses. To fix this [Accesses.intro] with target being the marked body genereated
        from [Step 5] is called.

      Step 7:
        Integrates the sequence from [Step 6] to its surrouding sequence.
        To avoid name clashes this transformation renames all the variables defined inside that sequence by using the rule defined
        by [vars] variable.
        Note: It's the users responsibility to introduce a good renaming strategy(See [Variable_core.rename module]).

      Step 8:
        Recall [Step 5] detaches the binded declaration. This step tries to attach that variable declaration with the value returned
        by the function.

      Step 9:
        TODO: Add all the steps when function inline was completely debugged

      TODO: when the system of intermediate steps for combi transformations is available,
      apply it to generate the intermediate steps for the inlining example.

   EXAMPLE:
    int g(int x, int y, int z, int w) {
  int p = x + x + y + z + w;
  return p + p;
}
int main1() { // initial step : target on g(..)
  int u = 1, v = 2, w = 3;
  int t = f(g(h(4), u, m(v, 2), (w + 1)));
}
int main2() { // Function_basic.bind_intro
  int u = 1, v = 2, w = 3;
  int r = [mymark:](g(h(4), u, m(v, 2), (w + 1)));
  int t = f(r);
}
int main3() { // Function.bind_args ~[cMark mymark]
  int u = 1, v = 2, w = 3;
  int a = h(4);
  int b = m(v, 2);
  int r = [mymark:](g(a, u, b, (w + 1)));
  int t = f(r);
}
int main4() { // Function_basic.inline ~[cMark mymark]
              // The mark gets moved to the surrounding declaration
  int u = 1, v = 2, w = 3;
  mymark: int r; // same as before, only you remove the initialization term
  mybody: {
    int p = ((((a + a) + u) + b) + (w + 1));
    r = (p + p);
  }
  int t = f(r);
}
int main5() { // Function.elim_body ~[cMark mark]
  int u = 1, v = 2, w = 3;
  int a = h(4);
  int b = m(v, 2);
  mymark: int r;
  int p = ((((a + a) + u) + b) + (w + 1));
  r = (p + p);
  int t = f(r);
}
int main6() { // Variable_basic.init_attach
  int u = 1, v = 2, w = 3;
  int a = h(4);
  int b = m(v, 2);
  int p = ((((a + a) + u) + b) + (w + 1));
  int r = (p + p);
}

Other example in the case of return:

int h(int x) {
  if (x > 0)
    return -x;
  return x;
}
int f1() {
  int a = 3;
  int r = [mymark:]h(a);
  int s = r;
}
int f2() { // result of Funciton_basic.inline_cal
    // generate goto, generate label, don't call init_attach
  int a = 3
  [mymark:]int r;
  if (a > 0) {
    r = -a;
    goto _exit;
  }
  r = a;
  _exit:;
  int s = r;
} *)
let%transfo inline ?(resname : string = "") ?(vars : rename = AddSuffix "") ?(args : string list = []) ?(keep_res : bool = false)
  ?(delete : bool = false) ?(debug : bool = false) ?(simpl : Transfo.t = Variable.default_inline_simpl) (tg : target) : unit
  =
  Trace.tag_valid_by_composition ();
  Marks.with_fresh_mark (fun subst_mark ->
    (* variable for storing the function names, in case if [delete] is true it will use this name to target the declarations and delete them *)
    let function_names = ref Var_set.empty in
    Stats.comp_stats "iteri_on_transformed_targets" (fun () ->
  iteri_on_transformed_targets (Internal.get_instruction_in_surrounding_sequence)
    (fun i t (path_to_seq, local_path, i1) ->
      let vars = Variable.map (fun x -> Tools.string_subst "${occ}" (string_of_int i) x) vars in
      let resname = ref resname in
      if !resname = "" then resname := sprintf "__res_%d" i;
      let path_to_instruction = path_to_seq @ [Dir_seq_nth i1] in
      let path_to_call = path_to_instruction @ local_path in

      let tg_out_trm = Path.resolve_path path_to_instruction t in
      let my_mark = "__inline" ^ "_" ^ (string_of_int i) in
      let mark_added = ref false in
      let call_trm = Path.get_trm_at_path path_to_call t in
      begin match call_trm.desc with
        | Trm_apps ({desc = Trm_var (_, f)}, _, _) -> function_names := Var_set.add f !function_names;
        | _ ->  fail t.loc "Function.get_function_name_from_call: couldn't get the name of the called function"
      end;

      let post_processing ?(deep_cleanup : bool = false)() : unit =
      Stats.comp_stats "post_processing" (fun () ->
        let new_target = cMark my_mark in
        if not !mark_added then Marks.add my_mark (target_of_path path_to_call);
        if args <> [] then bind_args args [new_target];
        let body_mark = "__TEMP_BODY" ^ (string_of_int i) in
        Stats.comp_stats "inline" (fun () ->
          Function_basic.inline ~body_mark ~subst_mark [new_target];);
        Stats.comp_stats "intro" (fun () ->
          Accesses_basic.intro [cMark body_mark];);
        Stats.comp_stats "elim_body" (fun () ->
          elim_body ~vars [cMark body_mark];);
        if deep_cleanup then begin
          let success_attach = match Trace.backtrack_on_failure (fun () ->
            Variable_basic.init_attach [new_target]
          ) with
          | Success -> true
          | Failure Variable_core.Init_attach_no_occurrences
          | Failure Variable_core.Init_attach_occurrence_below_control ->
            false
          | Failure e -> raise e
          in
          if success_attach then begin
            Variable.inline ~simpl [new_target];
            Variable.inline_and_rename ~simpl [nbAny; cVarDef !resname];
            if not keep_res then begin
              ignore (Trace.backtrack_on_failure (fun () ->
                Variable.inline_and_rename ~simpl [nbAny; cMark "__inline_instruction"]
              ));
              (* TODO: only with | TransfoError ? *)
              Marks.remove "__inline_instruction" [nbAny;cMark "__inline_instruction" ]
            end;
          end else if not keep_res then
            ignore (Trace.backtrack_on_failure (fun () ->
              Variable.inline_and_rename ~simpl [nbAny; cMark "__inline_instruction"]
              (* TODO: only with | TransfoError ? *)
            ));
          Marks.remove my_mark [nbAny; new_target]
        end;
        Marks.remove my_mark [nbAny; new_target];
        Record_basic.simpl_proj (target_of_path path_to_seq);
      )
        in
      begin match tg_out_trm.desc with
      | Trm_let _ ->
        Marks.add "__inline_instruction" (target_of_path path_to_instruction);
        Stats.comp_stats "bind_intro1" (fun () ->
          Function_basic.bind_intro ~my_mark ~fresh_name:!resname ~const:false (target_of_path path_to_call));
        mark_added := true;
        post_processing ~deep_cleanup:true ();
      | Trm_apps (_, [ls; rs], _) when is_set_operation tg_out_trm ->
        Stats.comp_stats "bind_intro2" (fun () ->
          Function_basic.bind_intro ~my_mark ~fresh_name:!resname ~const:false (target_of_path path_to_call));
        mark_added := true;
        post_processing ~deep_cleanup:true ()
      | Trm_apps _ ->
        post_processing ();
      | Trm_for _ | Trm_for_c _ ->
          if debug then Transfo_debug.path "Full_path to the call" path_to_call;
          Function_basic.bind_intro ~my_mark ~fresh_name:!resname ~const:false (target_of_path path_to_call) ;
        mark_added := true;
        post_processing ~deep_cleanup:true ();
      | _ -> fail tg_out_trm.loc "Function.inline: please be sure that you're tageting a proper function call"
      end;
    ) tg;
    if delete then Function_basic.delete [cOr
      (List.map (fun f -> [cTopFunDef f.name]) (Var_set.elements !function_names))];
    );
    simpl [cMark subst_mark];
  )

(* [inline_def]: like [inline], but with [tg] targeting the function definition.
   All function calls are inlined, with [delete = true] as default. *)
let%transfo inline_def ?(resname : string = "") ?(vars : rename = AddSuffix "") ?(args : string list = []) ?(keep_res : bool = false)
  ?(delete : bool = true) ?(simpl : Transfo.t = Variable.default_inline_simpl) (tg : target) : unit
  =
  Trace.tag_valid_by_composition ();
  Target.iter (fun t p ->
    let def_trm = Path.resolve_path p t in
    let error = "Function.inline_def: expected function definition" in
    let (qvar, _, _, _) = trm_inv ~error trm_let_fun_inv def_trm in
    (* FIXME: deal with qvar *)
    inline ~resname ~vars ~args ~keep_res ~delete ~simpl [nbAny; cFun qvar.name];
  ) tg

(* [beta ~indepth tg]: expects the target [tg] to be pointing at a function call or a function declaration whose
     parent trm is a function call. If its the first case then it will just call Function_basic.beta.
     If its the second case then this transformation will just redirect the target to the parent function call
     and then call Function_basic.beta.
     [indepth]: if true it will apply the beta reduction to all the descendants of [tg].
     [body_mark]: mark left in case this transformation is used as an intermediate step of an another transformation.


     Note: If [tg] points to a function call then similar to Function_basic.beta, this transformation can be considered as an
     alias of Function_basic.inline. If that's not the case then transformation will do something as the similar to the following:
     int a = (void f(int x) {return x})(3) --> int a = 3;.*)

(* [beta ~indepth tg]: applies beta-reduction on candidate function calls that appear
    either "exactly at" or "anywhere in depth" in the target [tg], depending on the value of ~indepth. *)
let%transfo beta ?(indepth : bool = false) ?(body_mark : mark = "") (tg : target) : unit =
  let tg = if indepth
    then tg @ [cFun ~fun_:[cFunDef ""] ""]
    else tg in
  iter_on_targets (fun t p ->
    let tg_trm = Path.resolve_path p t in
    match tg_trm.desc with
    | Trm_apps _ ->
      Function_basic.beta ~body_mark tg
    | Trm_let_fun (_f, _, _, _, _) ->
      let parent_path, _ = Xlist.unlast p in
      let parent_node = Path.resolve_path parent_path t in
      begin match parent_node.desc with
      | Trm_apps _ -> Function_basic.beta ~body_mark (target_of_path parent_path)
      | _ -> ()
      end
    | _ -> fail t.loc "Function.beta: this transformation expects a target to a function call"
  ) tg

(* [use_infix_ops ~tg_ops]: expects the target [tg] to be pointing at an instruction that can be converted to
     an infix form, for example x = x + 1 can be converted to x += 1,
    [indepth]: if true then it will check all the descendants of [t] if there are any write operations to be transformed
    [allow_identity]: if true it stops the transformation from failing when it finds nodes that can't be transformed.*)
let%transfo use_infix_ops ?(indepth : bool = false) ?(allow_identity : bool = true) (tg : target) : unit =
  let tg = if indepth
    then [nbMulti] @ tg @ [cWrite ~rhs:[cPrimPredFun is_infix_prim_fun] ()] else tg in
  Function_basic.use_infix_ops_at ~allow_identity tg


(* [uninline ~fxt tg]: expects the target [tg] to be pointing at an instruction that is similar to the first instruction
    of the body of the function declared in [fct]. Let nb be the number of instruction on the body of [fct]. The transformation
    will put the targeted instruction together with the following (nb -1) instructions into a sequence marked with a mark.
    Now the stage is ready for applying the basic version of uninline. After calling that transformation and assuming that
    everything went fine we can now eliminate the introduced sequence. The arg [with_for_loop] should be set to true if the
    original function declaration contains a for loop.*)
let%transfo uninline ?(contains_for_loop : bool = false) ~fct:(fct : target) (tg : target) : unit =
  let tg_fun_def = match get_trm_at fct with
  | Some td -> td
  | None -> fail None "Function.uninline: fct target does point to any node" in
  iter_on_targets (fun _ p ->
    let mark = Mark.next () in
    match tg_fun_def.desc with
    | Trm_let_fun (_, _, _, body, _) ->
      begin match body.desc with
      | Trm_seq tl ->
        let nb = Mlist.length tl in
        Sequence_basic.intro nb ~mark (target_of_path p);
        if contains_for_loop then Sequence_basic.intro_on_instr [cMark mark; cFor_c "";dBody];
        Function_basic.uninline ~fct [cMark mark]
      | _ -> fail tg_fun_def.loc "Function.uninline: weird function declaration "
      end
    | _ -> fail tg_fun_def.loc "Function.uinline: fct arg should point to a a function declaration"
  ) tg

(* [insert ~reparse decl tg]: expects the relative target [t] to point before or after an instruction,
     then it will insert the function declaration [decl] on that location.
     To integrate the new declaration with the current AST [reparse] should be set to true. *)
let%transfo insert ?(reparse : bool = false) (decl : string) (tg : target) : unit =
  Sequence.insert ~reparse (stmt decl) tg
