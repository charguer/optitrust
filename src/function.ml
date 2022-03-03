open Ast
open Path
include Function_basic

type rename = Variable.Rename.t


(*  [bind_args fresh_names tg] expets the target [tg] to point to a function call.
      Then it takes [fresh_names] which is a list of strings where the string
      at index i represents the variable going to be binded to the argument i
      of the function call. If one doesn't want to bind the argument at index i
      then it just leaves it as an empty string "". Basically this transformation is
      just an aplication of bind_intro n times. Where n is the numer of string inside
      [fresh_names] different from "".
*)
let bind_args (fresh_names : vars) : Target.Transfo.t =
 let counter = ref (-1) in
 Target.apply_on_transformed_targets (Internal.get_instruction_in_surrounding_sequence)
  (fun t (p, p_local, i) ->
   let path_to_call = p @ [Dir_seq_nth i] @ p_local in
   let call_trm = Path.resolve_path path_to_call t in
   begin match call_trm.desc with
   | Trm_apps (_, tl) ->
    if List.length fresh_names = 0
      then begin Tools.printf "bind_args: no arguments to bind, no changes to be done\n"; t end (* LATER: check this *)
      else if List.length tl <> List.length fresh_names then
        fail call_trm.loc "bind_args: for each argument of the function call, there should be associated either an empty string or a variable to be bounded to"
      else begin
           Tools.fold_lefti (fun n t fresh_name ->
            if fresh_name <> "" then
            let () = incr counter in
            Function_core.bind_intro (i + !counter) fresh_name false (p_local @ [Dir_arg_nth n]) t p
            else t) t fresh_names
            end
   | _ ->
     Ast_to_text.print_ast ~only_desc:true stdout call_trm;
     fail call_trm.loc "bind_args: expected a function call as target"
   end)

(* [elim_body ~vars tg] expects the target [tg] to point to the marked sequence.Then it will
    remove this sequence and its mark and merge the trms inside this sequence with te ones of the
    sequence containing the marked sequence. But before doing that, first a change of all the declared
    variables inside this sequence is performed. [vars] tells for the way the reanming is done.
    Either the user can give a list of variables together with their new names, or he can give the postfix
    that shoudl be assigned to all the declared variables.
*)
let elim_body ?(vars : rename = AddSuffix "") (tg : Target.target) : unit =
  Target.iter_on_targets ( fun t p ->
    let tg_trm = Path.resolve_path p t in
    match tg_trm.desc with
    | Trm_seq _ ->
      Variable.renames vars (Target.target_of_path p);
      Sequence_basic.elim (Target.target_of_path p)
    | _ -> fail tg_trm.loc "elim_body: the targetd should be pointing to a sequence"
  ) tg

(* [bind ~fresh_name ~args tg] expectes the target [tg] to point to a function call, then
    it will just call bind args and bind_intro. Basically this function is used to save the user from
    entering both of them.
*)
let bind ?(fresh_name : string = "res") ?(args : vars = []) (tg : Target.target) : unit =
  bind_args args tg;
  Function_basic.bind_intro ~const:false ~fresh_name tg

(* [inline ~name_result ~body_mark ~vars ~args  tg]
      expects the target tg to point to point to a function call. And automates completely the process
      of function call inlining.

      Example:

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
}

// TODO: Explain in detail the last step
// NOTE: if we want to optimize, we could instead of
// using ~[cMark mymark] use ~((target_of_path p)++[cMark mymark])
// where p is the path to the englobing sequence.
*)

let inline ?(name_result : string = "") ?(vars : rename = AddSuffix "") ?(args : vars = []) (tg : Target.target) : unit = 
  Target.iteri_on_transformed_targets (Internal.get_instruction_in_surrounding_sequence)
    (fun i t (path_to_seq, local_path, i1) -> 
      let vars = Variable.map (fun x -> Tools.string_subst "${occ}" (string_of_int i) x) vars in 
      let name_result = ref name_result in
      if !name_result = "" then name_result := "__TEMP_Optitrust";
      let path_to_instruction = path_to_seq @ [Dir_seq_nth i1] in
      let path_to_call = path_to_instruction @ local_path in
      let tg_out_trm = Path.resolve_path path_to_instruction t in
      let my_mark = "__inline" ^ "_" ^ (string_of_int i) in
      let mark_added = ref false in
      let post_processing ?(deep_cleanup : bool = false)() : unit = 
        let new_target = Target.cMark my_mark in 
        if not !mark_added then Marks.add my_mark (Target.target_of_path path_to_call);
        if args <> [] then bind_args args [new_target];
        let body_mark = "__TEMP_BODY" ^ (string_of_int i) in 
        Function_basic.inline ~body_mark [new_target];
        Accesses_basic.intro [Target.cMark body_mark];
        elim_body ~vars [Target.cMark body_mark];
        if deep_cleanup then begin
          let success_attach = ref true in 
            let _ = try Variable_basic.init_attach [new_target] with
                | Variable_core.Init_attach_no_occurrences
                | Variable_core.Init_attach_occurrence_below_control -> success_attach := false; ()
                | e -> raise e in 
             if !success_attach then begin
                Variable.inline ~delete:true [new_target];
                Variable.inline_and_rename [Target.nbAny; Target.cVarDef !name_result];
                try Variable.inline_and_rename [Target.nbAny; Target.cMark "__inline_instruction"] with | TransfoError _ -> ();
                Marks.remove "__inline_instruction" [Target.nbAny;Target.cMark "__inline_instruction" ] end
             else  
                try Variable.inline_and_rename [Target.nbAny; Target.cMark "__inline_instruction"] with | TransfoError _ -> ();
            Marks.remove my_mark [Target.nbAny; new_target]
        end;
        Marks.remove my_mark [Target.nbAny; new_target];
        Struct_basic.simpl_proj (Target.target_of_path path_to_seq)
       in 
      begin match tg_out_trm.desc with  
      | Trm_let _ -> 
        Marks.add "__inline_instruction" (Target.target_of_path path_to_instruction);
        Function_basic.bind_intro ~my_mark ~fresh_name:!name_result ~const:false (Target.target_of_path path_to_call);
        mark_added := true;
        post_processing ~deep_cleanup:true ();
      | Trm_apps (_, [ls; rs]) when is_set_operation tg_out_trm -> 
        Function_basic.bind_intro ~my_mark ~fresh_name:!name_result ~const:false (Target.target_of_path path_to_call);
        mark_added := true;
        post_processing ~deep_cleanup:true ()
      | Trm_apps _ -> 
        post_processing ();
      | _ -> fail tg_out_trm.loc "inline: please be sure that you're tageting a proper function call"
      end
    ) tg
(*


  The combi transformation Function.beta takes a target:

  Function.beta =
    - if this target points to a trm_app, apply Function_basic.beta on it
    - if this target points to a trm_let_fun, check that the parent node is a trm_app, and target this one.
        int a = (void f(int x) { return x; })(3)
        ->
        int a =3


        same result as if you ave
> Executing task: ./run_action.sh ./view_result.sh
          f(3)

        trm_app ~base:[trm_let_fun ~name:"f"]
        [trm_let_fun ~name:"f"]


  The point is that the user can say "beta reduce the function f"

  The combi transformation has prototype:
      Function.beta ?(target:target=[]) ()
  when target is not provided, we use the target [cApp ~base:[cFunDef()]]
  to indicate that we are looking for any application of a function definition through the AST.

*)

(* [beta ~indepth tg] applies beta-reduction on candidate functions calls that appear
    either "exactly at" or "anywhere in depth" in the target [tg], depending on the value of ~indepth. *)
let beta ?(indepth : bool = false) ?(body_mark : mark = "") (tg : Target.target) : unit =
  let tg = if indepth
    then tg @ [Target.cFun ~fun_:[Target.cFunDef ""] ""]
    else tg in
  Target.iter_on_targets (fun t p ->
    let tg_trm = Path.resolve_path p t in
    match tg_trm.desc with
    | Trm_apps _ ->
      Function_basic.beta ~body_mark tg
    | Trm_let_fun (_f, _, _, _) ->
      let parent_path, _ = Tools.unlast p in
      let parent_node = Path.resolve_path parent_path t in
      begin match parent_node.desc with
      | Trm_apps (_, _args) -> Function_basic.beta ~body_mark (Target.target_of_path parent_path)
      | _ -> ()
      end
    | _ -> fail t.loc "beta: this transformation expects a target to a function call"
  ) tg

(* [use_infix_ops ~tg_ops] by default it targets all the instructions of the form x = x + a or x = a + x an transforms them
    into x += a
*)

let use_infix_ops ?(indepth : bool = false) ?(allow_identity : bool = true) (tg : Target.target) : unit = 
  let tg = if indepth 
    then [Target.nbMulti;Target.cWrite ~rhs:[Target.cPrimPredFun is_infix_prim_fun] ()] else tg in 
  Function_basic.use_infix_ops_at ~allow_identity tg
  

(* [uninline ~fxt tg] expects the target [tg] to be pointing at an instruction that is similar to the first instruction
    of the body of the function declared in [fct]. Let nb be the number of instruction on the body of [fct]. The transformation
    will put the targeted instruction together with the following (nb -1) instructions into a sequence marked with a mark.
    Now the stage is ready for applying the basic version of uninline. After calling that transformation and assuming that
    everything went fine we can now eliminate the introduced sequence.
*)
let uninline ~fct:(fct : Target.target) : Target.Transfo.t =
  let tg_fun_def = Target.get_trm_at fct in
  Target.iter_on_targets (fun _ p ->
    let mark = Mark.next () in
    match tg_fun_def.desc with
    | Trm_let_fun (_, _, _, body) ->
      begin match body.desc with
      | Trm_seq tl ->
        let nb = Mlist.length tl in
        Sequence_basic.intro nb ~mark (Target.target_of_path p);
        Function_basic.uninline ~fct [Target.cMark mark]
      | _ -> fail tg_fun_def.loc "uninline: weird function declaration "
      end
    | _ -> fail tg_fun_def.loc "uinline: fct arg should point to a a function declaration"

)
