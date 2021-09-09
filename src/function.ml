open Ast
open Path
include Variable_core.Rename
include Function_basic

type rename = Variable_core.Rename.t


(*  [bind_args fresh_names tg] expets the target [tg] to point to a function call.
      Then it takes [fresh_names] which is a list of strings where the string
      at index i represents the variable going to be binded to the argument i
      of the function call. If one doesn't want to bind the argument at index i
      then it just leaves it as an empty string "". Basically this transformation is
      just an aplication of bind_intro n times. Where n is the numer of string inside
      [fresh_names] different from "".
*)
let bind_args (fresh_names : var list) : Target.Transfo.t =
 let counter = ref (-1) in
 Target.apply_on_transformed_targets (Internal.get_call_in_surrounding_sequence)
  (fun (p, p_local, i) t ->
   if List.length fresh_names = 0 then t
    else
      Tools.foldi (fun n t fresh_name ->
      if fresh_name <> "" then
        let ()  = counter := !counter+1 in
        Function_core.bind_intro (i + !counter)  fresh_name false (p_local @ [Dir_arg n]) t p
      else t) t fresh_names)

(* [elim_body ~vars tg] expects the target [tg] to point to the labelled sequence.Then it will
    remove this sequence and its label and merge the trms inside this sequence with te ones of the
    sequence containing the labelled sequence. But before doing that, first a change of all the declared
    variables inside this sequence is performed. [vars] tells for the way the reanming is done.
    Either the user can give a list of variables together with their new names, or he can give the postfix
    after which shoudl be assigned to all the declared variables.
*)
let elim_body ?(vars : rename = AddSuffix "") (tg : Target.target) : unit =
  let tg_body = if List.mem Target.dBody tg then tg else (tg @ [Target.dBody]) in
  Variable_basic.rename vars tg_body;
  Sequence_basic.elim tg;
  Sequence_basic.delete [Target.cVarDef "__OPTITRUST__SAFE_ATTACH_"]

(* [bind ~fresh_name ~args tg] expectes the target [tg] to point to a function call, then
    it will just call bind args and bind_intro. Basically this function is used to save the user from
    entering both of them.
*)
let bind ?(fresh_name : string = "res") ?(args : var list = []) (tg : Target.target) : unit =
  bind_args args tg;
  Function_basic.bind_intro ~const:false ~fresh_name tg

(* [inline_call ~name_result ~label ~vars ~args ~no_control_structures tg]
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
int main2() { // Function_basic.bind_intro --> TODO: Function_core,
  int u = 1, v = 2, w = 3;
  int r = g(h(4), u, m(v, 2), (w + 1));
  int t = f(r);
}
int main3() { // Function.bind_args
  int u = 1, v = 2, w = 3;
  int a = h(4);
  int b = m(v, 2);
  int r = g(a, u, b, (w + 1));
  int t = f(r);
}
int main4() { // Function_basic.inline_call
  int u = 1, v = 2, w = 3;
  bool __OPTITRUST__SAFE_ATTACH_ = true; // LATER: ARTHUR investigate this
  int r;
  body : {
    int p = ((((a + a) + u) + b) + (w + 1));
    r = (p + p);
  }
  int t = f(r);
}
int main5() { // Function.elim_body
  int u = 1, v = 2, w = 3;
  int a = h(4);
  int b = m(v, 2);
  int r;
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


--

int r = f(2)
->
{
 ...code of f inlined
}@nobrace

--

int a = g(f(2))
->
mark: int r = f(2);
int a = g(r)

---
IDEALLY

int a = g(f(2); f(3))
->
{
  mark: int r1 = f(2)
  mark: int r2 = f(3)
  int a = g(r1,r2)
}->
{
  { mark_toinline: int r = ... code of f(2) }
  { code of f(3) }
  int a = g(r1,r2)
}->
{
  mark_toinline: int r = ...
  code of f(2)
  code of f(3)
  int a = g(r1,r2)
}

  WARNING: this code probably does not support nested calls, e.g. f(f(2))
*)
(* TODO: think about whether all transformations should offer the possibility
   leave nobrace sequences -->probably yes *)
let inline_call ?(name_result = "") ?(label:var = "__TEMP_body") ?(vars : rename = AddSuffix "1") ?(args : var list = []) (tg : Target.target) : unit =
  let t = Trace.get_ast() in
  (*  TODO:   in target: let cMark m = Constr_mark m

  let mark_bindings = Ast.next_mark() in
  Transfo.iter_targets_in_reverse_order tg (fun (p:path) ->
    bind_intro ~mark:mark_bindings p)
  let mark_toinline = Ast.next_mark() in
  Transfo.iter_targets_in_reverse_order [cMark mark_bindings] (fun (p:path) ->
      do the Function_basic.inline_call ~mark:mark_toinline
      // make sure to create them in nobrace sequences)))
  Transfo.iter_targets_in_reverse_order [cMark mark_toinline] (fun (p:path) ->
    init_attach p)
  *)
  (* TODO: probably not needed
  Internal.nobrace_remove_after (fun _ ->
    Transfo.iter_targets
    *)
(* TODO:
  let iter_targets_independently f =
    try while true do
       match Target.resolve_target tg t with
       | [] -> raise Break
       | p::_ -> f p
       end
    done with Break -> ()
  *)


  (*  TODO:
  Transfo.iter_targets tg (fun (p:path) ->
  *)

  (* TODO: this should be Target.apply_on_targets tg (fun t -> .. *)
  let tg_paths = Target.resolve_target tg t in
  List.iter (fun tg_path ->
  let name_result = ref name_result in
  let (path_to_seq,local_path, i) = Internal.get_call_in_surrounding_sequence tg_path in
  let path_to_instruction = path_to_seq @ [Dir_seq_nth i] in
  let (tg_trm, _) = Path.resolve_path (path_to_instruction @ local_path) t in
  let (tg_out_trm, _) = Path.resolve_path path_to_instruction t in
  Sequence_basic.intro_on_instr ~visible:false (Target.target_of_path (path_to_instruction));
  (* The full path has changed *)
  let tg_path = path_to_instruction @ [Dir_seq_nth 0] @ local_path in
  let tg = Target.target_of_path tg_path in
  let _res_inlining_needed =
    begin match tg_out_trm.desc with
    | Trm_let (_, (x, _), _) ->
      let init1 = get_init_val tg_out_trm in
      if !name_result <> "" && init1 = tg_trm then fail tg_trm.loc "inline_call: no need to enter the result name in this case"
        else if init1 = tg_trm then begin name_result := x; false end
        else
            begin match !name_result with
            | ""  ->  name_result := "__TEMP_Optitrust";
                      Function_basic.bind_intro ~fresh_name:!name_result tg ;true
            | _ -> Function_basic.bind_intro ~fresh_name:!name_result tg ;false
            end
    | Trm_apps _ -> false
    | _ -> fail None "inline_call: expected a variable declaration or a function call"
    end in
  if args <> [] then bind_args args tg else ();
  Function_basic.inline_call ~label tg;
  let no_control_structures =
    let nb_ctrl_path = Target.resolve_target_exactly_one [Target.cVarDef "__OPTITRUST__SAFE_ATTACH_"] (Trace.get_ast()) in
    let (nb_ctrl, _) = Path.resolve_path  nb_ctrl_path (Trace.get_ast()) in
    begin match (get_init_val nb_ctrl).desc with
    | Trm_val(Val_lit (Lit_bool b)) -> b
    | _ -> fail nb_ctrl.loc "inline_call: could't find the variable __OPTITRUST__SAFE_ATTACH_"
    end
    in
  elim_body ~vars [Target.cLabel label];
  if no_control_structures && (!name_result <> "")
    then
      begin
      let spec_target = ((Target.target_of_path path_to_instruction) @ [Target.cVarDef !name_result]) in
      Variable_basic.init_attach spec_target
      (* if res_inlining_needed then Variable_basic.inline ~delete:true spec_target else () *)
      end
    else ();
  Internal.nobrace_remove_and_exit()) tg_paths
