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
let bind_args (fresh_names : vars) : Target.Transfo.t =
 let counter = ref (-1) in
 Target.apply_on_transformed_targets (Internal.get_instruction_in_surrounding_sequence)
  (fun (p, p_local, i) t ->
   let path_to_call = p @ [Dir_seq_nth i] @ p_local in
   let call_trm,_ = Path.resolve_path path_to_call t in
   begin match call_trm.desc with
   | Trm_apps (_, tl) ->
    if List.length fresh_names = 0
      then begin Tools.printf "bind_args: no arguments to bind, no changes to be done\n"; t end (* LATER: check this *)
      else if List.length tl <> List.length fresh_names then
        fail call_trm.loc "bind_args: for each argument of the function call, there should be associated either an empty string or a variable to be bounded to"
      else begin
           Tools.foldi (fun n t fresh_name ->
            if fresh_name <> "" then
            let () = incr counter in
            Function_core.bind_intro (i + !counter) fresh_name false (p_local @ [Dir_arg n]) t p
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
  Variable_basic.rename_on_block vars tg;
  Sequence_basic.elim tg

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


// NOTE: if we want to optimize, we could instead of
// using ~[cMark mymark] use ~((target_of_path p)++[cMark mymark])
// where p is the path to the englobing sequence.
*)
let inline ?(name_result = "") ?(body_mark : mark = "__TEMP_body") ?(vars : rename = AddSuffix "1") ?(args : vars = []) (tg : Target.target) : unit =
  Target.iteri_on_targets (fun i t p ->
    let name_result = ref name_result in
    let (path_to_seq,local_path, i1) = Internal.get_instruction_in_surrounding_sequence p in
    let path_to_instruction = path_to_seq @ [Dir_seq_nth i1] in
    let (tg_trm, _) = Path.resolve_path (path_to_instruction @ local_path) t in
    let (tg_out_trm, _) = Path.resolve_path path_to_instruction t in
    let my_mark = "__inline" ^ "_" ^ (string_of_int i) in
    let res_inlining_needed =
    begin match tg_out_trm.desc with
    | Trm_let (_, (x, _), init) ->
      let init1 = match get_init_val init with 
      | Some init1 -> init1
      | None -> fail t.loc "inline: coudl not get the target to the function call" in
      if !name_result <> "" && init1 = tg_trm then fail tg_trm.loc "inline: no need to enter the result name in this case"
        else if 
          Internal.same_trm init1  tg_trm then 
          begin 
          name_result := x; false end
        else
            begin match !name_result with
            | ""  ->  name_result := "__TEMP_Optitrust";
                      Function_basic.bind_intro ~my_mark ~fresh_name:!name_result (Target.target_of_path p);true
            | _ -> Function_basic.bind_intro ~my_mark ~fresh_name:!name_result (Target.target_of_path p);true
            end
    | Trm_apps (_f, [_; rs]) when is_set_operation tg_out_trm -> 
      if Internal.same_trm rs tg_trm then
        begin match !name_result with
            | ""  ->  name_result := "__TEMP_Optitrust";
                      Function_basic.bind_intro ~my_mark ~fresh_name:!name_result (Target.target_of_path p);true
            | _ -> Function_basic.bind_intro ~my_mark ~fresh_name:!name_result (Target.target_of_path p);true
            end else false
    | Trm_apps _ -> false 
    | _ -> fail None "inline: expected a variable declaration or a function call"
    end in
    let new_target = [Target.cMark my_mark] in
    if not res_inlining_needed then Marks.add my_mark (Target.target_of_path p);
    if args <> [] then bind_args args new_target else ();
    Function_basic.inline ~body_mark new_target;
    elim_body ~vars [Target.cMark body_mark];
    if !name_result <> "" then begin
        let () = try Variable_basic.init_attach (new_target) with
           | Variable_basic.Init_attach_no_occurrences
           | Variable_basic.Init_attach_occurrence_below_control -> ()
           | e -> raise e in
        if res_inlining_needed then Variable_basic.inline new_target;
        Marks.remove my_mark ([Target.nbAny] @ new_target)
    end;

  ) tg;
  
