open Ast
open Path

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
        Function_core.bind_intro (i + !counter)  fresh_name true (p_local @ [Dir_arg n]) t p
      else t) t fresh_names)

(* [elim_body ~renames tg] expects the target [tg] to point to the labelled sequence.Then it will
    remove this sequence and its label and merge the trms inside this sequence with te ones of the
    sequence containing the labelled sequence. But before doing that, first a change of all the declared
    variables inside this sequence is performed. [renames] tells for the way the reanming is done.
    Either the user can give a list of variables together with their new names, or he can give the postfix
    after which shoudl be assigned to all the declared variables.
*)
let elim_body ?(renames : rename = Postfix "") (tg : Target.target) : unit =
  let tg_body = if List.mem Target.dBody tg then tg else (tg @ [Target.dBody]) in
  Variable_basic.rename renames tg_body;
  Sequence_basic.elim tg

(* [bind ~fresh_name ~inner_fresh_names tg] expectes the target [tg] to point to a function call, then
    it will just call bind args and bind_intro. Basically this function is used to save the user from
    entering both of them.
*)
let bind ?(fresh_name : string = "res") ?(inner_fresh_names : var list = []) (tg : Target.target) : unit =
  bind_args inner_fresh_names tg;
  Function_basic.bind_intro ~const:false ~fresh_name tg

(* [inline_call ~name_result ~label ~renames ~inner_fresh_names ~no_control_structures tg]
      expects the target tg to point to point to a function call. And automates completely the process
      of function call inlining.
*)
let inline_call ?(name_result = "") ?(label:var = "__TEMP_body") ?(renames : rename = Postfix "1") ?(inner_fresh_names : var list = []) ?(_no_control_structures : bool = true) (tg : Target.target) : unit =
  let t = Trace.get_ast() in
  let name_result = ref name_result in
  let tg_path = Target.resolve_target_exactly_one tg t in
  let (path_to_seq,local_path, i) = Internal.get_call_in_surrounding_sequence tg_path in
  let (tg_trm, _) = Path.resolve_path (path_to_seq @ [Dir_seq_nth i] @ local_path) t in
  let (tg_out_trm, _) = Path.resolve_path (path_to_seq @ [Dir_seq_nth i]) t in
  let res_inlining_needed =
    begin match tg_out_trm.desc with
    | Trm_let (_, (x, _), _) ->
      let init1 = get_init_val tg_out_trm in
      if !name_result <> "" && init1 = tg_trm then fail tg_trm.loc "inline_call: no need to enter the result name in this case"
        else if init1 = tg_trm then begin name_result := x; false end
        else
            begin match !name_result with
            | ""  ->  name_result := "__TEMP_Optitrust";
                      Function_basic.bind_intro ~fresh_name:!name_result tg;true
            | _ -> Function_basic.bind_intro ~fresh_name:!name_result tg;false
            end
    | Trm_apps _ -> false
    | _ -> fail None "inline_call: expected a variable declaration or a function call"
    end in
  if inner_fresh_names <> [] then bind_args inner_fresh_names tg else ();
  Function_basic.inline_call ~label tg;
  elim_body ~renames [Target.cLabel label];
  if _no_control_structures && (!name_result <> "")
    then
      begin
      Variable_basic.init_attach [Target.cVarDef !name_result];
      if res_inlining_needed then Variable_basic.inline ~delete:true [Target.cVarDef !name_result] else ()
      end
    else ()



