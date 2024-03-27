open Prelude
open Target
include Specialize_basic

(* TODO: generalize to any condition, e.g. (Trm.Cmp (var "x") (lit 1)) *)
(** [variable]: adds a specialized execution path when [var == value]. *)
let%transfo variable ~(var : string) ~(value : trm)
  ?(mark_then : mark = no_mark) ?(mark_else : mark = no_mark)
  (tg : target) : unit =
  let var = find_var_in_current_ast ~target:tg var in
  Marks.with_marks (fun next_mark ->
    let mark_then = Mark.reuse_or_next next_mark mark_then in
    let mark_else = Mark.reuse_or_next next_mark mark_else in
    Target.iter (fun p ->
      let cond = trm_eq (trm_var var) value in
      If.insert ~cond ~mark_then ~mark_else (target_of_path p);
      Trace.without_substep_validity_checks (fun () ->
        (* TODO: we should know that [var = value] in the then branch. *)
        Variable.subst ~subst:var ~put:value [cMark mark_then]);
    ) tg)

(** [variable_multi]: repeats [variable] transfo to create multiple specialized paths.

    - [mark_then]: function to create a mark based on [var, value]
    *)
let%transfo variable_multi ?(mark_then : (string * trm) -> mark = fun _ ->  no_mark)
  ?(mark_else : mark = no_mark)
  (pairs: (string * trm) list) (tg : target) : unit =
  Marks.with_marks (fun next_mark ->
    let generic_mark = Mark.reuse_or_next next_mark mark_else in
    Marks.add generic_mark tg;
    let specialize (var, value) = begin
      let this_mark_then = next_mark () in
      variable ~var ~value ~mark_then:this_mark_then [cMark generic_mark];
      Marks.remove generic_mark [cMark this_mark_then; cMark generic_mark];
      let mark_then = mark_then (var, value) in
      Marks.add mark_then [cMark this_mark_then];
    end in
    List.iter specialize pairs;
  )

let function_arg (spec_name : string) (args_to_keep : bool list) (tg : target) : unit =
  Ast_data.fill_fun_defs_tbl (get_ast());
  iter_on_targets (fun t p ->
    let tg_trm = Path.resolve_path p t in
    match tg_trm.desc with
    | Trm_apps ({desc = Trm_var (_, qf)} as call, args, _) ->
      let opt_trms = List.map2 (fun arg arg_k ->
        if not arg_k then Some arg else None
      ) args args_to_keep in



      let call_clang_id = Ast_data.get_cursor_of_trm_unsome call in

      let clang_id =
        match Ast_data.get_cursor_of_trm call with
        | Some clang_id  ->
          Some (Clang.get_cursor_referenced call_clang_id)
        | None -> None
          in

      Specialize_basic.fundefs spec_name opt_trms [cTopFunDef ?clang_id qf.name];
      (* FIXME: #var-id,
          1. recover var from previous transfo?
          2. create var before previous transfo? *)
      let spec_var = find_var_in_current_ast spec_name in
      Specialize_basic.funcalls spec_var args_to_keep (target_of_path p);

    | _ -> trm_fail tg_trm "Specialize.function_arg: expected a target to a function call."

  ) tg
