open Prelude
open Target
include Sequence_basic

(** [intro ~start ~stop ~nb ~on ~mark ~visible]: this is a high level function for inserting a subsequnece
    inside another sequence.
     [start] - denotes the target for the starting point of the sub-sequence, it should be used in conjunction with
              [nb] or [end].
     [end] - denotes the target for the end point of the sub-sequnece, it should be used in conjunction with
             [nb] or [start].
    [nb] - in the case when the user does not give the target of  the end point of the sequence he can give as
      argument the number of instruction to include comming after [start] or [end]. If used with [start] the sign
      of [nb] should be poistive otherwise it should be negative.
    [on] - denotes a single target to be isolated inside the sub-sequence. When [on] is used all the other
      except mark and visible shoold be left empty. *)
let%transfo intro ?(start : target = []) ?(stop : target = []) ?(nb : int = 0)
  ?(on : target = []) ?(mark : string = no_mark) ?(visible : bool = true) (_u : unit) : unit =
  match on with
  | [_] ->  if (start = [] && stop = [] && nb = 0) then Sequence_basic.intro_on_instr ~mark ~visible on else ()
  | _ ->  begin match nb with
          | 0 -> if (start <> [] && stop <> [])
                  then Sequence_basic.intro_between ~mark start stop
                  else begin match start, stop with
                       | _, [] -> Sequence_basic.intro_after ~mark start
                       | [], _ -> Sequence_basic.intro_before ~mark stop
                       | _,_ -> failwith "Sequence.intro: can't provide both the start and stop and the number of instruction to include inside the sequence"
                       end
          | _ -> begin match start, stop with
                | _, [] -> Sequence_basic.intro ~mark nb start
                | [], _ -> Sequence_basic.intro ~mark (-nb) stop
                | _,_ -> failwith "Sequence.intro: can't provide both the start and stop and the number of instruction to include inside the sequence"
                end
         end


(** [intro_targets tg]: expects the target [tg] to point at one or more consecutive instuctions
      then it will introduce a sequence that contains those instructions. *)
let%transfo intro_targets ?(mark : string = no_mark)(tg : target) : unit =
  let nb_targets = ref 0 in
  let prev_index = ref (-1) in
  let first_target = [Target.occFirst] @ (Target.filter_constr_occurrence tg) in
  let surrounding_seq = ref [] in
  let tg = Target.enable_multi_targets tg in
  Target.iteri (
    fun i p ->
      let path_to_seq, index = Internal.isolate_last_dir_in_seq p in
      if i = 0 then surrounding_seq := path_to_seq
        else if !surrounding_seq <> path_to_seq then path_fail p "Sequence.intro_targets: all the targeted instructions should belong to the same scope and be consecutive";
      if index <> !prev_index + 1 && !prev_index <> -1 then path_fail p "Sequence.intro_targets: all the targeted instructions should be consecutive";
      incr nb_targets;
  ) tg;
  if !nb_targets < 1 then failwith "Sequence.intro_targets: expected at least 1 instruction";
  Sequence_basic.intro ~mark !nb_targets first_target

(** [elim tg]: expects the target [tg] to point at a sequence that appears nested inside another sequence and inline the elements of that nested sequence in its parent.

  Example: if [tg] points at [{ t2; t3 }] inside [{ t1; { t2; t3 }; t4 }], it "elims" the contents of the inner sequence, producing e.g., [{ t1; t2; t3; t3 }].

  If the sequence appears behind a let, use the sequence result to bind the value:
    if [tg] points at [{ t2; let v = e; t3; v}] inside [{ t1; let x = { t2; let v = e; t3; v }; t4 }], [elim] produces [{ t1; t2; let x = e; t3; t4}].

  If the sequence appears behind other nodes, first perform a Variable.bind and then inlines the generated sequence behind a let.
  In that case, if [res_name] is given, it becomes the new name of the result of the eliminated sequence.
  Else the result of the eliminated sequence is inlined.
  *)
let%transfo elim ?(resname : string = "") (tg : target) : unit =
  Target.iter (fun p ->
    let path_to_seq, local_path, index = Internal.get_instruction_in_surrounding_sequence p in
    match local_path with
    | [] -> Sequence_basic.elim_instr (target_of_path p)
    | [Dir_let_body] -> Sequence_basic.elim_let [cPath path_to_seq; dSeqNth index]
    | _ ->
      Marks.with_marks (fun gen_mark ->
        let mark_let = gen_mark () in
        let mark_result = gen_mark () in
        let resvar = if resname = "" then "TEMP_res" else resname in
        Variable_basic.bind ~const:true ~mark_let resvar (target_of_path p);
        Sequence_basic.elim_let ~mark_result [cMark mark_let];
        if resname = "" then Variable_basic.inline [cMark mark_result]
      )
    ) tg

(** [apply ~start ~stop ~nb f]: invokes [f mark] where the [mark] is attached to a temporary sequence created
   by [Sequence.intro ~start ~stop ~nb]. This sequence is eliminated immediately afterwards. *)
let apply ?(start : target = []) ?(stop : target = []) ?(nb : int = 0) (f : mark -> unit) : unit =
  let mark = Mark.next () in
  intro ~mark ~start ~stop ~nb ();
  f mark;
  elim [Target.cMark mark]

let rec process (r: trm mlist ref) (t:trm) : trm=
  let aux t = process r t in
  match trm_seq_inv t with
  | Some(tl, res_var) -> r:= Mlist.merge tl !r; Option.map_or (fun var -> trm_var var) trm_dummy res_var
  | None -> trm_map aux t
let rec pull_nested_seq_on  ~(recursively:bool) (t:trm) : trm =
(* must be called on a statement in a sequence *)
   let r = ref Mlist.empty in
   let t' = process r t in
   let instrs = Mlist.rev !r in
   let instrs = if not recursively then instrs else
      Mlist.map (fun instr -> pull_nested_seq_on ~recursively instr) instrs in
   trm_seq_nobrace (Mlist.merge instrs (Mlist.of_list [t']))
let%transfo pull_nested_seq ~(indepth:bool) (tg:target) =
apply_at_target_paths (fun t -> maybe_trm_bottom_up_try indepth (pull_nested_seq_on ~recursively:true) t)  tg
