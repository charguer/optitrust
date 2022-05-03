open Ast
include Sequence_basic

(* [intro ~start ~stop ~nb ~on ~mark ~visible]: this is a high level function for inserting a subsequnece
    inside another sequence.
     [start] - denotes the target for the starting point of the sub-sequnece, it should be used in conjunction with
      [nb] or [end]
     [end] - denotes the target for the end point of the sub-sequnece, it should be used in conjunction with
      [nb] or [start]
    [nb] - in the case when the user does not give the target of  the end point of the sequence he can give as
      argument the number of instruction to include comming after [start] or [end]. If used with [start] the sign
      of [nb] should be poistive otherwise it should be negative.
    [on] - denotes a single target to be isolated inside the sub-sequence. When [on] i used all the other
      except mark and visible shoold be left empty *)
let intro ?(start : Target.target = []) ?(stop : Target.target = []) ?(nb : int = 0)
  ?(on : Target.target = []) ?(mark : string = "") ?(visible : bool = true) () : unit =
  match on with
  | [_] ->  if (start = [] && stop = [] && nb = 0) then Sequence_basic.intro_on_instr ~mark ~visible on else ()
  | _ ->  begin match nb with
          | 0 -> if (start <> [] && stop <> [])
                  then Sequence_basic.intro_between ~mark start stop
                  else begin match start, stop with
                       | _, [] -> Sequence_basic.intro_after ~mark start
                       | [], _ -> Sequence_basic.intro_before ~mark stop
                       | _,_ -> fail None "Sequence.intro: can't provide both the start and stop and the number of instruction to include inside the sequence"
                       end
          | _ -> begin match start, stop with
                | _, [] -> Sequence_basic.intro ~mark nb start
                | [], _ -> Sequence_basic.intro ~mark (-nb) stop
                | _,_ -> fail None "Sequence.intro: can't provide both the start and stop and the number of instruction to include inside the sequence"
                end
         end


(* [intro_targets tg]: expects the target [tg] to point at one or more consecutive instuctions
      then it will introduce a sequence that contains those instructions. *)
let intro_targets ?(mark : string = "")(tg : Target.target) : unit =
  let nb_targets = ref 0 in
  let prev_index = ref (-1) in
  let first_target = [Target.occFirst] @ (Target.filter_constr_occurrence tg) in
  let surrounding_seq = ref [] in
  let tg = Target.enable_multi_targets tg in
  Target.iteri_on_targets (
    fun i t p ->
      let path_to_seq, index = Internal.isolate_last_dir_in_seq p in
      if i = 0 then surrounding_seq := path_to_seq
        else if !surrounding_seq <> path_to_seq then fail t.loc "Sequence.intro_targets: all the targeted instructions should belong to the same scope and be consecutive";
      if index <> !prev_index + 1 && !prev_index <> -1 then fail t.loc "Sequence.intro_targets: all the targeted instructions should be consecutive";
      incr nb_targets;
  ) tg;
  if !nb_targets < 1 then fail None "Sequence.intro_targets: expected at least 1 instruction";
  Sequence_basic.intro ~mark !nb_targets first_target

(* [apply ~start ~stop ~nb f]: invokes [f mark] where the [mark] is attached to a temporary sequence created
   by [Sequence.intro ~start ~stop ~nb]. This sequence is eliminated immediately afterwards. *)
let apply ?(start : Target.target = []) ?(stop : Target.target = []) ?(nb : int = 0) (f : mark -> unit) : unit =
  let mark = Mark.next () in
  intro ~mark ~start ~stop ~nb ();
  f mark;
  elim [Target.cMark mark]
