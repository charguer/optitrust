open Ast
(* [intro ~start ~stop ~nb ~on ~label ~visible]: this is a high level function for inserting a subsequnece
    inside another sequence. 
     [start] - denotes the target for the starting point of the sub-sequnece, it should be used in conjunction with 
      nb or [end]
     [end] - denotes the target for the end point of the sub-sequnece, it should be used in conjunction with 
      nb or [start]
    [nb] - in the case when the user does not give the target of  the end point of the sequence he can give as
      argument the number of instruction to include comming after [start] or [end]. If used with [start] the sign
      of [nb] should be poistive otherwise it should be negative.
    [on] - denotes a single target to be isolated inside the sub-sequence. When [on] i used all the other
      except label and visible shoold be left empty
*)
let intro ?(start : Target.target = []) ?(stop : Target.target = []) ?(nb : int = 0) 
  ?(on : Target.target = []) ?(label : string = "") ?(visible : bool = true) () : unit =
  match on with
  | [_] ->  if (start = [] && stop = [] && nb = 0) then Sequence_basic.intro_on_instr ~label ~visible on else ()
  | _ ->  begin match nb with 
          | 0 -> Sequence_basic.intro_between ~label start stop
          | _ -> begin match start, stop with
                | _, [] -> Sequence_basic.intro ~label nb start
                | [], _ -> Sequence_basic.intro ~label (-nb) stop
                | _,_ -> fail None "intro: can't enter both the start and stop and the number of instruction to include inside the sequence" 
                end
          end  
