include Rewrite_basic
open Syntax
open Target

(* FIXME: move somewhere else *)
let no_simpl (tg : target) : unit = ()

let%transfo equiv_at ?(simpl : Transfo.t = no_simpl) ?(glob_defs : string = "") ?(ctx : bool = false) ?(indepth : bool = false) (rule : string) (tg : target) : unit =
  Target.reparse_after ~reparse:true (fun tg ->
  Marks.with_fresh_mark (fun mark ->
    Rewrite_basic.equiv_at ~mark:(Some mark) ~glob_defs ~ctx ~indepth rule tg;
    simpl [cMark mark]
  )) tg