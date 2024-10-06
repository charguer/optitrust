


(*
   Remaining differences:
   - [ic/cn*cn + ic%cn] instead of [ic],
     could fix by arith simpl or with [PROJNM(...)] ops and simpls
   - [1..n] instead [of 0..(n-1)] loops, could fix by Loop.shift StartAtZero
   - [i++; i * 3|4|cn] instead of [i += 3|4|cn; i], could fix by Loop.scale
   - [k++; + k] instead of [k++, S++, D++], we may not want to introduce such pointer arithmetic.
   - no template support yet for S/ST types; OpenCV also casts inputs from uchar

cleanup of accesses

  !! Loop.shift_range (StartAtZero) [nbMulti; cFor "i"];
  !! Loop_basic.scale_range ~factor:(trm_int 3) [nbMulti; cIf ~cond:[sExpr "cn == 3"] (); dThen; cFor "i"];
*)
  (* loop scale / shift /  simpl ~unfold_alias:true *)
  (* Loop.scale_range ~factor:? [];
  Loop.shift_range ~factor:? []; *)

(* FIXME: removing cFor from specialize targets is not working, because we need to go inside seq. *)

 (* move Reduce.intro to after swap *)
  (*let mark_then (var, _value) = sprintf "%s" var in*)

(* DELETE reduce_elim in first branches *)
