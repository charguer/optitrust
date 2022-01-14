open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Variable_basic.simpl_deref [dRoot];
  )
"
int main() {
  int a = 1;
  int b = *(&a);
  int* p = &a;
  int* q = &(*p);
}
"
 (* LATER: replace [dRoot] with [] above *)


let _ = Run.script_cpp (fun _ ->
  (* !! Variable_basic.simpl_deref [dRoot]; --uncomment to simplify all *)

  !! Variable_basic.simpl_deref [cRead ~addr:[cVar "b"] ()];
  !! Variable_basic.simpl_deref [cRead ~addr:[cVar "a"] ()];

)

(* TODO

  simpl_deref  should be performing the simplification only exactly on the target.
  simpl_deref ~indepth:true   should be used for simplifying recursively

you can organize the code as follows:

in core level:

let simpl_deref_opt (t : trm) : trm option =
  match t.desc with
  | Trm_apps (op, [t1]) ->
    (* First case  &* both & and * are encoded as annotations of t*)
    if List.mem Address_operator t.add && List.mem Star_operator t.add then
    let new_add = List.filter (function |Address_operator | Star_operator -> false) t.add in
    Some {t with add = new_add}
    (* Second case: * is a get operation and & is annotation encode inside  t1 *)
    else if op.desc = Trm_val (Val_prim (Prim_unop Unop_get))  && List.mem Address_operator t1.add then
      begin
      let new_t1 = {t1 with add = []} in
      Some (trm_get ~annot:[Mutable_var_get] new_t1)
      end
    else None
  | _ -> None

let simpl_deref_at (t : trm) : trm =
    match simpl_deref_opt t with
    | None -> if allow_identity then t else fail None "simpl_deref: not a &(*t) or *(&t) expression"
    | Some t2 -> t2

let simpl_deref_indepth (t : trm) : trm =
  let rec aux (t : trm) : trm =
    | Trm_apps (op, [t1]) ->
        begin match simpl_deref_at t with
        | Some t2 -> trm_map aux t2 (* t2 should be strictly simpler than t, so we make progress *)
        | None -> trm_map aux t
        end
    | _ -> trm_map aux t
  in
  aux t


in basic level:

let simpl_deref ?(indepth:bool=true) (tg : Target.t) : unit =
  let tg = if tg = [] then [cRoot] else tg in
  let tr = Variable_core.(if indepth then simpl_deref_at else simpl_deref_indepth) in
  Target.apply_on_targets tg tr

in unit tests:

  simpl_deref []
    => simplifies everywhere

  simpl_deref [cVarDef "x"]
    ==> simplifies everywhere inside the def of x

  simpl_deref ~indepth:false [cFunDef "f"; cRead ()]
    ==> simplifies only at the read inside "f"


  New convention: all functions named with "simpl_"  take an ~indepth whose default value in false.
  => beta should be renamed to simpl_beta, and take indepth with true as default
  => infix_ops should follow the same scheme as suggested above for simple_defer

  Motivation: the exact targets of simplification operations are usually numerous and hard to give a target expression for;
  thus in general we want to target a surrounding statement, or even possibly the root.
*)
