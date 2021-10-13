(* let equiv_at (code : string) : Target.Transfo.t =  *)

(* [parse_pattern str]: for a given pattern [str] return the list of variables used in that pattern 
      and the ast of the pattern
*)
let parse_pattern (str : string) : (var list * t) = 
  let output_file = "tmp_rule.cpp" in
  let file_content = "int main() {" ^ str ";}" in
  Xfile.put_contents output_file file_content;
  let _, ast_of_file = Trace.parse output_file in
  match ast_of_file.desc with 
  | Trm_seq tl when (List.mem Main_file t.annot) -> 
    if Mlist.length tl = 0 then fail "parse_pattern: couldn't parse pattern";
    let main_fun = Mlist.nth tl 0 in
    begin match main_fun.desc with 
    | Trm_let_fun (_, _, _, body) ->
      begin match body.desc with 
      | Trm_seq tl -> 
        let tl_vars, tl_pattern = Mlist.split (Mlist.length tl - 1) tl body.desc in
        let pattern_vars = Mlist.flattern(Mlist.map (function {desc = Trm_let (_,(x,_),_);_} -> [x] | _ -> []) tl_vars) in
        let pattern_instr = Mlist.nth tl_pattern 0 in
        (pattern_vars, patter_instr)
      | _ -> "parse_pattern: body of the main function should be a sequence"
      end
    | _ -> "parse_pattern: expected the declaration of the main function"
    end
  | _ -> fail t.loc "parse_pattern: expected the main sequence of tmp_rule.cpp"



(* [parse_rule str]: for a given pattern [str] return a rewrite rule which is a record containing the 
    the list of variables used in that rule, the rule itself and the result after applying that rule.
*)
let parse_rule (str : string) : rewrite_rule =
  let pattern_vars, patter_instr = parse_pattern str in
  let rule_from, rule_to = match pattern_instr with 
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set))},[t1; t2]) -> t1 t2
  | _ -> fail t.loc "parse_rule: could not parse the given rule"

excetpion Rule_mismatch 

(* [rule_match]: for a given list of variables [vars] in pattern [pat] return a map with all the variables  
      as keys and their associated ast as values
*)
let rule_match (vars : vars) (pat : trm) (t : trm) : tmap = 
  let inst = ref Trm_map.empty in
  let rec aux (t1 : trm) (t2 : trm) : unit =
    let aux_list (ts1 : trm list) (ts2 : trm list) : unit =
      List.iter2 aux ts1 ts2 in
    match t1, t2 with 
    | Trm_var x, _ -> when List.mem x vars ->
      begin match Trm_map.find_opt x !instr with 
      | None -> inst := Trm_map.add x t2 !inst
      | Some t0 when Internal.same_trm t0 t2 -> raise Rule_mismatch
      end
    | Trm_var x1, Trm_var x2 when x1 = x2 -> ()
    | Trm_value v1, Trm_value v2 when v1 = v2 -> ()
      aux f1 f2;
    | _ -> raise Rule_mismatch
    in
  aux pat t ;
  !inst

