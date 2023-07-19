open Syntax

(* [subst tm t]: find all the occurrences of variables in [t] and check if they belong to map [tm]
    if yes then assign its values otherwise do nothing *)
let rec subst (tm : tmap) (t : trm) : trm =
  let aux (t : trm) : trm =
    subst tm t in
  (* make a recursive call by removing from the map
    the keys that satisfy [f] *)
  let aux_filter (f : var -> bool)  (t : trm) : trm =
    let tm2 = Var_map.filter (fun k v -> not (f k)) tm in
    subst tm2 t in
  match t.desc with
  (* Hack to avoid unnecessary get operations when we substitute a variable occurrence with arbitrary code *)
  | Trm_var (vk, x) ->
    begin match Var_map.find_opt x.qvar_var tm with
    | Some t1 ->
      let t1 = {t1 with annot = t1.annot} in
      if (is_trm_arbit t1 && vk = Var_mutable) then trm_address_of t1 else t1
    | _ -> t
    end
  | Trm_seq ts ->
    let cur_tm = ref tm in
    let subst_item ti =
      begin match ti.desc with
      | Trm_let (_, (x, ty), tbody, _) ->
        let ti2 = subst !cur_tm ti in
        cur_tm := Var_map.filter (fun k _v -> k <> x) tm;
        ti2
      | Trm_let_fun (f, __retty, targs, tbody, _) ->
        cur_tm := Var_map.filter (fun k _v -> k <> f.qvar_var) tm;
        subst !cur_tm ti
      | _ -> subst !cur_tm ti
      end
      in
      let ts2 = Mlist.map subst_item ts in
      { t with desc = Trm_seq ts2}
  | Trm_for (l_range, _, _) ->
    let (index, _, _, _, _, _) = l_range in
    trm_map (aux_filter (fun x -> x = index)) t
  | Trm_for_c (init, _, _, _, _) ->
    let vs = vars_bound_in_trm_init init in
    trm_map (aux_filter (fun x -> List.mem x vs)) t
  | _ -> trm_map aux t


(* [subst x u t]: replace all the occurences of x with t *)
let subst_var (x : var) (u : trm) (t : trm) =
  let empty_tmap =  Var_map.empty  in
  let tmap = Var_map.add x u empty_tmap  in
  subst tmap t
