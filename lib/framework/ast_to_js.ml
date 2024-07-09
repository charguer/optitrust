(* BROKEN: Json.Raw replaces Json.Str *)

(* A module for creating a json view of OptiTrust ast *)

open Prelude
open Ast_to_text
open PPrint

(** [json]: alias for Json type *)
type json = Json.t
type t = json

(** [quote]: alias for Json.quote *)
let quote = Json.quote

(** [strquote]: alias for Json.strquote *)
let strquote = Json.str

(** [nodeid]: IDs to be assigned to each ast trm *)
type nodeid = int

(** [nodeid_invalid]: ID assigned to the root of the ast *)
let nodeid_invalid = (-1)


(** [typ_to_json typ]: converts an OptiTrust type to a Json object  *)
let typ_to_json (typ : typ) : t =
  Str (Tools.document_to_string (bquotes (Ast_to_c.(typ_to_doc (default_style ()) typ))))

(** [typdef_to_json td]: converts an OptiTrust typedef to a json object *)
let typdef_to_json (td : typedef) : t =
  Str (Tools.document_to_string (bquotes (Ast_to_c.(typedef_to_doc (default_style ())) td)))

(** [json_to_js]: creates a javascript variable of type json object from ast [j] *)
let json_to_js (index : int) (j : t) : document =
  let json_ast = Json.to_doc j in
  string "contents" ^^ brackets (string(string_of_int index)) ^^ equals ^^ json_ast ^^ semi ^^ hardline

(** [code_to_js]: creates a javascript variable of type string representing the source code src *)
let code_to_js (out : out_channel) (index : int) (src : string) : unit =
  let doc = string "source" ^^ brackets (string (string_of_int index)) ^^ equals ^^ bquotes (string src) ^^ hardline in
  ToChannel.pretty 0.9 80 out doc

(** [loc_to_json t]: converts trm location to a Json object *)
let loc_to_json (t : trm) : json =
  begin match t.loc with
  | None -> strquote ""
  | Some {loc_file = _; loc_start = {pos_line = start_row; pos_col = start_column}; loc_end = {pos_line = end_row; pos_col = end_column}} ->
      Json.Obj [
        (strquote "start", Json.Obj
          [ (strquote "line" , Json.Int start_row);
            (strquote "col", Json.Int start_column)] );
        (strquote "end", Json.Obj[
           (strquote "line", Json.Int end_row);
           (strquote "col", Json.Int end_column)] )]
  end

(** [typd_var_lis_to_json ty]: converts a list of typed vars to a Json object *)
let typed_var_list_to_json (tv : typed_vars) : json =
  (* TODO: #var-id , also encode namespaces and id ? *)
  Json.Obj (List.map (fun (v,typ) -> (strquote v.name, typ_to_json typ)) tv)


(** [child_to_json label child_id]: auxiliary function for converting trm child nodes to Json objects *)
let child_to_json (label : string) (child_id : nodeid) : json =
  Json.Obj [ ((strquote "label"), strquote label);
                ((strquote "id", Json.Int child_id)) ]

(** [ichild_to_json ~prefix i child_id]: similar to [child_to_json] but this one adds labels based on the child indicces *)
let ichild_to_json ?(prefix:string="") (i : int) (child_id : nodeid) : json =
  child_to_json (prefix ^ string_of_int i) (child_id)


(** [childre_to_field children]: converts a list of children to a json field  *)
let children_to_field (children: json list) : (json * json) =
  (strquote "children", Json.List children)

(** [kind_to_field kind]: converts a trm kind to a json field *)
let kind_to_field (kind : string) : json * json =
  (strquote "kind", strquote kind)

(** [value_to_field value]: converts an OptiTrust [value] to a json field *)
let value_to_field (value : string) : json * json =
  (strquote "value", strquote value)

(** [directive_to_json directive]: converts an OptiTrust OpenMP directive to a json field *)
let directive_to_json (directive : directive) : json * json =
  let directive_js =
  match directive with
  | Atomic _-> strquote "Atomic"
  | Atomic_capture -> strquote "Atomic_capture"
  | Barrier -> strquote "Barrier"
  | Cancel _ -> strquote "Cancel"
  | Cancellation_point _-> strquote "Cancellation_point"
  | Critical _ -> strquote "Critical"
  | Declare_simd _ -> strquote "Declare_simd"
  | Declare_reduction _ -> strquote "Declare_reduction"
  | Declare_target _ -> strquote "Declare_target"
  | Distribute _ -> strquote "Distribute"
  | Distribute_parallel_for _ -> strquote "Distribute_parallel_for"
  | Distribute_parallel_for_simd _ -> strquote "Distribute_parallel_for_simd"
  | Distribute_simd -> strquote "Distribute_simd"
  | End_declare_target -> strquote "End_declare_target"
  | Flush _ -> strquote "Flush"
  | For _ -> strquote "For"
  | For_simd _ -> strquote "For_simd"
  | Master -> strquote "Master"
  | Ordered _ -> strquote "Ordered"
  | Parallel _ -> strquote "Parallel"
  | Parallel_for _ -> strquote "Parallel_for"
  | Parallel_for_simd _ -> strquote "Parallel_for_simd"
  | Parallel_sections _ -> strquote "Parallel_sections"
  | Section -> strquote "Section"
  | Sections _ -> strquote "Sections"
  | Simd _ -> strquote "Simd"
  | Single _ -> strquote "Single"
  | Target _ -> strquote "Target"
  | Target_data _ -> strquote "Target_data"
  | Target_enter_data _ -> strquote "Target_enter_data"
  | Target_exit_data _ -> strquote "Target_exit_data"
  | Target_teams _ -> strquote "Target_teams"
  | Target_teams_distribute _ -> strquote "Target_teams_distribute"
  | Target_teams_distribute_parallel_for _ -> strquote "Target_teams_distribute_parallel_for"
  | Target_teams_distribute_parallel_for_simd _ -> strquote "Target_teams_distribute_parallel_for_simd"
  | Target_teams_distribute_simd _ -> strquote "Target_teams_distribute_simd"
  | Target_update _ -> strquote "Target_update"
  | Task _ -> strquote "Task"
  | Taskgroup -> strquote "Taskgroup"
  | Taskloop _ -> strquote "Taskloop"
  | Taskloop_simd _ -> strquote "Taskloop_simd"
  | Taskwait _ -> strquote "Taskwait"
  | Taskyield -> strquote "Taskyield"
  | Teams _ -> strquote "Teams"
  | Teams_distribute _ -> strquote "Teams_distribute"
  | Teams_distribute_end _ -> strquote "Teams_distribute_end"
  | Teams_distribute_parallel_for _ -> strquote "Teams_distribute_parallel_for"
  | Teams_distribute_parallel_for_simd _ -> strquote "Teams_distribute_parallel_for_simd"
  | Threadprivate _ -> strquote "Threadprivate"
  in (strquote "directive", directive_js)


(** [directive_to_json directive]: converts an OptiTrust OpenMP routine to a json field *)
let routine_to_json (routine : omp_routine) : json * json =
  let routine_js =
  match routine with
  | Set_num_threads _ -> strquote "Set_num_threads"
  | Get_num_threads -> strquote "Get_num_threads"
  | Get_max_threads -> strquote "Get_max_threads"
  | Get_thread_num -> strquote "Get_thread_num"
  | Get_num_procs -> strquote "Get_num_procs"
  | In_parallel -> strquote "In_parallel"
  | Set_dynamic _ -> strquote "Set_dynamic"
  | Get_dynamic -> strquote "Get_dynamic"
  | Get_cancellation -> strquote "Get_cancellation"
  | Set_nested _ -> strquote "Set_nested"
  | Get_nested -> strquote "Get_nested"
  | Set_schedule _ -> strquote "Set_schedule"
  | Get_schedule _ -> strquote "Get_schedule"
  | Get_thread_limit -> strquote "Get_thread_limit"
  | Set_max_active_levels _ -> strquote "Set_max_active_levels"
  | Get_max_active_levels -> strquote "Get_max_active_levels"
  | Get_level -> strquote "Get_level"
  | Get_ancestor_thread_num -> strquote "Get_ancestor_thread_num"
  | Get_team_size _ -> strquote "Get_team_size"
  | Get_active_level -> strquote "Get_active_level"
  | In_final -> strquote "In_final"
  | Get_proc_bind -> strquote "Get_proc_bind"
  | Set_default_device _ -> strquote "Set_default_device"
  | Get_default_device -> strquote "Get_default_device"
  | Get_num_devices -> strquote "Get_num_devices"
  | Get_num_teams -> strquote "Get_num_teams"
  | Get_team_num -> strquote "Get_team_num"
  | Is_initial_device -> strquote "Is_initial_device"
  | Init_lock _ -> strquote "Init_lock"
  | Init_nest_lock _ -> strquote "Init_nest_lock"
  | Destroy_lock _ -> strquote "Destroy_lock"
  | Destroy_nest_lock _ -> strquote "Destroy_nest_lock"
  | Set_lock _ -> strquote "Set_lock"
  | Set_nest_lock _ -> strquote "Set_nest_lock"
  | Unset_lock _ -> strquote "Unset_lock"
  | Unset_nest_lock _ -> strquote "Unset_nest_lock"
  | Test_lock _ -> strquote "Test_lock"
  | Test_nest_lock _ -> strquote "Test_nest_lock"
  | Get_wtime -> strquote "Get_wtime"
  | Get_wtick -> strquote "Get_wtick"
  in (strquote "routine", routine_js)


(** [node_to_js aux t]: based on the description of [t] convert [t] to a pair of json objects(field, value),
     [aux] is goign to be applied for processing chidlren nodes *)
let node_to_js (aux : trm -> nodeid) (t : trm) : (json * json) list =
    match t.desc with
    | Trm_val v ->
        [ kind_to_field "val";
          (strquote "value", Json.str (Tools.document_to_string (PPrint.bquotes (Ast_to_c.(val_to_doc (default_style())) (trm_get_cstyles t) t.typ v))));
          children_to_field [] ]
    | Trm_var x ->
        [ kind_to_field "var";
          value_to_field x.name; (* TODO: #var-id , also encode namespaces and id ? *)
          children_to_field [] ]
    | Trm_record l ->
        [ kind_to_field  "struct";
          (* LATER: Temporary hack for labelled struct inits. *)
          children_to_field (List.mapi ichild_to_json (List.map aux(List.split_pairs_snd (Mlist.to_list l)))) ]
    | Trm_array l ->
        [ kind_to_field "array";
          children_to_field (List.mapi ichild_to_json (List.map aux (Mlist.to_list l))) ]
    | Trm_let ((x,typ),init) ->
        [ kind_to_field "var-def";
          (strquote "name", strquote x.name); (* TODO: #var-id , also encode namespaces and id ? *)
          (strquote "def-type", typ_to_json typ);
          children_to_field ([(child_to_json "init" (aux init))])]
    | Trm_let_mult _ -> [] (* TODO: *)
    | Trm_let_fun (f, typ, xts, tbody, _) ->
      [ kind_to_field "fun-def";
            (strquote "name", strquote f.name); (* TODO: #var-id , also encode namespaces and id ? *)
            (strquote "args", typed_var_list_to_json xts);
            (strquote "return_type", typ_to_json typ);
            children_to_field ([(child_to_json "body" (aux tbody))]) ]
    | Trm_typedef td ->
      [ kind_to_field "typdef";
        (strquote "name", strquote td.typedef_name.name); (* TODO: #var-id , also encode namespaces and id ? *)
        (strquote "contents", typdef_to_json td);
        children_to_field [] ]
    | Trm_if (cond, then_, else_) ->
        [ kind_to_field "if";
          children_to_field [
            child_to_json "cond" (aux cond);
            child_to_json "then" (aux then_);
            child_to_json "else" (aux else_) ] ]
    | Trm_seq l ->
        let l = Mlist.to_list l in
        [ kind_to_field "seq";
          children_to_field (List.mapi ichild_to_json (List.map aux l))]
    | Trm_apps (f,args,_) ->
        let args_children = List.mapi (ichild_to_json ~prefix:"_arg" ) (List.map aux args) in
        let children = (child_to_json "fun" (aux f)) :: args_children in
        [ kind_to_field "app";
          children_to_field children]
    | Trm_for (range, body, _) ->
      [ kind_to_field "simple_for";
          (strquote "index", strquote range.index.name); (* TODO: #var-id , also encode namespaces and id ? *)
          children_to_field [
            child_to_json "start" (aux range.start);
            child_to_json "stop" (aux range.stop);
            child_to_json "step" (aux range.step);
            child_to_json "body" (aux body) ] ]
    | Trm_for_c (init, cond, step, body, _) ->
        [ kind_to_field "for";
          children_to_field [
            child_to_json "init" (aux init);
            child_to_json "cond" (aux cond);
            child_to_json "step" (aux step);
            child_to_json "body" (aux body) ] ]
    | Trm_while (cond, body) ->
        [ kind_to_field "while";
          children_to_field [
            child_to_json "cond" (aux cond);
            child_to_json "then" (aux body)] ]
    | Trm_do_while (body, cond) ->
        [ kind_to_field "do_while";
          children_to_field [
            child_to_json "then" (aux body);
            child_to_json "cond" (aux cond)] ]
    | Trm_switch (cond,_cases) ->
        [ kind_to_field "switch";
          (* I will cover cases later on *)
          children_to_field [child_to_json (quote "cond") (aux cond)] ]
          (* LATER: support only for now the form:  (not supporting ([p00;p01],t1))
                Trm_switch (cond, [([p0], t0); ([p1], t1); ([], t2)]) =
             "pat_0", aux p0
             "body_0", aux t0
             "pat_1", aux p1
             "body_1", aux t1
             "body_2", aux t2
          *)
          (* children_to_field (List.flatten (List.mapi children_of_icase cases)) *)
          (* let children_of_icase (i:int) (case:(trms*trm)) : (string,nodeid) =
                let pat_label = "pat_" ^ string_of_int i in
                let body_label = "body_" ^ string_of_int i in
                match case with
                | ([],body) -> [(body_label, aux body)]
                | ([tpat],body) -> [(pat_label, aux tpat); (body_label, aux body)]
                | (_,body) -> trm_fail t "Ast_to_js.multipattern switch not yet supported in json output"
           *)
    | Trm_abort res ->
        begin match res with
        | Ret res->
           let children = match res with
             | None -> []
             | Some ret -> [ child_to_json "value" (aux ret) ]
             in
           [ kind_to_field "return";
            children_to_field children ]
        | Break _ ->
            [ kind_to_field "break";
              children_to_field [] ]
        | Continue _ ->
            [ kind_to_field "continue";
              children_to_field [] ]
        end
    | Trm_goto label ->
        [ kind_to_field "goto";
          (strquote "target", strquote label);
          children_to_field []]
    | Trm_arbitrary a_kind ->
       let code_str = code_to_str a_kind in
      [kind_to_field "arbitrary code";
      value_to_field code_str]
    | Trm_omp_routine r -> [routine_to_json r]
    | Trm_extern (_, l) ->
      [ kind_to_field "extern";
        children_to_field (List.mapi ichild_to_json (List.map aux l))]
    | Trm_namespace (name, t1, _) ->
      [ kind_to_field "namespace";
          value_to_field name;
          children_to_field [child_to_json "namespace" (aux t1)]]
    | Trm_template (_, t) ->
      [ kind_to_field "template";
          children_to_field [child_to_json "template" (aux t)]]
    | Trm_using_directive nmspc ->
      [ kind_to_field "using namespace";
          value_to_field nmspc]
    | Trm_fun (xfs, ty_opt, tbody, _) ->
      let ret_ty_js = begin match ty_opt with | Some ty -> typ_to_json ty | None -> Json.str "" end in
      [ kind_to_field "lambda";
            (strquote "args", typed_var_list_to_json xfs);
            (strquote "return_type", ret_ty_js);
            children_to_field ([(child_to_json "body" (aux tbody))]) ]

(** [ast_to_json trm_root]: converts a full ast to a Json object *)
let ast_to_json (trm_root : trm) : json =
  (* node id generator *)
  let nextid = ref (-1) in
  let get_nextid () : nodeid =
    incr nextid;
    !nextid in

  (* output of the fuction *)
  let result : ((json * json) list) ref = ref [] in

  (* recursive construction *)
  let rec aux id_parent (t : trm) : nodeid =
    let id = get_nextid() in
    let specific_fields = node_to_js (aux id) t in
    let json = Json.Obj (specific_fields @ [
      (strquote "parent", Json.Int id_parent);
      (strquote "typ",  (( match t.typ with
                          | None -> strquote "<no type information>"
                          | Some typ -> typ_to_json typ )));
      (strquote "errors", Json.List (List.map Json.str t.errors));
      (* (strquote "annot", strquote (annot_list_to_string t) ); *) (* Fix me! *)
      (strquote "loc", loc_to_json t);
      ]) in
    result := (Json.Int id, json) :: !result;
    id in
  let id_of_root = aux nodeid_invalid trm_root in
  assert (id_of_root = 0);
  Json.Obj (!result)

(** [ast_json_to_doc out t]: prints OptiTrust ast to a json using the channel [out] *)
let ast_json_to_doc (out : out_channel) (t : trm) : unit =
  ToChannel.pretty 0.9 80 out (Json.to_doc (ast_to_json t))


(** [ast_to_js out index t]: prints OptiTrust ast to an out_channel(usually a .js file) as an array called [contents], see function json_to_js
    [index] represents the state of the ast after applying i-th transformation *)
let ast_to_js (out : out_channel) (index : int) (t : trm) : unit =
  ToChannel.pretty 0.9 80 out (json_to_js index (ast_to_json t)  )
