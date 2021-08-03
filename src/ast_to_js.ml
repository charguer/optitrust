open Ast
open Ast_to_text
module Json = struct
  open PPrint
  (* Representation of a json object *)
  type t =
    (* TODO: perhaps we should quote all strings by default.. *)
    | Str of string
    | Int of int
    | Boolean of bool
    | List of t list
    | Object of (t * t) list

  (* Smart constructors *)

  let quote x = "\"" ^ x ^ "\""
  let str x = Str x
  let strquote x = Str (quote x)


  (* Printing functions *)
  let typ_to_json(typ : typ) : t =
    Str (Tools.document_to_string (bquotes (Ast_to_c.typ_to_doc typ)) )

  let typdef_to_json(td : typedef) : t =
    Str (Tools.document_to_string (bquotes (Ast_to_c.typedef_to_doc td)))

  let print_object (dl : document list) : document =
    surround 2 1 lbrace (separate (comma ^^ break 1) dl) rbrace

  let rec json_to_doc (j : t) : document =
    match j with
    | Str s -> string s
    | Int i -> string (string_of_int i)
    | Boolean b-> string (string_of_bool b)
    | List l -> Tools.print_list ~sep:"," (List.map json_to_doc l)
    | Object o -> Tools.print_object (List.map (fun (k,j) -> json_to_doc k ^^ string ": " ^^ json_to_doc j) o)

  let json_to_js ?(index : int = (-1)) (j : t) : document =
   let json_ast = json_to_doc j in
   match index with
   | -1 ->  string "contents" ^^ equals ^^ json_ast ^^ semi ^^ hardline
   | _ ->   string "contents" ^^ brackets (string(string_of_int index)) ^^ equals ^^ json_ast ^^ semi ^^ hardline

  let code_to_js (out : out_channel) (index : int) (src : string) : unit =
    let doc = match index with (* TODO: factorize code better *)
      | -1 -> string "source"  ^^ equals ^^ bquotes (string src) ^^ hardline
      | _ -> string "source" ^^ brackets (string (string_of_int index)) ^^ equals ^^ bquotes (string src) ^^ hardline
      in
    PPrintEngine.ToChannel.pretty 0.9 80 out doc

end

type json = Json.t
let quote = Json.quote
let strquote = Json.strquote

type nodeid = int
let nodeid_invalid = (-1)

(* The void object is [{}]. *)
let void =
  Json.Object []

let loc_to_json (t : trm) : json =
  begin match t.loc with
  | None -> Json.Str (quote "")
  | Some {loc_file = _; loc_start = {pos_line = start_row; pos_col = start_column}; loc_end = {pos_line = end_row; pos_col = end_column}} ->
      Json.Object [
        (strquote "start", Json.Object
          [ (strquote "line" , Json.Int start_row);
            (strquote "col", Json.Int start_column)] );
        (strquote "end", Json.Object[
           (strquote "line", Json.Int end_row);
           (strquote "col", Json.Int end_column)] )]
  end

let typed_var_list_to_json (tv : typed_var list) : json =
  Json.Object (List.map (fun (v,typ) -> (strquote v, Json.typ_to_json typ)) tv)

let child_to_json (label : string) (child_id : nodeid) : json =
  Json.Object [ ((strquote "label"), Json.Str (quote label));
                ((strquote "id", Json.Int child_id)) ]

let ichild_to_json ?(prefix:string="") (i : int) (child_id : nodeid) : json =
  child_to_json (prefix ^ string_of_int i) (child_id)

let children_to_field (children: json list) : (json * json) =
  (strquote "children", Json.List children)

let kind_to_field (kind : string) : json * json =
  (strquote "kind", Json.Str (quote kind))

let value_to_field (value : string) : json * json =
  (strquote "value", Json.Str (quote value))

let directive_to_json (directive : directive) : json * json = 
  let directive_js =
  match directive with
  | Atomic _-> Json.Str  "Atomic"
  | Atomic_capture -> Json.Str  "Atomic_capture"
  | Barrier -> Json.Str  "Barrier"
  | Cancel _ -> Json.Str  "Cancel"
  | Cancellation_point _-> Json.Str  "Cancellation_point"
  | Critical -> Json.Str  "Critical"
  | Declare_simd _ -> Json.Str  "Declare_simd"
  | Declare_reduction _ -> Json.Str  "Declare_reduction"
  | Declare_target -> Json.Str  "Declare_target"
  | Distribute _ -> Json.Str  "Distribute"
  | Distribute_parallel_for _ -> Json.Str  "Distribute_parallel_for"
  | Distribute_parallel_for_simd _ -> Json.Str  "Distribute_parallel_for_simd"
  | Distribute_simd -> Json.Str  "Distribute_simd"
  | End_declare_target -> Json.Str  "End_declare_target"
  | Flush _ -> Json.Str  "Flush"
  | For _ -> Json.Str  "For"
  | For_simd _ -> Json.Str  "For_simd"
  | Master -> Json.Str  "Master"
  | Ordered -> Json.Str  "Ordered"
  | Parallel _ -> Json.Str  "Parallel"
  | Parallel_for -> Json.Str  "Parallel_for"
  | Parallel_for_simd _ -> Json.Str  "Parallel_for_simd"
  | Parallel_sections _ -> Json.Str  "Parallel_sections"
  | Section -> Json.Str  "Section"
  | Sections _ -> Json.Str  "Sections"
  | Simd _ -> Json.Str  "Simd"
  | Single _ -> Json.Str  "Single"
  | Target _ -> Json.Str  "Target"
  | Target_data _ -> Json.Str  "Target_data"
  | Target_enter_data _ -> Json.Str  "Target_enter_data"
  | Target_exit_data _ -> Json.Str  "Target_exit_data"
  | Target_teams _ -> Json.Str  "Target_teams"
  | Target_teams_distribute _ -> Json.Str  "Target_teams_distribute"
  | Target_teams_distribute_parallel_for _ -> Json.Str  "Target_teams_distribute_parallel_for"
  | Target_teams_distribute_parallel_for_simd _ -> Json.Str  "Target_teams_distribute_parallel_for_simd"
  | Target_teams_distribute_simd _ -> Json.Str  "Target_teams_distribute_simd"
  | Target_update _ -> Json.Str  "Target_update"
  | Task _ -> Json.Str  "Task"
  | Taskgroup -> Json.Str  "Taskgroup"
  | Taskloop _ -> Json.Str  "Taskloop"
  | Taskloop_simd _ -> Json.Str  "Taskloop_simd"
  | Taskwait -> Json.Str  "Taskwait"
  | Taskyield -> Json.Str  "Taskyield"
  | Teams _ -> Json.Str  "Teams"
  | Teams_distribute _ -> Json.Str  "Teams_distribute"
  | Teams_distribute_end _ -> Json.Str  "Teams_distribute_end"
  | Teams_distribute_parallel_for _ -> Json.Str  "Teams_distribute_parallel_for"
  | Teams_distribute_parallel_for_simd _ -> Json.Str  "Teams_distribute_parallel_for_simd"
  | Threadprivate _ -> Json.Str  "Threadprivate"
  in (strquote "directive", directive_js)

let routine_to_json (routine : omp_routine) : json * json = 
  let routine_js = 
  match routine with 
  | Set_num_threads _ -> Json.Str  "Set_num_threads"
  | Get_num_threads -> Json.Str  "Get_num_threads"
  | Get_max_threads -> Json.Str  "Get_max_threads"
  | Get_thread_num -> Json.Str  "Get_thread_num"
  | Get_num_procs -> Json.Str  "Get_num_procs"
  | In_parallel -> Json.Str  "In_parallel"
  | Set_dynamic _ -> Json.Str  "Set_dynamic"
  | Get_dynamic -> Json.Str  "Get_dynamic"
  | Get_cancellation -> Json.Str  "Get_cancellation"
  | Set_nested _ -> Json.Str  "Set_nested"
  | Get_nested -> Json.Str  "Get_nested"
  | Set_schedule _ -> Json.Str  "Set_schedule"
  | Get_schedule _ -> Json.Str  "Get_schedule"
  | Get_thread_limit -> Json.Str  "Get_thread_limit"
  | Set_max_active_levels _ -> Json.Str  "Set_max_active_levels"
  | Get_max_active_levels -> Json.Str  "Get_max_active_levels"
  | Get_level -> Json.Str  "Get_level"
  | Get_ancestor_thread_num -> Json.Str  "Get_ancestor_thread_num"
  | Get_team_size _ -> Json.Str  "Get_team_size"
  | Get_active_level -> Json.Str  "Get_active_level"
  | In_final -> Json.Str  "In_final"
  | Get_proc_bind -> Json.Str  "Get_proc_bind"
  | Set_default_device _ -> Json.Str  "Set_default_device"
  | Get_default_device -> Json.Str  "Get_default_device"
  | Get_num_devices -> Json.Str  "Get_num_devices"
  | Get_num_teams -> Json.Str  "Get_num_teams"
  | Get_team_num -> Json.Str  "Get_team_num"
  | Is_initial_device -> Json.Str  "Is_initial_device"
  | Init_lock _ -> Json.Str  "Init_lock"
  | Init_nest_lock _ -> Json.Str  "Init_nest_lock"
  | Destroy_lock _ -> Json.Str  "Destroy_lock"
  | Destroy_nest_lock _ -> Json.Str  "Destroy_nest_lock"
  | Set_lock _ -> Json.Str  "Set_lock"
  | Set_nest_lock _ -> Json.Str  "Set_nest_lock"
  | Unset_lock _ -> Json.Str  "Unset_lock"
  | Unset_nest_lock _ -> Json.Str  "Unset_nest_lock"
  | Test_lock _ -> Json.Str  "Test_lock"
  | Test_nest_lock _ -> Json.Str  "Test_nest_lock"
  | Get_wtime -> Json.Str  "Get_wtime"
  | Get_wtick -> Json.Str  "Get_wtick"
  in  (strquote "routine", routine_js)

(* Here, [aux] is to be applied for processing children *)
let node_to_js (aux : trm -> nodeid) (t : trm) : (json * json) list =
    match t.desc with
    | Trm_val v ->
        [ kind_to_field "val";
          (strquote "value", Json.Str (Tools.document_to_string (PPrint.bquotes(Ast_to_c.val_to_doc v))));
          children_to_field [] ]
    | Trm_var x ->
        [ kind_to_field "var";
          value_to_field x;
          children_to_field [] ]
    | Trm_struct l ->
        [ kind_to_field  "struct";
          children_to_field (List.mapi ichild_to_json (List.map aux l)) ]
    | Trm_array l ->
        [ kind_to_field "array";
          children_to_field (List.mapi ichild_to_json (List.map aux l)) ]
    | Trm_let (_,(x,typ),init) ->
        [ kind_to_field "var-def";
          (strquote "name", Json.Str (quote x));
          (strquote "def-type", Json.typ_to_json typ);
          children_to_field ([(child_to_json "init" (aux init))])]
    | Trm_let_fun (f, typ, xts, tbody) ->
      [ kind_to_field "fun-def";
            (strquote "name", Json.Str (quote f));
            (strquote "args", typed_var_list_to_json xts);
            (strquote "return_type", Json.typ_to_json typ);
            children_to_field ([(child_to_json "body" (aux tbody))]) ]
    | Trm_typedef td ->
      [ kind_to_field "typdef";
        (strquote "name", Json.Str (quote td.typdef_tconstr));
        (strquote "contents", Json.typdef_to_json td);
        children_to_field [] ]
    | Trm_if (cond, then_, else_) ->
        [ kind_to_field "if";
          children_to_field [
            child_to_json "cond" (aux cond);
            child_to_json "then" (aux then_);
            child_to_json "else" (aux else_) ] ]
    | Trm_seq l ->
        [ kind_to_field "seq";
          children_to_field (List.mapi ichild_to_json (List.map aux l))]
    | Trm_apps (f,args) ->
        let args_children = List.mapi (ichild_to_json ~prefix:"_arg" ) (List.map aux args) in
        let children = (child_to_json "fun" (aux f)) :: args_children in
        [ kind_to_field "app";
          children_to_field children]
    | Trm_for (index, _, start, stop, step, body) ->
      [ kind_to_field "simple_for";
          (strquote "index", Json.Str (quote index));
          children_to_field [
            child_to_json "start" (aux start);
            child_to_json "stop" (aux stop);
            child_to_json "step" (aux step);
            child_to_json "body" (aux body) ] ]
    | Trm_for_c (init, cond, step, body) ->
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
    | Trm_switch (cond,_cases) ->
        [ kind_to_field "switch";
          (* I will cover cases later on *)
          children_to_field [child_to_json (quote "cond") (aux cond)] ]
          (* TODO: support only for now the form:  (not supporting ([p00;p01],t1))
                Trm_switch (cond, [([p0], t0); ([p1], t1); ([], t2)]) =
             "pat_0", aux p0
             "body_0", aux t0
             "pat_1", aux p1
             "body_1", aux t1
             "body_2", aux t2
          *)
          (* children_to_field (List.flatten (List.mapi children_of_icase cases)) *)
          (* let children_of_icase (i:int) (case:(trm list*trm)) : (string,nodeid) =
                let pat_label = "pat_" ^ string_of_int i in
                let body_label = "body_" ^ string_of_int i in
                match case with
                | ([],body) -> [(body_label, aux body)]
                | ([tpat],body) -> [(pat_label, aux tpat); (body_label, aux body)]
                | (_,body) -> fail t.loc "multipattern switch not yet supported in json output"
           *)
    | Trm_abort res ->
        begin match res with
        | Ret res->
           let children = match res with
             | None -> []
             | Some ret -> (* TODO: boggus? [Json.Object [(quote "value",Str (aux ret))]]*)
                 [ child_to_json "value" (aux ret) ]
             in
           [ kind_to_field "return";
            children_to_field children ]
        | Break ->
            [ kind_to_field "break";
              children_to_field [] ]
        | Continue ->
            [ kind_to_field "continue";
              children_to_field [] ]
        end
    | Trm_labelled (label,t) ->
        [ kind_to_field "labelled";
          value_to_field label;
          children_to_field [child_to_json "labelled" (aux t)]]
    | Trm_goto label ->
        [ kind_to_field "goto";
          (strquote "target", Json.Str label);
          children_to_field []]
    | Trm_any t ->
        [ kind_to_field "any";
          children_to_field [child_to_json "any" (aux t)]]
    | Trm_arbitrary _ -> fail t.loc  "node_to_js: arbitrary code dissappears when C code is parsed"
    | Trm_omp_directive d -> [directive_to_json d]
    | Trm_omp_routine r -> [routine_to_json r] 
    
let annot_to_string (t_ann : trm_annot) : string =
  match t_ann with
     | Grouped_binding -> quote "Grouped_binding"
     | No_braces _ -> quote "No_braces"
     | Access -> quote "Access"
     | Multi_decl -> quote "Multi_decl"
     | Empty_cond -> quote "Empty_cond"
     | App_and_set -> quote "App_and_set"
     | Include h -> quote "Include" ^ " " ^ h
     | Main_file -> quote "Main_file"
     | Mutable_var_get -> quote "Mutable_var_get"
     | As_left_value -> quote "As_left_value"
     | Highlight _ -> quote "Hightlight" 
  

  let annot_list_to_string (t : trm) : string =
    Tools.list_to_string ((List.map annot_to_string) t.annot)


let add_to_string (add : special_operator) =
      match add with
      | Add_address_of_operator -> quote "Add_address_of_operator"
      | Add_star_operator -> quote "Add_star_operator"

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
    (* LATER: may consider to go through Trm_decoration, not consider it as a node *)
    let id = get_nextid() in
    let specific_fields = node_to_js (aux id) t in
    let json = Json.Object (specific_fields @ [
      (strquote "parent", Json.Int id_parent);
      (strquote "typ",  (( match t.typ with
                          | None -> Json.Str (quote "<no type information>")
                          | Some typ -> Json.typ_to_json typ )));
      (strquote "add", Json.List (List.map Json.str (List.map add_to_string t.add)));
      (strquote "is_statement", Json.Boolean t.is_statement);
      (strquote "annot", Json.Str (annot_list_to_string t) );
      (strquote "loc", loc_to_json t);
      (strquote "attributes", Json.List (List.map Json.str (List.map Tools.document_to_string
                                 (List.map print_attribute t.attributes))))
      ]) in
    result := (Json.Int id, json) :: !result;
    id in
  let id_of_root = aux nodeid_invalid trm_root in
  assert (id_of_root = 0);
  Json.Object (!result)


let ast_json_to_doc (out : out_channel) (t : trm) : unit =
  PPrintEngine.ToChannel.pretty 0.9 80 out (Json.json_to_doc (ast_to_json t))

(* Convert ast into a json format then print it as a javascript variable inside a javascript file
  the index represents the state of the ast after applying i-th transformation
*)
let ast_to_js (out : out_channel) (index : int) (t : trm) : unit =
  PPrintEngine.ToChannel.pretty 0.9 80 out (Json.json_to_js (ast_to_json t) ~index )
