open Ast
open PPrint
open Ast_to_text

(* TODO: move to tools.ml *)
let document_to_string (d : document) : string =
  let b = Buffer.create 80 in
  PPrintEngine.ToBuffer.pretty 0.9 80 b d;
  Buffer.contents b


module Json = struct 

  type t =
    | Str of string
    | Int of int 
    | Boolean of bool
    | List of t list 
    | Object of (string * t) list 
  
  let str x = Str x

  (* TODO: let list_to_json l = 
    Path.string_of_list ~sep:"," l *)

 (* TODO: this is very slow, only for debug or test *)
  let rec json_to_string (j : t) : string = 
    match j with 
    | Str s -> s 
    | Int i -> string_of_int i
    | Boolean b -> string_of_bool b
    | List  l -> Path.string_of_list ~sep:"," (List.map json_to_string l)
    | Object o -> Path.string_of_list ~sep:","  ~bounds:["{";"}"] (List.map (fun (s, j) -> s ^ ":" ^ (json_to_string j)) o)
  (* TODO: json_to_doc ..*)

end  

type json = Json.t 

type nodeid = string 

let loc_to_json (t : trm) : json = 
  begin match t.loc with
  | None -> Json.Str ""
  | Some (_, start_row, end_row, start_column, end_column) ->
      Json.Object [
        ("start", Json.Object
          [("line", Json.Int start_row);
          ("col", Json.Int start_column)]);
        ("end", Json.Object[("line",Json.Int end_row);("col",Json.Int end_column)])] (* TODO tabs *)
  end

(* TODO *)

let typ_to_string typ =  (document_to_string (Ast_to_text.print_typ typ)) 




let typed_var_list_to_json (tv : typed_var list) : json =   
  let tv = List.map (fun (v,typ) -> (v, Json.Str (document_to_string (Ast_to_text.print_typ typ)))) tv in 
  Json.Object tv 


(* TODO: renamings
  - ast_of_clang -> clang_to_ast
  - file_of_node
  - string_of_path -> path_to_string
  - string_of_dir
  - doc_of_add
*)

let child_to_json (label:string) (child_id:nodeid) : json = 
  Json.Object [ ("label", Json.Str label); ("id", Json.Str child_id) ]

let ichild_to_json ?(prefix:string="") (i:int) (child_id:nodeid) : json = 
  child_to_json (prefix ^ string_of_int i) (child_id)

(* let children_to_json(childnred : ()) *)
let children_to_json (children: json list) : (string * json)= 
  ("children", Json.List children)
(* let children_to_json (children:(label*nodeid) list) : json =
  ("children", Json.List [ List.map child_to_json children ])
 *)

let kind_to_json (kind:string) : string * json =
  ("kind", Json.Str kind)

let value_to_json (value: string) : string *json = 
  ("value", Json.Str value)

(* Here, [aux] is to be applied for processing children *)

let node_to_js (aux : trm -> nodeid) (t : trm) : (string * json) list = 
    Json.( (* TODO: no need to write Json. all over the place *)
    match t.desc with (* TODO: style *)
    | Trm_val v -> 
        [ kind_to_json "val"; (* TODO: use this in all cases *)
          value_to_json (document_to_string (print_val v));
          children_to_json [] ]
    | Trm_var x -> 
        [ kind_to_json "var";
          value_to_json x;
          children_to_json [] ]
    | Trm_struct l -> 
        let childrenids = List.map aux l in 
        [ kind_to_json "struct";
          children_to_json (List.mapi ichild_to_json childrenids)]
    | Trm_array l -> (* TODO: function to factorize *)
        let lid = List.map aux l in 
        let children = List.mapi (fun i x -> ichild_to_json i x) lid in 
        [kind_to_json "array";
          children_to_json children]
    | Trm_decl d ->
        begin match d with 
        | Def_var ((x,typ),body) -> 
          [
            kind_to_json "var-def";
            ("name", Json.Str x);
            ("def-type", Json.Str (typ_to_string typ));
            children_to_json ([(child_to_json "body" (aux body))])]
        | Def_fun (f,typ,xts,tbody) ->
          [
            kind_to_json "fun-def";
            ("name", Json.Str f);
            ("args", typed_var_list_to_json xts);

            ("return_type", Json.Str (typ_to_string typ));
            children_to_json ([(child_to_json "body" (aux tbody))])

          ]
        | Def_typ (tv,typ) -> 
          [
            kind_to_json "typ-def";
            ("name", Json.Str tv);
            ("contents", Json.Str (typ_to_string typ));
            children_to_json []]
        | Def_enum (tv,_) -> (*TODO: support enum better--figure out what are the trmoptions * *)
          [ kind_to_json "enum-def";
            value_to_json tv;
            children_to_json []]
        end 
    | Trm_if (cond, then_, else_) ->
        [ kind_to_json "if";
          children_to_json [ 
            child_to_json "cond" (aux cond);
            child_to_json "then" (aux then_);
            child_to_json "else" (aux else_) ]
      ]
    | Trm_seq l -> 
        let childrenids = List.map aux l in 
        [ kind_to_json "seq";
          children_to_json (List.mapi ichild_to_json childrenids)]
    | Trm_apps (f,args) ->
        let childrenids = List.map aux args in 
        let children = List.mapi (ichild_to_json ~prefix:"_arg" )childrenids in
        let children = child_to_json "f" (aux f) :: children in 
        [ kind_to_json "app";
          children_to_json children]
    | Trm_for (init, cond, step, body) -> 
        [ kind_to_json "for";
          children_to_json [
            child_to_json "init" (aux init);
            child_to_json "cond" (aux cond);
            child_to_json "step" (aux step);
            child_to_json "body" (aux body)
          ]]
    | Trm_while (cond, body) -> 
        [ kind_to_json "while";
          children_to_json [ 
            child_to_json "cond" (aux cond);
            child_to_json "then" (aux body)]]
    | Trm_switch (cond,_cases) ->
        [ kind_to_json "switch";
          (* I will cover cases later on *)
          children_to_json [child_to_json "cond" (aux cond)]]
          (* TODO: support only for now the form:  (not supporting ([p00;p01],t1))
                Trm_switch (cond, [([p0], t0); ([p1], t1); ([], t2)]) =
             "pat_0", aux p0 
             "body_0", aux t0
             "pat_1", aux p1
             "body_1", aux t1
             "body_2", aux t2 
          *)
          (* children_to_json (List.flatten (List.mapi children_of_icase cases)) *)
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
             | Some ret -> [Object [("value",Str (aux ret))]]
             in
           [
              kind_to_json "return";
              children_to_json children

              (*("children",  List [Json.Object[("label", Json.Str "abort");("id", Json.Str (aux ret))]]) *)
            ]

        | Break ->
          [   kind_to_json "break";
              children_to_json []]
        | Continue ->
          [   kind_to_json "continue";
              children_to_json []]
        end 
    | Trm_labelled (label,t) -> 
        [ kind_to_json "labelled";
          value_to_json label;
          children_to_json [child_to_json "labelled" (aux t)]]
    | Trm_goto label -> 
        [ kind_to_json "goto";
          ("target", Json.Str label);
          children_to_json []]
    | Trm_decoration (_,t,_) ->
        [ kind_to_json "decoration";
          ("kind", Json.Str "decoration");
          children_to_json [child_to_json "decoration" (aux t)]]
    | Trm_any t -> 
        [ kind_to_json "any";
          children_to_json [child_to_json "any" (aux t)]]
  )


let annot_to_string (t : trm) : string = 
  begin match t.annot with
  | None -> "_"
  | Some a ->
     begin match a with
     | Heap_allocated -> "Heap_allocated"
     | Initialisation_instruction -> "Initialisation_instruction"
     | Delete_instructions -> "Delete_instructions"
     | No_braces -> "No_braces"
     | Access -> "Access"
     | Multi_decl -> "Multi_decl"
     | Empty_cond -> "Empty_cond"
     | App_and_set -> "App_and_set"
     | Include h -> "Include" ^ " " ^ h
     | Main_file -> "Main_file"
     end
  end
let add_to_string (add : print_addition) =
      match add with
      | Add_address_of_operator -> "Add_address_of_operator"
      | Add_star_operator -> "Add_star_operator"
    

let ast_to_json (root:trm) : json = 
  (* node id generator *)
  let nextid = ref 0 in 
  let get_nextid () = 
    incr nextid;
    "node_" ^ (string_of_int !nextid) in 
  (* output of the fuction *)
  let result : ((string * json) list) ref = ref [] in 
  (* recursive construction *)
  
  let rec aux id_parent t = 
    (* LATER: may consider to go through Trm_decoration, not consider it as a node *)
    let id = get_nextid() in 
    let specific_fields = node_to_js (aux id) t in 
    let json = Json.Object (specific_fields @ [
      ("parent", Json.Str id_parent);
      ("typ", Json.Str (( match t.typ with 
                          | None -> ""
                          | Some typ -> typ_to_string typ)));
      ("add", Json.List (List.map Json.str (List.map add_to_string t.add)));
      ("is_instr",(Json.Boolean t.is_instr));
      ("annot", Json.Str (annot_to_string t));
      ("loc", (loc_to_json t));
      ("attributes",Json.List (List.map Json.str (List.map document_to_string (List.map print_attribute t.attributes))))
    ]) in 
    result := (id, json) :: !result;
    id in 
  let _id_of_root = aux (get_nextid()) root in
  Json.Object (!result)



(* json_to_document *)

  
  

