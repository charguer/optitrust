open Ast
open Ast_to_text


module Json = struct
  open PPrint

(* TODO: delete? *)
let braces (d : document) : document =
  soft_surround 2 1 lbrace d rbrace

(* TODO: move to tools.ml *)
let document_to_string (d : document) : string =
  let b = Buffer.create 80 in
  PPrintEngine.ToBuffer.pretty 0.9 80 b d;
  Buffer.contents b

  (** Representation of a json object *)

  type t =
    | Str of string
    | Int of int
    | Boolean of bool
    | List of t list
    | Object of (string * t) list

  (** Smart constructors *)

  let str x = Str x

  (** Printing functions *)

  let print_list (dl : document list) : document =
    surround 2 1 lbracket (separate (comma ^^ break 1) dl) rbracket

  let print_object (dl : document list) : document =
    surround 2 1 lbrace (separate (comma ^^ break 1) dl) rbrace

  let rec json_to_doc (j : t) : document =
    match j with
    | Str s -> string s
    | Int i -> string (string_of_int i)
    | Boolean b-> string (string_of_bool b)
    | List l -> print_list (List.map json_to_doc l)
    | Object o -> print_object (List.map (fun (s,j) -> string s ^^ string ": " ^^ json_to_doc j) o)

 (* DEPRECATED remove when json_to_doc works *)
  let rec json_to_string (j : t) : string =
    match j with
    | Str s ->  s
    | Int i -> string_of_int i
    | Boolean b -> string_of_bool b
    | List  l -> Path.string_of_list ~sep:"," (List.map json_to_string l)
    | Object o -> Path.string_of_list ~sep:","  ~bounds:["{";"}"] (List.map (fun (s, j) -> s ^ ":" ^ (json_to_string j)) o)

end

type json = Json.t

type nodeid = string

(* The void object is [{}]. *)
let void =
  Json.Object []

let loc_to_json (t : trm) : json =
  begin match t.loc with
  | None -> void
  | Some (_, start_row, end_row, start_column, end_column) ->
      Json.Object [
        ("start", Json.Object
          [ ("line", Json.Int start_row);
            ("col", Json.Int start_column)] );
        ("end", Json.Object[
           ("line", Json.Int end_row);
           ("col", Json.Int end_column)] )]
  end

let typ_to_string (typ : typ) : string =
   document_to_string (Ast_to_text.print_typ typ)

let typ_to_json (typ : typ) : json =
  Json.str (typ_to_string typ)

let typed_var_list_to_json (tv : typed_var list) : json =
  Json.Object ( List.map (fun (v,typ) -> (v, typ_to_json typ)) tv)


(* TODO: renamings
  - ast_of_clang -> clang_to_ast
  - file_of_node
  - string_of_path -> path_to_string
  - string_of_dir
  - doc_of_add
*)

let child_to_json (label : string) (child_id : nodeid) : json =
  Json.Object [ ("label", Json.Str label); ("id", Json.Str child_id) ]

let ichild_to_json ?(prefix:string="") (i : int) (child_id : nodeid) : json =
  child_to_json (prefix ^ string_of_int i) (child_id)

let children_to_field (children: json list) : (string * json) =
  ("children", Json.List children)

let kind_to_field (kind : string) : string * json =
  ("kind", Json.Str kind)

let value_to_field (value : string) : string *json =
  ("value", Json.Str value)

(* Here, [aux] is to be applied for processing children *)

let node_to_js (aux : trm -> nodeid) (t : trm) : (string * json) list =
    match t.desc with
    | Trm_val v ->
        [ kind_to_field "val";
          value_to_field (document_to_string (print_val v));
          children_to_field [] ]
    | Trm_var x ->
        [ kind_to_field "var";
          value_to_field x;
          children_to_field [] ]
    | Trm_struct l ->
        [ kind_to_field "struct";
          children_to_field (List.mapi ichild_to_json (List.map aux l)) ]
    | Trm_array l ->
        [ kind_to_field "array";
          children_to_field (List.mapi ichild_to_json (List.map aux l)) ]
    | Trm_decl d ->
        begin match d with
        | Def_var ((x,typ),body) ->
          [ kind_to_field "var-def";
            ("name", Json.Str x);
            ("def-type", typ_to_json typ);
            children_to_field ([(child_to_json "body" (aux body))])]
        | Def_fun (f,typ,xts,tbody) ->
          [ kind_to_field "fun-def";
            ("name", Json.Str f);
            ("args", typed_var_list_to_json xts);
            ("return_type", typ_to_json typ);
            children_to_field ([(child_to_json "body" (aux tbody))]) ]
        | Def_typ (tv,typ) ->
          [ kind_to_field "typ-def";
            ("name", Json.Str tv);
            ("contents", typ_to_json typ);
            children_to_field [] ]
        | Def_enum (tv,_) -> (*TODO: support enum better--figure out what are the trmoptions * *)
          [ kind_to_field "enum-def";
            value_to_field tv;
            children_to_field [] ]
        end
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
    | Trm_for (init, cond, step, body) ->
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
          children_to_field [child_to_json "cond" (aux cond)] ]
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
             | Some ret -> [Json.Object [("value",Str (aux ret))]]
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
          ("target", Json.Str label);
          children_to_field []]
    | Trm_decoration (_,t,_) ->
        [ kind_to_field "decoration";
          ("kind", Json.Str "decoration");
          children_to_field [child_to_json "decoration" (aux t)]]
    | Trm_any t ->
        [ kind_to_field "any";
          children_to_field [child_to_json "any" (aux t)]]


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

let ast_to_json (trm_root : trm) : json =
  (* node id generator *)
  let nextid = ref (-1) in
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
                          | None -> "<no type information>"
                          | Some typ -> typ_to_string typ)));
      ("add", Json.List (List.map Json.str (List.map add_to_string t.add)));
      ("is_instr", Json.Boolean t.is_instr);
      ("annot", Json.Str (annot_to_string t));
      ("loc", loc_to_json t);
      ("attributes", Json.List (List.map Json.str (List.map document_to_string
                                 (List.map print_attribute t.attributes))))
      ]) in
    result := (id, json) :: !result;
    id in
  let parent_of_root = "no_parent" in
  let id_of_root = aux parent_of_root trm_root in
  assert (id_of_root = "node_0");
  Json.Object (!result)

let ast_json_to_doc (out : out_channel) (t : trm) : unit =
  PPrintEngine.ToChannel.pretty 0.9 80 out (Json.json_to_doc (ast_to_json t))



