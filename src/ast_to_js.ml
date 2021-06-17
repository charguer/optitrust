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
    | Object of (string * t) list

  (* Smart constructors *)

  let str x = Str x


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
    | Object o -> Tools.print_object (List.map (fun (s,j) -> string s ^^ string ": " ^^ json_to_doc j) o)

  let json_to_js ?(index : int = (-1)) (j : t) : document =
   let json_ast = json_to_doc j in
   match index with
   | -1 ->  string "contents" ^^ equals ^^ json_ast ^^ semi
   | _ ->   string "contents" ^^ brackets (string(string_of_int index)) ^^ equals ^^ json_ast ^^ semi

  let code_to_js (out : out_channel) (index : int) (* DEPRECATED (ast : trm)*) (src : string) : unit =
    (* DEPRECATED let src = Ast_to_c.trm_to_doc ast in *)
    let doc = match index with (* TODO: factorize code better *)
      | -1 -> string "source"  ^^ equals ^^ bquotes (string src)
      | _ -> string "source" ^^ brackets (string (string_of_int 0)) ^^ equals ^^ bquotes (string src)
      in
    PPrintEngine.ToChannel.pretty 0.9 80 out doc

end
let quote x = "\"" ^ x ^ "\""

type json = Json.t

type nodeid = string

(* The void object is [{}]. *)
let void =
  Json.Object []

let loc_to_json (t : trm) : json =
  begin match t.loc with
  | None -> Json.Str (quote "")
  | Some (_, start_row, end_row, start_column, end_column) ->
      Json.Object [
        (quote "start", Json.Object
          [ (quote "line" , Json.Int start_row);
            (quote "col", Json.Int start_column)] );
        (quote "end", Json.Object[
           (quote "line", Json.Int end_row);
           (quote "col", Json.Int end_column)] )]
  end

let typed_var_list_to_json (tv : typed_var list) : json =
  Json.Object ( List.map (fun (v,typ) -> (v, Json.typ_to_json typ)) tv)

let child_to_json (label : string) (child_id : nodeid) : json =
  Json.Object [ ((quote "label"), Json.Str (quote label));
                ((quote "id", Json.Str child_id)) ]

let ichild_to_json ?(prefix:string="") (i : int) (child_id : nodeid) : json =
  child_to_json (prefix ^ string_of_int i) (child_id)

let children_to_field (children: json list) : (string * json) =
  (quote "children", Json.List children)

let kind_to_field (kind : string) : string * json =
  (quote "kind", Json.Str (quote kind))

let value_to_field (value : string) : string *json =
  (quote "value", Json.Str (quote value))

(* Here, [aux] is to be applied for processing children *)

let node_to_js (aux : trm -> nodeid) (t : trm) : (string * json) list =
    match t.desc with
    | Trm_val v ->
        [ kind_to_field "val";
          value_to_field (Tools.document_to_string (PPrint.bquotes(Ast_to_c.val_to_doc v)));
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
    (* TODO: Ask Arthur if Var_kind is needed *)
    | Trm_let (_,(x,typ),init) ->
        [ kind_to_field "var-def";
          (quote "name", Json.Str (quote x));
          (quote "def-type", Json.typ_to_json typ);
          children_to_field ([(child_to_json "init" (aux init))])]
    | Trm_let_fun (f, typ, xts, tbody) ->
      [ kind_to_field "fun-def";
            (quote "name", Json.Str (quote f));
            (quote "args", typed_var_list_to_json xts);
            (quote "return_type", Json.typ_to_json typ);
            children_to_field ([(child_to_json "body" (aux tbody))]) ]
    | Trm_typedef td ->
      [ kind_to_field (quote "typdef");
        (quote "name", Json.Str (quote td.typdef_tconstr));
        (quote "contents", Json.typdef_to_json td);
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
          (quote "target", Json.Str label);
          children_to_field []]
    | Trm_decoration (_,t,_) ->
        [ kind_to_field "decoration";
          children_to_field [child_to_json "decoration" (aux t)]]
    | Trm_any t ->
        [ kind_to_field "any";
          children_to_field [child_to_json "any" (aux t)]]


let annot_to_string (t : trm) : string =
  begin match t.annot with
  | None -> quote ""
  | Some a ->
     begin match a with
     (* | Delete_instructions -> "\"Delete_instructions\"" *)
     | Grouped_binding -> quote "Grouped_binding"
     | No_braces -> quote "No_braces"
     | Access -> quote "Access"
     | Multi_decl -> quote "Multi_decl"
     | Empty_cond -> quote "Empty_cond"
     | App_and_set -> quote "App_and_set"
     | Include h -> quote "Include" ^ " " ^ h
     | Main_file -> quote "Main_file"
     | Mutable_var_get -> quote "Mutable_var_get"
     end
  end

let add_to_string (add : print_addition) =
      match add with
      | Add_address_of_operator -> quote "Add_address_of_operator"
      | Add_star_operator -> quote "Add_star_operator"

let ast_to_json (trm_root : trm) : json =
  (* node id generator *)
  let nextid = ref (-1) in
  let get_nextid () =
    incr nextid;
    quote ("node_" ^ (string_of_int !nextid))  in

  (* output of the fuction *)
  let result : ((string * json) list) ref = ref [] in

  (* recursive construction *)
  let rec aux id_parent (t : trm) =
    (* LATER: may consider to go through Trm_decoration, not consider it as a node *)
    let id = get_nextid() in
    let specific_fields = node_to_js (aux id) t in
    let json = Json.Object (specific_fields @ [
      (quote "parent", Json.Str id_parent);
      (quote "typ",  (( match t.typ with
                          | None -> Json.Str (quote "<no type information>")
                          | Some typ -> Json.typ_to_json typ )));
      (quote "add", Json.List (List.map Json.str (List.map add_to_string t.add)));
      (quote "is_statement", Json.Boolean t.is_statement);
      (quote "annot", Json.Str (annot_to_string t) );
      (quote "loc", loc_to_json t);
      (quote "attributes", Json.List (List.map Json.str (List.map Tools.document_to_string
                                 (List.map print_attribute t.attributes))))
      ]) in
    result := (id, json) :: !result;
    id in
  let parent_of_root = quote "no_parent" in
  let id_of_root = aux parent_of_root trm_root in
  assert (id_of_root = quote "node_0");
  Json.Object (!result)


let ast_json_to_doc (out : out_channel) (t : trm) : unit =
  PPrintEngine.ToChannel.pretty 0.9 80 out (Json.json_to_doc (ast_to_json t))

(* Convert ast into a json format then print it as a javascript variable inside a javascript file
  the index represents the state of the ast after applying i-th transformation
*)
let ast_to_js (out : out_channel) (index : int) (t : trm) : unit =
  PPrintEngine.ToChannel.pretty 0.9 80 out (Json.json_to_js (ast_to_json t) ~index )


