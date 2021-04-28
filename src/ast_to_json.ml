open Ast
open PPrint
open Ast_to_text

let document_to_string (d : document) : string =
  let b = Buffer.create 80 in
  PPrintEngine.ToBuffer.pretty 0.9 80 b d;
  Buffer.contents b


module Json = 
  struct 
    type t =
      | Str of string
      | Int of int 
      | Boolean of bool
      | List of t list 
      | Object of (string * t) list 
    
    (* TODO: let json_of_list l = *)

    let rec json_to_string (j : t) : string = 
      match j with 
      | Str s -> s 
      | Int i -> string_of_int i
      | Boolean b -> string_of_bool b
      | List  l -> Path.string_of_list ~sep:"," (List.map json_to_string l)
      | Object o ->  Path.string_of_list ~sep:","  ~bounds:["{";"}"] (List.map (fun (s, j) -> s ^ ":" ^ (json_to_string j)) o)
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
           ("end", Json.Object[("line",Json.Int end_row);("col",Json.Int end_column)])]
         
  end

(* Get the list of integers with left bound i and right bound j *)
let range i j = 
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc)
    in aux j []



(* Here, [aux] is to be applied for processing children *)
let node_to_js (aux : trm -> nodeid) (t : trm) : (string * json) list= 
    match t.desc with
    | Trm_val v -> 
        [
        ("kind", Json.Str "val");
        ("value", Json.Str (document_to_string (print_val  v)));
        ("children", Json.List [])
        ]
    | Trm_var x -> 
      [
        ("kind", Json.Str "var");
        ("value", Json.Str x);
        ("children", Json.List [])
      ]
    | Trm_struct l -> 
      let lid = List.map aux l in 
      let children = List.mapi (fun i x -> Json.Object 
        [("label", Json.Int i);("id", Json.Str x)]
      ) lid in 

      [
        ("kind", Json.Str "struct");
        ("children", Json.List children)
      ]
    
    | Trm_array l ->
      let lid = List.map aux l in 
      let children = List.mapi (fun i x -> Json.Object 
        [("label", Json.Int i);("id", Json.Str x)]
      ) lid in 

      [
        ("kind", Json.Str "array");
        ("children", Json.List children)
      ] 
    | Trm_decl d ->
        begin match d with 
        | Def_var ((x,t),_) -> 
          [
            ("kind", Json.Str "var-def");
            ("name", Json.Str x);
            ("children", Json.Object [("label", Json.Str "body");("id", Str (document_to_string (Ast_to_text.print_typ t)))])
          ]
        | Def_fun (f,typ,xts,tbody) ->
          [
            ("kind", Json.Str "fun-def");
            ("name", Json.Str f);
            ("children", Json.Object [("label", Json.Str "body");("id", Json.Str (aux tbody))]);
            ("args", List []);
            ("return_type", Json.Str (document_to_string (Ast_to_text.print_typ typ)))
          ]
        | Def_typ (tv,typ) -> 
          [
            ("kind", Json.Str "type-def");
            ("value", Json.Str tv);
            ("children", List [])
          ]
        | Def_enum (tv,_) ->
          [("kind", Json.Str "enum-def");
            ("value", Json.Str tv);
            ("children", List [])
          ]
        end 
    | Trm_if (cond, then_, else_) ->
      [
        ("kind", Json.Str "if");
        ("children", List 
        [ Json.Object[("label", Json.Str "cond");("id", Json.Str (aux cond))];
          Json.Object[("label", Json.Str "then");("id", Json.Str (aux then_))];
          Json.Object[("label", Json.Str "cond");("id", Json.Str (aux else_))]
        ])
      ]
    | Trm_seq l -> 
      let lid = List.map aux l in 
      let children = List.mapi (fun i x -> Json.Object 
        [("label", Json.Int i);("id", Json.Str x)]
      ) lid in 

      [
        ("kind", Json.Str "seq");
        ("children", Json.List children)
      ]
    | Trm_apps (f,args) ->
      [
        ("kind", Json.Str "apps");
        ("children", List []);
      ]
    | Trm_switch (cond,cases) ->
      [
        ("kind", Json.Str "switch");
        ("children", List [])
      ]
    | Trm_abort res ->
      begin match res with 
      | Ret (Some ret)->    [
            ("kind", Json.Str "abort");
            ("children",  List [Json.Object[("label", Json.Str "abort");("id", Json.Str (aux ret))]]) 
          ]
      | Ret None ->
         [
            ("kind", Json.Str "abort");
            ("val", Json.Str "None");
            ("children", List [])
          ]
      
      | Break ->
         [
            ("kind", Json.Str "abort");
            ("val", Json.Str "Break");
            ("children", List [])
          ]
      | Continue ->
         [
            ("kind", Json.Str "abort");
            ("val", Json.Str "Continue");
            ("children", List [])
          ]
      end 
    | Trm_labelled (label,t) -> 
      [
        ("kind", Json.Str "labelled");
        ("children", List [Json.Object[("label", Json.Str "labelled");("id",Json.Str(aux t))]])
      ]
    | Trm_goto label -> 
      [
        ("kind", Json.Str "goto");
        ("value", Json.Str label);
        ("children", List [])
      ]
    | Trm_decoration (_,t,_) ->
      [
        ("kind", Json.Str "decoration");
        ("children", List [Json.Object[("label", Json.Str "decoration");("id", Json.Str (aux t))]])
      ]
    | Trm_any t -> 
      [
        ("kind", Json.Str "any");
        ("children", List [Json.Object[("label", Json.Str "any");("id",Json.Str (aux t))]])
      ]
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
    

let ast_to_js (root : trm) : nodeid = 
  (* node id generator *)

  let nextid = ref 0 in 
  let get_nextid () = 
    incr nextid;
    "node_" ^ (string_of_int !nextid) in 
  (* output of the fuction *)
  let result : ((nodeid * json) list) ref = ref [] in 
  (* recursive construction *)
  
  let rec aux t = 
    let id = get_nextid() in 
    let specific_fields = node_to_js aux  t in 
    let json = Json.Object (specific_fields @ [
      (* ("parent", Json.Int parentid); *)
      ("typ", Json.Str (( match t.typ with 
                          | None -> ""
                          | Some typ -> document_to_string (print_typ typ))));
      ("add", Json.List (List.map(fun x -> Json.Str x)(List.map add_to_string t.add)));
      ("is_instr",(Json.Boolean t.is_instr));
      ("annot", Json.Str (annot_to_string t));
      ("loc", (loc_to_json t));
      ("attributes",Json.List (List.map (fun x -> Json.Str x) (List.map document_to_string (List.map print_attribute t.attributes))))
    ]) in 
    result := (id,json) :: !result;
    id in 
  aux root
