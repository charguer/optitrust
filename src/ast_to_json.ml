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
      | List of t list 
      | Object of (string * t) list 
    
    (* TODO: let json_of_list l = *)

    let rec json_to_string (j : t) : string = 
      match j with 
      | Str s -> s 
      | Int i -> string_of_int i
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
let (--) i j = 
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
        ("children", List [])
        ]
    | Trm_var x -> 
      [
        ("kind", Json.Str "var");
        ("value", Json.Str x);
        ("children", List [])
      ]
    | Trm_struct l -> 
      let indices = 1--(List.length) l in 
      let lid = List.map aux l in 
      let lid = List.map(fun x -> ("id", Json.Str x)) lid in 
      let labels = List.map(fun x -> ("label", Json.Int x)) indices in 
      let children = List.map2 (fun x1 x2 -> Json.Object [x1;x2]) labels lid in 
      [
        ("kind", Json.Str "struct");
        ("children", Json.List children)
      ]
    
    | Trm_array l ->
      let indices = 1--(List.length) l in 
      let lid = List.map aux l in 
      let lid = List.map(fun x -> ("id",x)) lid in 
      let labels = List.map(fun x -> ("label",Json.Int x)) indices in 
      let children = List.map2 (fun x1 x2 -> Json.Object [x1;x2]) labels lid in 
      [
        ("kind", Json.Str "array"),
        ("children", children)
      ] 
    | Trm_decl d ->
        match d with 
        | Def_var ((x,t),_) -> 
          [
            ("kind", Json.Str "var-def");
            ("name", Json.Str x);
            ("children", Json.Object [("label","body"),("id",aux t)])
          ]
        | Def_fun (f,typ,xts,tbody) ->
          [
            ("kind", Json.Str "fun-def");
            ("name", Json.Str f);
            ("children", Json.Object [("label","body");("id",aux tbody)]);
            ("args", []);
            ("return_type", document_to_string (Ast_to_text.print_typ t))
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
    | Trm_if (cond, then_, else_) ->
      [
        ("kind", Json.Str "if");
        ("children", List 
        [ Json.Object[("label","cond");("id",aux cond)];
          Json.Object[("label","then");("id",aux then_)];
          Json.Object[("label","cond");("id",aux else_)]
        ])
      ]
    | Trm_seq l -> 
      let indices = 1--(List.length) l in 
      let lid = List.map aux l in 
      let lid = List.map(fun x -> ("id",x)) lid in 
      let labels = List.map(fun x -> ("label",Json.Int x)) indices in 
      let children = List.map2 (fun x1 x2 -> Json.Object [x1;x2]) labels lid in       
      [
        ("kind", Json.Str "seq");
        ("children", children)
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
      [
        ("kind", Json.Str "abort");
        ("children", List [Json.Object[("label","abort"),("id",aux res)]])
        
      ]
    | Trm_labelled (label,t) -> 
      [
        ("kind", Json.Str "labelled");
        ("children", List [Json.Object[("label","labelled");("id",aux t)]])
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
        ("children", List [Json.Object[("label","decoration"),("id",aux t)]])
      ]
    | Trm_any t -> 
      [
        ("kind", Json.Str "any");
        ("children", List [Json.Object[("label","any"),("id",aux t)]])
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


let ast_to_js (root : trm) : nodeid = 
  (* node id generator *)
  let nextid = ref 0 in 
  
  let get_nextid () = 
    incr nextid;
    "node_" ^ (string_of_int !nextid) in 
  (* output of the fuction *)
  let result : ((nodeid * json) list) ref = ref [] in 
  (* recursive construction *)
  
  let rec aux parentid t = 
    let id = get_nextid() in 
    let specific_fields = node_to_js (fun ti -> aux id ti) t in 
    let json = Json.Object (specific_fields @ [
      ("parent", Json.Int parentid);
      ("typ", Json.Str ((document_to_string (print_typ t.typ))));
      ("add", List (List.map (document_to_string addition_to_string) t.add));
      ("is_instr",(Bool t.is_instr));s
      ("annot", Json.Str (annot_to_string t));
      ("loc", (loc_to_json t));
      ("attributes", List (t.attributes))
    ]) in 
    result := (nodeid,json) :: !result;
    id in 
  aux (-1) root
