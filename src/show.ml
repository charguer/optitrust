open Printf
open Target
open Ast

include Style

(* Usage:
     Show.trm ~msg:"foo:" t
     Show.trm ~msg:"foo:" tg
     Show.trm ~msg:"foo:" (Target.resolve_path p)
     ShowAt.trm ~msg:"foo:" (Target.of_path p)
*)


(*----------------------------------------------------------------------------------*)
(** Printing combinators *)

let prt_channel = ref stdout

(*** [with_captured_show dest f] executes [f], and during this execution all the
    strings issued by show commands are captured and added to the reference [dest]. *)
let with_captured_show ?(activated:bool=true) (dest : string ref) (f : unit -> 'a) : 'a =
  if not activated then f() else begin
    (* TODO: how to create an output channel from a string buffer instead of using a file? *)
    let tmp_file = Filename.temp_file "capture" ".txt" in
    let c = open_out tmp_file in
    let saved = !prt_channel in
    prt_channel := c;
    let finalize () =
      close_out c;
      dest := Xfile.get_contents tmp_file;
      prt_channel := saved
      in
    try f(); finalize()
    with e -> finalize(); raise e

  end


let prt ?(prefix : string = "") ?(suffix : string = "") (msg : string) : unit =
  fprintf !prt_channel "%s%s%s" prefix msg suffix

(* TODO: should the \n be included by default? *)

(** [prt_msg msg] prints a message [msg] followed with a colon, unless it ends with a linebreak *)
let prt_msg (msg : string) : unit =
  if msg = "" then () else
  if msg.[String.length msg - 1] = '\n' then prt msg
  else prt ~suffix:": " msg

let prt_list ?(msg : string = "") ?(sep : string = "") (pr : 'a -> unit) (xs : 'a list) : unit =
  prt_msg msg;
  List.iter (fun x -> pr x; if sep <> "" then prt sep) xs

let prt_opt ?(msg : string = "") (empty : string) (pr : 'a -> unit) (xopt : 'a option) : unit =
  prt_msg msg;
  match xopt with
  | None -> prt ~suffix:"\n" empty
  | Some x -> pr x

let add_linebreak (msg : string) : string =
  if msg <> "" then msg ^ "\n" else msg

(*----------------------------------------------------------------------------------*)
(** Printing operations *)


(** Print paths *)

let path ?(msg : string = "") (p : path) : unit =
  prt_msg msg;
  prt ~suffix:"\n" (Path.path_to_string p)

let paths ?(msg : string = "") (ps : paths) : unit =
  prt_list ~msg path ps

(* Print terms *)

let trm ?(style = Default) ?(msg : string = "") (t : trm) : unit =
  prt_msg msg;
  let custom_style = to_custom_style style in
  let t =
    if custom_style.decode then begin
      if not (Trm.trm_is_mainfile t) then begin
        prt "WARNING: trm: unsupported decoding of non root trm, falling back on printing encoded term\n";
        t
      end else begin
        Ast_fromto_AstC.cfeatures_intro t
      end
    end else t
    in
  let st =
    match custom_style.print with
    | Lang_AST style -> Ast_to_text.ast_to_string ~style t
    | Lang_C style -> AstC_to_c.ast_to_string ~style t
    in
  prt ~suffix:"\n" st

let trms ?(style = Default) ?(msg : string = "") (ts : trms) : unit =
  prt_list ~msg trm ts

(* DEPRECATED: use ShowAt.trm []
let ast ?(style = Default) ?(msg : string = "") () : unit =
  trm ~style ~msg (**:(add_linebreak msg)*) (Trace.ast ())
*)

(* types *)

let typ ?(msg : string = "") (t : typ) : unit =
  let t_str = AstC_to_c.typ_to_string t in
  prt_msg msg;
  prt ~suffix:"\n" t_str

let typ_opt ?(msg : string = "") (topt : typ option) : unit =
  prt_opt ~msg "<no_typ>" typ topt

let typs ?(msg : string = "") (ts : typ list) : unit =
  prt_list ~msg typ ts

(* marks *)

let marks ?(msg : string = "") (t : trm) : unit =
  prt_msg msg;
  prt ~suffix:"\n" (Tools.list_to_string ~sep:"; " ~bounds:["[";"]"] (Mark.trm_get_marks t))

(* annot *)

let annot ?(msg : string = "") (t : trm) : unit =
  prt_msg msg;
  let d = Ast_to_text.(print_trm_annot (default_style())) t in
  prt ~suffix:"\n" (Tools.document_to_string d)

let cstyle ?(msg : string = "") (t : trm) : unit =
  prt_msg msg;
  let d = Ast_to_text.(print_cstyles_annot (default_style())) (Trm.trm_get_cstyles t) in
  prt ~suffix:"\n" (Tools.document_to_string d)

(* desc *)

let desc ?(msg : string = "") (t : trm) : unit =
  prt_msg msg;
  prt ~suffix:"\n" (trm_desc_to_string t.desc)


(*----------------------------------------------------------------------------------*)
(** Functions that show things at a given target in the current AST. *)

module At = struct

  (* Combinators *)

  let at (f: path -> trm -> unit) ?(msg : string = "") (tg : Target.target) : unit =
    let ps = Target.resolve_target_current_ast tg in
    prt_msg msg;
    let nbps = List.length ps in
    if nbps > 1
      then prt (sprintf "target resolves to %d paths\n" nbps);
    List.iteri (fun i p ->
      if nbps > 1 then prt (sprintf "[occ #%d] " (i + 1));
      f p (Target.resolve_path_current_ast p)
    ) ps

  let at_trm (f: trm -> unit) ?(msg : string = "") (tg : Target.target) : unit =
    at ~msg (fun _p t -> f t) tg

  (* Operations *)

(* DEPRECATED: use ShowAt.trm []
  let ast ?(msg : string = "") (tg : Target.target) : unit =
    if tg <> [] then fail None "ShowAt.ast: can only be called on the root, with argument []";
    ast ~msg ()
    *)

  let path ?(msg : string = "") (tg : Target.target) : unit =
    at ~msg (fun p _t -> path p) tg

  let trm ?(style = Default) ?(msg : string = "") (tg : Target.target) : unit =
    at_trm ~msg (trm ~style) tg

  let typ = at_trm (fun t -> typ_opt t.typ)
  let marks = at_trm marks
  let annot = at_trm annot
  let cstyle = at_trm cstyle
  let desc = at_trm desc

  (* DEPRECATED

  let trm ?(style = Default) = at_trm (trm ~style)

  let typ ?(msg : string = "") (tg : Target.target) : unit =
    at_trm ~msg (fun t -> typ_opt t.typ) tg

  let marks ?(msg : string = "") (tg : Target.target) : unit =
    at_trm ~msg marks tg

  let annot ?(msg : string = "") (tg : Target.target) : unit =
    at_trm ~msg annot tg

  let desc ?(msg : string = "") (tg : Target.target) : unit =
    at_trm ~msg desc tg
  *)

  (* TODO: printer for resources and ctx and usage informations *)

  (* LATER: is_statement *)
  (* LATER: arith *)
end


(*----------------------------------------------------------------------------------*)
(** Show functions whose output can be viewed as a diff or as steps in trace. *)

(* [ast ()] prints on the left-hand side the C code, and on the right-hand side the
    internal AST code *)
let ast ?(msg : string = "show-ast") () : unit =
  let ast_left = Trace.ast() in
  let ast_right = ast_left in
  let style_left = Style.default_custom_style () in
  let cstyle_default = AstC_to_c.(default_style()) in
  let style_right = { style_left with
    decode = false;
    print = Lang_C { cstyle_default with optitrust_syntax = true } } in
  Trace.show_step ~name:msg ~ast_left ~ast_right ~style_left ~style_right ()

(* [target tg] shows the C code on the left-hand side of the diff, and the code decorated
   with marks as comments on the terms that the target [tg] resolves to. *)
let target ?(msg : string = "show-target") ?(line : int = -1) ?(types : bool = false) (tg : target) : unit =
  let add_marks_and_get_ast () : trm =
    (* Calling [enable_multi_targets] to automatically add [nbMulti] if there is no occurence constraint. *)
    let tg = enable_multi_targets tg in
    let marks_base = show_next_id() in
    let mark_of_occurence (i:int) : string =
      if !Flags.keep_marks_added_by_target_show
        then Printf.sprintf "%d_%d" marks_base i
        else Printf.sprintf "%d" i
      in
    applyi (fun i t p ->
      let m = mark_of_occurence i in
      match Path.last_dir_before_inv p with
      | Some (p, k) ->
        target_between_show_transfo m k t p
      | None -> target_show_transfo ~types m t p
    ) tg;
    Trace.ast() in
  let ast_left = Trace.ast() in
  let ast_right =
    if !Flags.keep_marks_added_by_target_show
      then add_marks_and_get_ast()
      else step_backtrack ~discard_after:true add_marks_and_get_ast in
  let style_left = Style.default_custom_style () in
  let style_right = style_left in
  Trace.show_step ~name:msg ~ast_left ~ast_right ~style_left ~style_right ()

(* [tg] is an alias for [target] *)
let tg = target

(* [res] shows the C code on the left-hand side of the diff, and the code decorated
   with computed resources on the right-hand side.
   The option [~ast] allows to provide a specific ast, otherwise resources are
   reported for the full current ast.  *)
let res ?(msg : string = "show-resources") ?(ast: trm = Trace.ast ()) () : unit =
  let ast_left = Trace.ast() in
  let ast_right = (*Ast_fromto_AstC.computed_resources_intro *) ast_left in
  let style_left = Style.default_custom_style () in
  let aststyle_default = Ast.default_style () in
  let aststyle = { aststyle_default with print_generated_ids = true } in
  let cstyle_default = AstC_to_c.(default_style()) in
  let cstyle = { cstyle_default with
    ast = aststyle;
    optitrust_syntax = true; } in
  (*let customstyle_default = Style.default_custom_style() in*)
  let style_right = { (*customstyle_default with*)
      decode = false;
      typing = { typing_resources = true };
      print = Lang_C cstyle } in
  Trace.show_step ~name:msg ~ast_left ~ast_right ~style_left ~style_right ()



(* LATER: Fix me
(* [show_type ~line ~reparse tg]: an alias for show with the argument [types] set to true. *)
let show_type ?(line : int = -1) (*DEPRECATED?(reparse : bool = false)*) (tg : target) : unit =
  show ~line (* DEPRECATED ~reparse*) ~types:true tg
*)

(*----------------------------------------------------------------------------------*)


(* LATER

var_to_string ?style ...
typconstr_to_string
lit_to_string
upop_string
binop_to_string
prim_to_doc style
val_to_string
typedef_to_string

*)



(* DEPRECATED
type debug_trm_style =
  | Display (* C with no decoding *)
  | ContractDisplay (* C with no decoding + contracts *)
  | Internal (* text repr *)
  | InternalDisplay (* C in OptiTrust syntax *)
(* TODO instead, with predefined vals: *)
type c_style = { foo: unit } (* TODO *)
type ocaml_style = { foo: unit } (* TODO *)
type ast_style = { foo: unit } (* TODO *)

(* TODO: move to Ast_to_c and Ast_to_text *)
type language_style = OptiTrust of c_style | C of c_style | OCaml of ocaml_style | AST of ast_style
(* The record [print_resource_style] contains a list of flags to determine *)
type print_resource_style =
type print_language = {
  decode: bool; (* perform decoding *)
  contract: bool; (* print loop contract *)
  pretty_matrix_notation: bool; (* print t[MINDEX(n,m,a,b)] as t[a][b] *)
  var_id: bool; (* print internal variable identifiers *)
  string_repr: bool; (* print string representation for expressions *)
  mark: bool; (* print marks *)
  annot: bool; (* print annotations *)
  (* LATER: node_id: bool; print internal AST node identifier *)
} {
  before: bool;
  after: bool;
  contract_invoc: bool;
  ctx: bool;
  usage: bool;
  post_inst: bool;
}
  res: res_style;
  decode: bool; (* perform decoding *)

type style = {
  common: common_style;
  lang: language_style;
}
*)

(* = Show.trm ~style msg (Target.resolve_path p) *)
(* = ShowAt.trm ~style msg (Target.of_path p) *)
(* let current_ast_at_path ?(style = Display) ?(msg : string = "") (p : Path.path) : *)
