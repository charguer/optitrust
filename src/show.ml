open Printf
open Prelude
open Target

(* Usage:
     Show.trm ~msg:"foo:" t
     Show.trm ~msg:"foo:" tg
     Show.trm ~msg:"foo:" (Target.resolve_path p)
     ShowAt.trm ~msg:"foo:" (Target.of_path p)
*)

(*----------------------------------------------------------------------------------*)
(* Printing options *)

type style =
  | XCustom of custom_style
  | XDefault
  | XC
  | XInternal
  | XInternalAst
  | XInternalAstOnlyDesc

and custom_style = {
  decode : bool; (* TODO: decode on non full ASTs? *)
  print : print_language }

and print_language =
  | AST of Ast_to_text.style
  | C of AstC_to_c.style
  (* Redundand constructors, to avoid need for parentheses,
     e.g. ~style:XC  instead of ~style:(c()) *)


(** Common printing options *)

let c () : style  =
  XCustom {
    decode = true;
    print = C ( AstC_to_c.( default_style ()) ) }

let internal () : style =
  let s = AstC_to_c.default_style () in
  XCustom {
    decode = false;
    print = C { s with optitrust_syntax = true } }

let internal_ast () : style  =
  let s = Ast_to_text.default_style () in
  XCustom {
    decode = true;
    print = AST s }

let internal_ast_only_desc () : style  =
  let s = Ast_to_text.default_style () in
  XCustom {
    decode = false;
    print = AST { s with only_desc = true } }

let default_style () = c

(* [resolve_style style] eliminates the redundant constructors *)
let resolve_style (style : style) : custom_style =
  match style with
  | XCustom c -> c
  | XDefault -> default_style()
  | XC -> c()
  | XInternal -> internal()
  | XInternalAst -> internal_ast()
  | XInternalAstOnlyDesc -> internal_ast_only_desc()


(*----------------------------------------------------------------------------------*)
(** Printing combinators *)

let prt = printf

(* TODO: should the \n be included by default? *)

(** [prt_msg msg] prints a message [msg] followed with a colon *)
let prt_msg (msg : string) : unit =
  if msg = "" then () else prt "%s: " msg

let prt_list ?(msg : string = "") ?(sep : string = "") (pr : 'a -> unit) (xs : 'a list) : unit =
  prt_msg msg;
  List.iter (fun x -> pr x; if sep <> "" then prt "%s" sep) xs

let prt_opt ?(msg : string = "") (empty : string) (pr : 'a -> unit) (xopt : 'a option) : unit =
  prt_msg msg;
  match xopt with
  | None -> prt "%s\n" empty
  | Some x -> pr x

(*----------------------------------------------------------------------------------*)
(** Printing operations *)

(** Print paths *)

let path ?(msg : string = "") (p : path) : unit =
  prt_msg msg;
  prt "%s\n" (Path.path_to_string p)

let paths ?(msg : string = "") (ps : paths) : unit =
  prt_list ~msg path ps

(* Print terms *)

let trm ?(style = XDefault) ?(msg : string = "") (t : trm) : unit =
  prt_msg msg;
  let custom_style = resolve_style style in
  let t =
    if custom_style.decode then begin
      if not trm_is_mainfile t then begin
        prt "%s\n" "WARNING: trm: unsupported decoding of non root trm, falling back on printing encoded term";
        t
      end else begin
        Ast_fromto_AstC.cfeatures_intro t
      end
    end else t
    in
  let st =
    match t.print with
    | AST style -> AstC_to_text.ast_to_string ~style t
    | C style -> AstC_to_c.ast_to_string ~style t
    in
  prt "%s\n" st

let trms ?(style = XDefault) ?(msg : string = "") (ts : trms) : unit =
  prt_list ~msg trm ts

let ast ?(style = XDefault) ?(msg : string = "") : unit =
  trm ?style ?msg (Trace.ast ())

(* types *)

let typ ?(msg : string = "") (t : typ) : unit =
  let t_str = AstC_to_c.typ_to_string t in
  prt_msg msg;
  prt "%s\n" t_str

let typ_opt ?(msg : string = "") (topt : typ option) : unit =
  prt_opt ~msg "<no_typ>" typ topt

let typs ?(msg : string = "") (ts : typ list) : unit =
  prt_list ~msg typ ts


(*----------------------------------------------------------------------------------*)
(** Functions that show things at a given target in the current AST. *)

module At = struct

  let at ?(msg : string = "") (f: trm -> unit) (tg : Target.target) : unit =
    let ps = Target.resolve_target_current_ast tg in
    prt_msg msg;
    let nbps = List.length ps in
    prt "target resolves to %d paths\n" nbps;
    List.iteri (fun i p ->
      if nbps > 1 then prt "[occ #%d] " (i + 1);
      f (Target.resolve_path_current_ast p)
    ) ps

  let trm ?(style = XDefault) ?(msg : string = "") (tg : Target.target) : unit =
    at ~msg (trm ~style) tg

  let typ ?(msg : string = "") (tg : Target.target) : unit =
    at ~msg (fun t -> typ t.typ) tg

  (* TODO: res *)
  (* TODO: path *)
  (* TODO: arith *)
  (* TODO: marks *)
  (* TODO: annot *)
  (* TODO: desc *)
  (* TODO: stmt *)
  (* TODO: info *)

end


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
