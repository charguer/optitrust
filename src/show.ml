open Printf
open Prelude
open Target

type debug_trm_style =
  | Display (* C with no decoding *)
  | ContractDisplay (* C with no decoding + contracts *)
  | Internal (* text repr *)
  | InternalDisplay (* C in OptiTrust syntax *)
(* TODO instead, with predefined vals: *)
type c_style = { foo: unit } (* TODO *)
type ocaml_style = { foo: unit } (* TODO *)
type ast_style = { foo: unit } (* TODO *)
type language = OptiTrust of c_style | C of c_style | OCaml of ocaml_style | AST of ast_style
(* TODO: move to Ast_to_c and Ast_to_text *)
type res_style = {
  before: bool;
  after: bool;
  contract_invoc: bool;
  ctx: bool;
  usage: bool;
  post_inst: bool;
}
type style = {
  contract: bool;
  decode: bool;
  lang: language;
  res: res_style;
  beautify_mindex: bool;
  var_id: bool;
  string_repr: bool;
  mark: bool;
  annot: bool;
  loc: bool; (* LATER *)
  is_statement: bool; (* LATER: deprecate *)
  node_id: bool; (* LATER *)
}
let c_enc = { contract = true; decode = false; lang = C { foo = () } }
let c = { contract = true; decode = true; lang = C { foo = () } }

let prt = printf
let prtmsg msg = if msg = "" then () else prt "%s: " msg

let path ?(msg : string = "") (p : path) : unit =
  prtmsg msg;
  prt "%s\n" (Path.path_to_string p)

let paths ?(msg : string = "") (ps : paths) : unit =
  prtmsg msg;
  prt "%s\n" (Tools.list_to_string (List.map Path.path_to_string ps))


let trm ?(style = ContractDisplay) ?(msg : string = "") (t : trm) : unit =
  let t_str = match style with
  | Display -> AstC_to_c.ast_to_string t
  | ContractDisplay -> AstC_to_c.ast_to_string (Ast_fromto_AstC.contract_intro (Ast_fromto_AstC.ghost_args_intro t))
  | Internal -> Ast_to_text.ast_to_string t
  | InternalDisplay -> AstC_to_c.ast_to_string ~optitrust_syntax:true t
  in
  prtmsg msg;
  prt "%s\n" t_str

let trms ?(msg : string = "") (ts : trms) : unit =
  prtmsg msg;
  prt "%s\n" (Tools.list_to_string (List.map AstC_to_c.ast_to_string ts))

let typ ?(msg : string = "") (t : typ) : unit =
  let t_str = AstC_to_c.typ_to_string t in
  prtmsg msg;
  prt "%s\n" t_str

let typ_opt ?(msg : string = "") (t : typ option) : unit =
  printmsg msg;
  match t with
  | None -> prt "typ = None"
  | Some t -> typ t

let typs ?(msg : string = "") (ts : typ list) : unit =
  prtmsg msg;
  prt "%s\n" (Tools.list_to_string (List.map AstC_to_c.typ_to_string ts))

let current_ast ?(style = Display) ?(msg : string = "") : unit =
  trm ~style msg (Trace.ast ())

(* = Show.trm ~style msg (Target.resolve_path p) *)
(* = ShowAt.trm ~style msg (Target.of_path p) *)
(* let current_ast_at_path ?(style = Display) ?(msg : string = "") (p : Path.path) : *)

(** Functions that show things at a given target in the current AST. *)
module At = struct
  let at ?(msg : string = "") (f: trm -> unit) (tg : Target.target) : unit =
    let ps = Target.resolve_target_current_ast tg in
    prtmsg msg;
    let nbps = List.length ps in
    prt "target resolves to %d paths\n" nbps;
    List.iteri (fun i p ->
      if nbps > 1 then prt "[occ #%d] " (i + 1);
      f (Target.resolve_path_current_ast p)
    ) ps

  (* TODO: option for decoding *)
  let trm ?(style = Display) ?(msg : string = "") (tg : Target.target) : unit =
    at ~msg (trm ~style) tg

  (* styles: ast_to_c ast_to_text ast_to_*)
  let typ (* ? ?(style = Display) *) ?(msg : string = "") (tg : Target.target) : unit =
    at ~msg (fun t -> typ_opt t.typ) tg

  (* TODO: res *)
  (* TODO: path *)
  (* TODO: arith *)
  (* TODO: marks *)
  (* TODO: annot *)
  (* TODO: desc *)
  (* TODO: stmt *)
  (* TODO: info *)
end

(* TODO: take functions from Target.ml, and split into Show and ShowAt = Show.At modules. *)
