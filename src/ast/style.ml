(**
  A "style" describes how a piece of AST should be pretty-printed,
  in particular if using C syntax, or OptiTrust syntax, or raw constructor syntax.
  Depending on the syntax targeted, different options may be set.
  The files Ast.ml, AstC_to_c.ml and AstC_to_text.ml each define a set of options.

  For convenience to the user, this file defines commonly used styles:
  - Default : currently, equivalent to C
  - C: C syntax
  - Internal: OptiTrust syntax, almost like C but showing the internal encoding
  - InternalAst: textual representation of AST constructors, with all information
  - InternalAstOnlyDesc: lighter textual representation of AST constructors

*)

(*----------------------------------------------------------------------------------*)
(* Printing options *)

type style =
  | Custom of custom_style
  | Default
  | C
  | Internal
  | InternalAst
  | InternalAstOnlyDesc

and typing_style = {
  typing_contracts : bool;
  typing_ghost : bool;
  typing_ctx_res : bool;
  typing_produced_res : bool;
  typing_used_res : bool;
  typing_joined_res : bool;
  typing_framed_res : bool;
  typing_contract_inst : bool;
  print_generated_res_ids: bool; (* print auto-generated resource names *)
  }

and custom_style = {
  decode : bool; (* TODO: decode on non full ASTs? *)
  typing : typing_style;
  print : print_language }

and print_language =
  | Lang_AST of Ast_to_text.style
  | Lang_C of AstC_to_c.style
  (* Redundant constructors, to avoid need for parentheses,
     e.g. ~style:XC  instead of ~style:(c()) *)

[@@deriving show]

let ast_style_of_custom_style (style : custom_style) : Ast.style =
  match style.print with
  | Lang_AST s -> s.ast
  | Lang_C s -> s.ast

(** Typing styles *)

let typing_all : typing_style = {
  typing_contracts = true;
  typing_ghost = true;
  typing_ctx_res = true;
  typing_produced_res = true;
  typing_joined_res = true;
  typing_used_res = true;
  typing_framed_res = true;
  typing_contract_inst = true;
  print_generated_res_ids = true;
}

let typing_none : typing_style = {
  typing_contracts = false;
  typing_ghost = false;
  typing_ctx_res = false;
  typing_produced_res = false;
  typing_joined_res = false;
  typing_used_res = false;
  typing_framed_res = false;
  typing_contract_inst = false;
  print_generated_res_ids = false;
}

let typing_annot : typing_style =
  { typing_none with
      typing_ghost = true;
      typing_contracts = true; }

let typing_ctx : typing_style =
  { typing_annot with typing_ctx_res = true; }

let typing_usage : typing_style =
  { typing_ctx with
      typing_used_res = true;
      print_generated_res_ids = true; }

let typing_all_but_frame : typing_style =
  { typing_all with typing_framed_res = false; }

(* typing display for interactive html document *)
let typing_html : typing_style =
  typing_all_but_frame (* could also be typing_all *)

(** Common printing options *)

let c ?(typing_style : typing_style = typing_annot) () : style  =
  Custom {
    decode = true;
    typing = typing_style;
    print = Lang_C ( AstC_to_c.( default_style ()) ) }

let c_res () : style  =
  c ~typing_style:typing_all ()

let c_ctx () : style  =
  c ~typing_style:typing_ctx ()

let internal () : style =
  let s = AstC_to_c.default_style () in
  Custom {
    decode = false;
    typing = { typing_annot with print_generated_res_ids = true };
    print = Lang_C { s with
      optitrust_syntax = true;
      ast = { s.ast with print_var_id = true } } }

let internal_ast () : style  =
  let s = Ast_to_text.default_style () in
  Custom {
    decode = false;
    typing = typing_annot;
    print = Lang_AST s }

let internal_ast_only_desc () : style =
  let s = Ast_to_text.default_style () in
  Custom {
    decode = false;
    typing = typing_annot;
    print = Lang_AST { s with only_desc = true } }

let default_custom_style () : custom_style =
  { decode = not !Flags.bypass_cfeatures && not !Flags.print_optitrust_syntax;
    typing = if !Flags.debug_html_view then typing_all else typing_annot;
    print = Lang_C (AstC_to_c.(default_style())) }

let custom_style_for_reparse () : custom_style =
  { decode = true;
    typing = typing_annot;
    print = Lang_C (AstC_to_c.style_for_reparse()) }

(* [to_custom_style style] eliminates the redundant constructors *)
let to_custom_style (style : style) : custom_style =
  let style_as_custom = match style with
    | Default -> Custom (default_custom_style())
    | Custom _ -> style
    | C -> c()
    | Internal -> internal()
    | InternalAst -> internal_ast()
    | InternalAstOnlyDesc -> internal_ast_only_desc()
    in
    match style_as_custom with
    | Custom custom_style -> custom_style
    | _ -> failwith "Show.to_custom_style: not custom"

