(**
  A "style" describes how a piece of AST should be pretty-printed,
  in particular if using C syntax, or OptiTrust syntax, or raw constructor syntax.
  Depending on the syntax targeted, different options may be set.
  The files Ast.ml, Ast_to_c.ml and AstC_to_text.ml each define a set of options.

  For convenience to the user, this file defines commonly used styles:
  - Default : currently, equivalent to C
  - C: C syntax
  - Internal: OptiTrust syntax, almost like C but showing the internal encoding
  - InternalAst: textual representation of AST constructors, with all information
  - InternalAstOnlyDesc: lighter textual representation of AST constructors

*)

(*----------------------------------------------------------------------------------*)
(* Printing options *)

type typing_style = {
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

type output_style = {
  decode : bool; (* TODO: decode on non full ASTs? *)
  typing : typing_style;
  print : print_language }

and print_language =
  | Lang_AST of Ast_to_text.style
  | Lang_C of Ast_to_c.style
  (* Redundant constructors, to avoid need for parentheses,
     e.g. ~style:XC  instead of ~style:(c()) *)

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

(** Common printing options *)

let c ?(typing_style : typing_style = typing_annot) () : output_style =
  { decode = true;
    typing = typing_style;
    print = Lang_C ( Ast_to_c.( default_style ()) ) }

let c_res () : output_style  =
  c ~typing_style:typing_all ()

let c_ctx () : output_style =
  c ~typing_style:typing_ctx ()

let internal () : output_style =
  let s = Ast_to_c.default_style () in
  { decode = false;
    typing = { typing_annot with print_generated_res_ids = true };
    print = Lang_C { s with
      optitrust_syntax = true;
      print_var_id = true } }

let internal_ast () : output_style =
  { decode = false;
    typing = typing_none;
    print = Lang_AST { Ast_to_text.style_full with
      print_var_id = !Flags.debug_var_id } }

let internal_ast_only_desc () : output_style =
  { decode = false;
    typing = typing_none;
    print = Lang_AST { Ast_to_text.style_desc with
      print_var_id = !Flags.debug_var_id } }

let default_style () : output_style =
  { decode = not !Flags.bypass_cfeatures && not !Flags.print_optitrust_syntax;
    typing = typing_annot;
    print = Lang_C (Ast_to_c.default_style ()) }

let style_for_reparse () : output_style =
  { decode = true;
    typing = typing_annot;
    print = Lang_C (Ast_to_c.style_for_reparse ()) }
