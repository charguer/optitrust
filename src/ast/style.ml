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

and typing = {
  typing_resources : bool;
  (* LATER: finer grained printing of resources
  typing_ctx : bool;
  typing_usage : bool; *) }

and custom_style = {
  decode : bool; (* TODO: decode on non full ASTs? *)
  typing : typing;
  print : print_language }

and print_language =
  | Lang_AST of Ast_to_text.style
  | Lang_C of AstC_to_c.style
  (* Redundand constructors, to avoid need for parentheses,
     e.g. ~style:XC  instead of ~style:(c()) *)


(** Common printing options *)

let c () : style  =
  Custom {
    decode = true;
    typing = { typing_resources = false };
    print = Lang_C ( AstC_to_c.( default_style ()) ) }

let cres () : style  =
  Custom {
    decode = true;
    typing = { typing_resources = true };
    print = Lang_C ( AstC_to_c.( default_style ()) ) }

let internal () : style =
  let s = AstC_to_c.default_style () in
  Custom {
    decode = false;
    typing = { typing_resources = false };
    print = Lang_C { s with
      optitrust_syntax = true;
      ast = { s.ast with print_var_id = true } } }

let internal_ast () : style  =
  let s = Ast_to_text.default_style () in
  Custom {
    decode = false;
    typing = { typing_resources = false };
    print = Lang_AST s }

let internal_ast_only_desc () : style =
  let s = Ast_to_text.default_style () in
  Custom {
    decode = false;
    typing = { typing_resources = false };
    print = Lang_AST { s with only_desc = true } }

let default_custom_style () : custom_style =
  { decode = not !Flags.print_optitrust_syntax;
    typing = { typing_resources = !Flags.display_resources };
    print = Lang_C (AstC_to_c.(default_style())) }

let custom_style_for_reparse () : custom_style =
  { decode = true;
    typing = { typing_resources = false };
    print = Lang_C (AstC_to_c.style_for_reparse()) }

(* [resolve_style style] eliminates the redundant constructors *)
let resolve_style (style : style) : custom_style =
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
    | _ -> failwith "Show.resolve_style: not custom"
