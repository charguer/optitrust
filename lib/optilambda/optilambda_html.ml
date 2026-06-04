(** HTML rendering helpers for OptiLambda.

    This module keeps HTML-specific rendering local to [optitrust.optilambda].
    The textual printer remains the source of truth for the visible code; the
    HTML layer adds metadata that webviews can use for hover/tooltips. *)

open Ast

let escape_html (s : string) : string =
  let buffer = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string buffer "&amp;"
      | '<' -> Buffer.add_string buffer "&lt;"
      | '>' -> Buffer.add_string buffer "&gt;"
      | '"' -> Buffer.add_string buffer "&quot;"
      | '\'' -> Buffer.add_string buffer "&#39;"
      | c -> Buffer.add_char buffer c)
    s;
  Buffer.contents buffer

let attr (name : string) (value : string) : string =
  Printf.sprintf " %s=\"%s\"" name (escape_html value)

let typ_title (style : Optilambda_style.style) (t : trm) : string option =
  match t.typ with
  | Some ty when not (Optilambda_printer.is_auto_type ty) ->
      Some ("type: " ^ Optilambda_printer.typ_to_string ~style ty)
  | _ -> None

let trm_to_html ?(style = Optilambda_style.default) (t : trm) : string =
  let code = Optilambda_printer.trm_to_string ~style t in
  let title_attr =
    match typ_title style t with
    | None -> ""
    | Some title -> attr "title" title
  in
  Printf.sprintf
    "<span class=\"optilambda optilambda-%s\" data-optilambda-representation=\"%s\"%s>%s</span>"
    (Optilambda_style.representation_to_string style.representation)
    (Optilambda_style.representation_to_string style.representation)
    title_attr
    (escape_html code)

let typ_to_html ?(style = Optilambda_style.default) (ty : typ) : string =
  Printf.sprintf "<span class=\"optilambda-type\">%s</span>" (escape_html (Optilambda_printer.typ_to_string ~style ty))
