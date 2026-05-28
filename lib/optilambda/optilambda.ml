(** Public entry point for the OptiLambda textual language. *)

module Style = Optilambda_style
module Printer = Optilambda_printer

type style = Style.style

let default_style = Style.default

let trm_to_doc = Printer.trm_to_doc
let trm_to_string = Printer.trm_to_string
let typ_to_doc = Printer.typ_to_doc
let typ_to_string = Printer.typ_to_string

