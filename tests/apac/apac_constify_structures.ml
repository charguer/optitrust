open Optitrust
open Target
open Typ
open Ast

let _ = Run.script_cpp (fun () ->
            !! Apac_constification.constify [
                nbAny;
                cFunDefAndDecl ""
              ];
          )
