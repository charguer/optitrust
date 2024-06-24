open Ast

type error_context = {
  ctx_desc: string; (* Optional description of the link between this context and the error, used for errors with multiple contexts *)
  path: Dir.path option;
  trm: trm option;
  loc: location;
}

(** [Contextualized_error]: exception raised within a given context.
    When multiple *)
exception Contextualized_error of error_context list * exn

let contextualized_error (ctx : error_context list) (error : string) : 'a =
  raise (Contextualized_error (ctx, Failure error))

let contextualized_exn (ctx : error_context list) (exn : exn) : 'a =
  Printexc.(raise_with_backtrace (Contextualized_error (ctx, exn)) (get_raw_backtrace ()))

let path_error_context ?(desc="") (p : Dir.path) : error_context =
  {
    ctx_desc = desc;
    path = Some p;
    trm = None;
    loc = None
  }

(* LATER: use Path.fail or fail ~path *)
(** [path_fail p err]: fails with error [error] raised on path [p] *)
let path_fail (p : Dir.path) (error : string) : 'a =
  contextualized_error [path_error_context p] error

let path_exn (p : Dir.path) ?(error : string = "") (exn : exn) : 'a =
  contextualized_exn [path_error_context p] exn

let trm_error_context ?(desc="") (t : trm) : error_context =
  {
    ctx_desc = desc;
    path = None;
    trm = Some t;
    loc = t.loc
  }

(* LATER: move to trm.ml or have fail ~trm *)
(** [trm_fail t err]: fails with error [error] raised on term [t] *)
let trm_fail (t : trm) (error : string) : 'a =
  contextualized_error [trm_error_context t] error

let unsome_or_trm_fail (t: trm) (error: string) (x_opt : 'a option) : 'a =
    match x_opt with
    | Some x -> x
    | None -> trm_fail t error

let loc_error_context ?(desc="") (loc : location) : error_context =
  {
    ctx_desc = desc;
    path = None;
    trm = None;
    loc = loc
  }

let loc_fail (loc : location) (error : string) : 'a =
  contextualized_error [loc_error_context loc] error

let trm_printer : (trm -> string) ref = ref (fun _ -> "<no printer>")
(* Replace with (ast_to_string ~style:(only_desc_style ())) *)

let _ = Printexc.register_printer (function
  | Contextualized_error (ctx, exn) ->
    let ctx_lines = List.concat_map (fun c -> List.filter_map (fun x -> x) [
        (if c.ctx_desc = "" then None else Some c.ctx_desc);
        Option.map (fun p -> "@ path " ^ Dir.path_to_string p) c.path;
        Option.map (fun t -> "@ term " ^ !trm_printer t) c.trm;
        Option.map (fun loc -> "@ loc " ^ (loc_to_string (Some loc))) c.loc
      ]) ctx
     in
    Some (String.concat ":\n" (ctx_lines @ [(Printexc.to_string exn)]))
  | _ -> None)
