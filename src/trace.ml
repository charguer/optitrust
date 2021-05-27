open Ast

type context =
  {extension : string; directory : string; prefix : string; includes : string;
   clog : out_channel}

(* list of context, AST stack *)
let trace : (context * (trm Stack.t)) list ref =
  ref [(init_ctx, Stack.create ())]


let get_trace () : (contex * (trm Stack.t)) = 
  !trace

let set_trace (ctx : contex) (astStack : trm Stack.t) : unit =
  trace := [ctx, astStack]

