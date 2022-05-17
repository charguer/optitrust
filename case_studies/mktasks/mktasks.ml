open Optitrust
open Target
open Ast

type adj = (var, var list) Hashtbl.t

let adj_add adj f g =
  let gs = try Hashtbl.find adj f
           with Not_found -> [] in
  Hashtbl.replace adj f (g::gs)

let graphdep (t : trm) : adj =
  let adj = Hashtbl.create 10 in
  let rec aux (curf : var option) (t : trm) : unit =
    match t.desc with
    | Trm_let_fun (f, rettyp, targs, body) ->
        if (curf <> None) then failwith "nested functions";
        if Hashtbl.mem adj f then failwith "function occurs several time";
        Hashtbl.add adj f [];
        aux (Some f) body
    | Trm_apps (g, args) ->
        begin match curf with
        | None -> failwith "function call not inside a function def"
        | Some f -> adj_add f g
        end
    | _ -> trm_iter aux t
    in
  aux None t;
  adj

type proto = {
  proto_rettype : typ;
  proto_argtypes : typ list; }


let run_on_functions (f : trm -> (var * typ * (typed_vars) * trm) -> unit) : unit =
  let rec aux (t : trm) : unit =
    begin match t.desc with
    | Trm_let_fun (f, rettype, targs, body) -> f t (f, rettype, targs, body)
    end;
    trm_iter aux t
    in
  aux t

let gather_prototype : (var, proto) Hashtbl.t =
   let protos = Hashtbl.create 10 in
   run_on_functions (fun t (f, proto_rettype, proto_argtypes, body) ->
       Hashtbl.add protos f { proto_rettype; proto_argtypes });
   protos


let gather_prototype_a_la_mano : (var, proto) Hashtbl.t =
  let protos = Hashtbl.create 10 in
  let rec aux (t : trm) : unit =
    match t.desc with
    | Trm_let_fun (f, proto_rettype, proto_argtypes, body) ->
        Hashtbl.add protos f { proto_rettype; proto_argtypes }
    | _ -> trm_iter aux t
    in
  aux t;
  protos


let add_const_marks (funisconst : (var, bool) Hashtbl.t) (t : trm) : trm =
 let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_let_fun (f, proto_rettype, proto_argtypes, body) ->
        let isconst =
          try Hashtbl.find funisconst f
          with Not_found -> failwith "fun not in funisconst" in
        (* let t = trm_map aux t in *)
        if isconst
          then trm_annot_add Fun_const t
          else t
    | _ -> trm_iter aux t
    in
  aux t

let assign_unique_ids_to_variables // binding points and occurences

let make_malloc =
  Target.transfo_on_targets (Ast.trm_annot_remove Stackvar)



let add_possible_const () =
  let g = graphdep (ast()) in
  let tbl = canbeconst g in
  set_const tbl

let _ = Run.script_cpp (fun () ->

  !! add_possible_const ();

  show [cVarDef "x"];

  !! make_malloc [cVarDef "y"];

  !! Variable_basic.bind "u" [nbMulti; cTopFunDef "f"; cFun "g"; dArg 1];

)



(* NOTE:
let make_malloc tg =
  Target.transfo_on_targets (fun t Ast.trm_annot_remove Stackvar t) tg
*)


(*

- extract all nested calls, except arithmetic ones
  generated names

- var from stack to heap alloc
  if var appears as a function argument
  and not at top of function



  void foo() {
     int a;

     if (true) {
      int c;

      int d; // to malloc
      f(d);
     }
     f(a)
     int b;
     g(a,b)
  }

  int main()
    foo()









*)