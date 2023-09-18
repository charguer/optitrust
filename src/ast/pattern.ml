open Ast
open Trm

exception PatternFailed

let (!) (inside: 'a -> 't -> 'b) (k:'t -> 'a) (v: 't): 'b = inside (k v) v
let __ (k: 'a) (v: 't): 'a = k
let (^|) (p1: 'a -> 't -> 'b) (p2: 'a -> 't -> 'b) (k: 'a) (v: 't) =
  try p1 k v with PatternFailed -> p2 k v

let check (f: 't -> bool) (k: 'a) (v: 't): 'a =
  if f v then k else raise PatternFailed
let eq (x: 't) : 'a -> 't -> 'a = check ((==) x)

let rec pattern_match (v: 'a) (ks: ('a -> 'b) list): 'b =
  match ks with
  | k :: ks ->
    begin try
      k v
    with PatternFailed -> pattern_match v ks
    end
  | [] -> raise PatternFailed

let trm_let (mut: 'a -> varkind -> 'b) (var: 'b -> var -> 'c) (typ: 'c -> typ -> 'd) (body: 'd -> trm -> 'e) (k: 'a) (t: trm): 'e =
  match trm_let_inv t with
  | Some (tmut, tvar, ttyp, tbody) ->
    let k = mut k tmut in
    let k = var k tvar in
    let k = typ k ttyp in
    let k = body k tbody in
    k
  | None -> raise PatternFailed

let trm_seq (fn: 'a -> trm mlist -> 'b) (k: 'a) (t: trm): 'b =
  match trm_seq_inv t with
  | Some seq -> fn k seq
  | None -> raise PatternFailed

let trm_apps (fn: 'a -> trm -> 'b) (args: 'b -> trm list -> 'c) (ghost_args: 'c -> (hyp * formula) list -> 'd) (k: 'a) (t: trm): 'd =
  match t.desc with
  | Trm_apps (f, a, ga) ->
    let k = fn k f in
    let k = args k a in
    let k = ghost_args k ga in
    k
  | _ -> raise PatternFailed

let trm_var (var: 'a -> var -> 'b) (k: 'a) (t: trm): 'b =
  match trm_var_inv t with
  | Some v -> var k v
  | None -> raise PatternFailed

let var_eq (v: var): 'a -> var -> 'a = check (var_eq v)

(* Probably useless? *)
let trm_apps_specific_var (v: var) args = trm_apps (trm_var (var_eq v)) args __

let nil (k: 'a) (l: _ list) : 'a =
  match l with
  | [] -> k
  | _ -> raise PatternFailed

let (^::) (fh: 'a -> 't -> 'b) (ft: 'b -> 't list -> 'c) (k: 'a) (l: 't list): 'c =
  match l with
  | h :: t ->
    let k = fh k h in
    let k = ft k t in
    k
  | _ -> raise PatternFailed

let trm_apps0 fn = trm_apps fn nil __
let trm_apps1 fn arg1 = trm_apps fn (arg1 ^:: nil) __
let trm_apps2 fn arg1 arg2 = trm_apps fn (arg1 ^:: arg2 ^:: nil) __
let trm_apps3 fn arg1 arg2 arg3 = trm_apps fn (arg1 ^:: arg2 ^:: arg3 ^:: nil) __

let trm_int (f: 'a -> int -> 'b) (k: 'a) (t: trm): 'b =
  match trm_int_inv t with
  | Some x -> f k x
  | _ -> raise PatternFailed

let trm_fun args ?(ret_type = __) ?(contract = __) body k t =
  match t.desc with
  | Trm_fun (targs, tret_type, tbody, tcontract) ->
    let k = args k targs in
    let k = ret_type k tret_type in
    let k = body k tbody in
    let k = contract k tcontract in
    k
  | _ -> raise PatternFailed

let trm_string f k t =
  match trm_lit_inv t with
  | Some (Lit_string str) -> f k str
  | _ -> raise PatternFailed

let pair f1 f2 k (x1, x2) =
  let k = f1 k x1 in
  let k = f2 k x2 in
  k
