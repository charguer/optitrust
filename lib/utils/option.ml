include Stdlib.Option

(******************************************************************************)
(*                          Extensions to Option                              *)
(******************************************************************************)

(** [to_string]: returns "None" or "Some [f v]" *)
let to_string (f : 'a -> string) (o : 'a option) : string =
  value ~default:"None" (
    map (fun v -> "Some " ^ f v) o)

(** [map f]: applies [f] on optional objects *)
let map (f : 'a -> 'b) (o : 'a option) : 'b option =
  match o with
  | None -> None
  | Some v -> Some (f v)

(** [map_or f v] applies [f] on the value if Some, otherwise returns [v]. *)
let map_or f v = fold ~none:v ~some:f

(** [and_ a b]: returns None if [a] is None, otherwise [b]. *)
let and_ (a : 'a option) (b : 'a option) : 'a option =
  match a with
  | None -> None
  | Some _ -> b

(** [or_ a b]: returns [a] if [a] is Some, otherwise [b]. *)
let or_ (a : 'a option) (b : 'a option) : 'a option =
  match a with
  | Some _ -> a
  | None -> b

(** [ors]: n-ary [option_or]. *)
let ors (opts : 'a option list) : 'a option =
  Stdlib.List.fold_left or_ None opts

(** [unsome x_opt]: extracts the underlying object of [x_opt] is there is one such object. *)
let unsome ?(error:string="Option.unsome found None") (x_opt : 'a option) : 'a =
  match x_opt with
  | Some x -> x
  | None -> failwith error

let or_else (x_opt : 'a option) (f : unit -> 'a option) : 'a option =
  match x_opt with
  | Some x -> x_opt
  | None -> f ()

let unsome_or_else (x_opt : 'a option) (f : unit -> 'a) : 'a =
  match x_opt with
  | Some x -> x
  | None -> f ()

let flat_map (f: 'a -> 'b list) (opt: 'a option): 'b list =
  value ~default:[] (map f opt)

(** [Monad]: when opened add the operators [let*] and [and*] for the
   option monad. *)
module Monad = struct
  let (let*) = bind

  let (and*) a b =
    let* a in
    let* b in
    Some (a, b)
end
