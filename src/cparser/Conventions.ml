open AST
open BinInt
open BinNums
open Conventions1
open List0
open Locations

(** val max_outgoing_1 : coq_Z -> loc -> coq_Z **)

let max_outgoing_1 accu = function
| R _ -> accu
| S (sl, ofs, ty) ->
  (match sl with
   | Outgoing -> Z.max accu (Z.add ofs (typesize ty))
   | _ -> accu)

(** val max_outgoing_2 : coq_Z -> loc rpair -> coq_Z **)

let max_outgoing_2 accu = function
| One l -> max_outgoing_1 accu l
| Twolong (l1, l2) -> max_outgoing_1 (max_outgoing_1 accu l1) l2

(** val size_arguments : signature -> coq_Z **)

let size_arguments s =
  fold_left max_outgoing_2 (loc_arguments s) Z0

(** val parameter_of_argument : loc -> loc **)

let parameter_of_argument l = match l with
| R _ -> l
| S (sl, n, ty) -> (match sl with
                    | Outgoing -> S (Incoming, n, ty)
                    | _ -> l)

(** val loc_parameters : signature -> loc rpair list **)

let loc_parameters s =
  map (map_rpair parameter_of_argument) (loc_arguments s)

(** val tailcall_is_possible : signature -> bool **)

let tailcall_is_possible sg =
  forallb (fun l -> match l with
                    | R _ -> true
                    | S (_, _, _) -> false)
    (regs_of_rpairs (loc_arguments sg))
