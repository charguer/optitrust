open Optitrust
open Ast
open Trm
open Trm_unify
open Matrix_trm
open Typ

type range = trm * trm * trm
type index = trm

(**
   A [Star] represents a permission on an entire group.
   The [range] represents the inf,sup and iterator.
   The [index] represents the accessed cell. Look at group_repr for an example
   Example: for i in r -> .


   An [Index] represents a permission on a specific cell in an array.

   A [starindex] represents a permission on a single dimension of an array.
   You can have either:
     - Permission on the entire dimension: for i in r -> &t[MINDEX(..., i, ...)]
     - Permission on a single index of this dimension.
*)
type starindex =
  | Star of range * index
  | Index of index

(** [group_repr] is the internal representation of a permission on an array : For each dimension you can either have a [Star] or an [Index], the term [t] represents the array base
Example : for i1 in 0..n1 -> &x[MINDEX1(n1,n2,f(i1),2)] ---> [Star(0,n1,i1,f(i1)), Index(2)],x  *)

type group_repr = starindex list * trm

(**
   [focus_list] is the internal representation of the transformation
   that allows us to go from the required resources to the resources we have.

   Each entry is a quadruple: [group_repr] * [group_repr] * [var] * [trm],
   which specifies how to transform the first group into the second
   by instantiating [var] with [trm].
*)

type focus_list = (group_repr * group_repr) list

(**[PRINTING INFRASTRUCTURE] *)
let print_trm_string (t : trm) : string =
  let doc = Ast_to_text.print_trm Ast_to_text.default_style t in
  Tools.document_to_string doc

let print_range ((a, b, c) : range) : string =
  Printf.sprintf "(%s, %s, %s)" (print_trm_string a) (print_trm_string b) (print_trm_string c)

let print_starindex (s : starindex) : string =
  match s with
  | Index i -> Printf.sprintf "Index(%s)" (print_trm_string i)
  | Star (r, i) -> Printf.sprintf "Star(%s, %s)" (print_range r) (print_trm_string i)

let print_group_repr ((stars, t) : group_repr) : string =
  let stars_str = stars |> List.map print_starindex |> String.concat "; " in
  Printf.sprintf "([%s], %s)" stars_str (print_trm_string t)

let print_focus_list (f : focus_list) : unit =
  let content =
    f
    |> List.map (fun (g1, g2) ->
           Printf.sprintf "(%s, %s)" (print_group_repr g1) (print_group_repr g2)
       )
    |> String.concat " ; " in
  Printf.printf "[%s]\n" content

let starindex (min : int) (max : int) (ite : trm) (value : trm) =
  Star ((trm_int min, trm_int max, ite), value)

let simple_access (base : string) (dims : int list) (indices : trm list) =
  access (trm_toplevel_var base) (List.map (fun t -> trm_int t) dims) indices

(** [LIST OF TESTS] *)
let trm_v (s : string) = trm_toplevel_var s

(* for i in 0..10 -> t[MINDEX1(10,i)] *)

let from_focus1 = ([ starindex 0 10 (trm_toplevel_var "i") (trm_toplevel_var "i") ], trm_v "t")

(* t[MINDEX1(10,2)] *)
let to_focus1 = ([ Index (trm_int 2) ], trm_v "t")

(* direct transformation *)
let expected1 = [ (from_focus1, to_focus1) ]
let test1 = ((from_focus1, to_focus1), Some expected1)

(* for i1 in 0..10 -> for i2 in 0..10 -> t[MINDEX2(10,9,i1,i2)] *)
let simple_focus_from =
  ([ starindex 0 10 (trm_v "i1") (trm_v "i1"); starindex 0 10 (trm_v "i2") (trm_v "i2") ], trm_v "t")

(* for i2 in 0..10 ->  t[MINDEX2(10,9,2,i2)]*)
let simple_focus_to = ([ Index (trm_int 2); starindex 0 10 (trm_v "i2") (trm_v "i2") ], trm_v "t")

(* only one transformation *)
let simple_focus_expected = [ (simple_focus_from, simple_focus_to) ]
let simple_focus = ((simple_focus_from, simple_focus_to), Some simple_focus_expected)

(* for i1 in 0..10 -> for i3 in 0..10 -> for i4 in 0..10 -> t[MINDEX4(10,10,10,10,i1,2,i3,i4)]  *)
let general_simple_focus_from =
  ( [
      starindex 0 10 (trm_v "i1") (trm_v "i1");
      Index (trm_int 2);
      starindex 0 10 (trm_v "i3") (trm_v "i3");
      starindex 0 10 (trm_v "i4") (trm_v "i4");
    ],
    trm_v "t"
  )

(* for i1 in 0..10  -> for i4 in 0..10 -> t[MINDEX4(10,10,10,10,i1,2,3,i4)]  *)

let general_simple_focus_to =
  ( [
      starindex 0 10 (trm_v "i1") (trm_v "i1");
      Index (trm_int 2);
      Index (trm_int 3);
      starindex 0 10 (trm_v "i4") (trm_v "i4");
    ],
    trm_v "t"
  )

let general_simple_focus_expected = [ (general_simple_focus_from, general_simple_focus_to) ]

let general_simple_focus =
  ((general_simple_focus_from, general_simple_focus_to), Some general_simple_focus_expected)

(* for i1 in 0..n1 -> for i2 in 0..n2 -> t[MINDEX2(n1,n2,10-i1,i2)]  *)
let complex_access_from =
  ( [
      starindex 0 10 (trm_v "i1") (trm_sub ~typ:typ_int (trm_int 10) (trm_v "i1"));
      starindex 0 10 (trm_v "i2") (trm_v "i2");
    ],
    trm_v "t"
  )

(* for i1 in 0..n1 -> t[MINDEX2(n1,n2,10-i1,2)] *)
let complex_access_to =
  ( [
      starindex 0 10 (trm_v "i1") (trm_sub ~typ:typ_int (trm_int 10) (trm_v "i1")); Index (trm_int 2);
    ],
    trm_v "t"
  )

let complex_access_expected = [ (complex_access_from, complex_access_to) ]
let complex_access = ((complex_access_from, complex_access_to), Some complex_access_expected)

let identity (ind : string) =
  trm_exact_div_int (trm_add ~typ:typ_int (trm_v "ind") (trm_v "ind")) (trm_int 2)

(* for i1 in 0..n1 -> for i2 in 0..n2 -> t[MINDEX2(n1,n2,(i1+i1)/2,i2)]  *)

let complex_access_2_from =
  ( [ starindex 0 10 (trm_v "i1") (identity "i1"); starindex 0 10 (trm_v "i2") (trm_v "i2") ],
    trm_v "t"
  )
(* for i2 in 0..n2 -> t[MINDEX2(n1,n2,(c+c)/2,i2)]  *)

let complex_access_2_to =
  ( [ starindex 0 10 (trm_v "i1") (identity "c"); starindex 0 10 (trm_v "i2") (trm_v "i2") ],
    trm_v "t"
  )

let complex_access_2_expected = [ (complex_access_2_from, complex_access_2_to) ]
let complex_access_2 = ((complex_access_2_from, complex_access_2_to), Some complex_access_2_expected)

(* for i1 in 0..n1 -> for i2 in 0..10 ->for i3 in 0..10 ->for i4 in 0..10 -> t[MINDEX2(10,10,10,10,i1,i2,i3,i4)]  *)

let several_focus_from =
  ( [
      starindex 0 10 (trm_v "i1") (trm_v "i1");
      starindex 0 10 (trm_v "i2") (trm_v "i2");
      starindex 0 10 (trm_v "i3") (trm_v "i3");
      starindex 0 10 (trm_v "i4") (trm_v "i4");
    ],
    trm_v "t"
  )
(* for i1 in 0..n1  ->for i4 in 0..10 -> t[MINDEX2(10,10,10,10,i1,2,3,i4)]  *)

let several_focus_to =
  ([ starindex 0 10 (trm_v "i1") (trm_v "i1"); Index(trm_int 2); Index(trm_int 3); starindex 0 10 (trm_v "i4") (trm_v "i4") ], trm_v "t")

(* for i1 in 0..n1 -> for i3 in 0..10 ->for i4 in 0..10 -> t[MINDEX2(10,10,10,10,i1,2,i3,i4)]  *)

let several_focus_mid =
  ( [
      starindex 0 10 (trm_v "i1") (trm_v "i1");
      starindex 0 10 (trm_v "i3") (trm_v "i3");
      starindex 0 10 (trm_v "i4") (trm_v "i4");
    ],
    trm_v "t"
  )

let several_focus_expected =
  [ (several_focus_from, several_focus_mid); (several_focus_mid, several_focus_to) ]

let several_focus = ((several_focus_from, several_focus_to), Some several_focus_expected)

(* for i1 in 0..n1 -> for a in 0..2 -> t[MINDEX1(n1,i1+a) ~> Cell *)

(* what is this range ?  *)
let star_not_index_from =
  ( [ starindex 0 10 (trm_v "i") (trm_add_int (trm_v "i") (trm_v "a")) ],
    Resource_formula.formula_group (new_var "a") (trm_int 2) (trm_v "t")
  )

let star_not_index_to =
  ( [ Index (trm_add_int (trm_v "c") (trm_v "a")) ],
    Resource_formula.formula_group (new_var "a") (trm_int 2) (trm_v "t")
  )

let star_not_index_expected = [ (star_not_index_from, star_not_index_to) ]

(** [TEST THAT SHOULD NOT PASS]  *)
let star_no_index = ((star_not_index_from, star_not_index_to), star_not_index_expected)

(* t[MINDEX1(10,2)] *)
let index_to_star_from = ([ Index (trm_int 2) ], trm_v "t")

(* for i in 0..10 -> t[MINDEX1(10,i)] *)
let index_to_star_to = ([ starindex 0 10 (trm_v "i") (trm_v "i") ], trm_v "t")
let index_to_star_expected = None
let index_to_star = ((index_to_star_from, index_to_star_to), index_to_star_expected)
let not_full_star_from = ([ starindex 0 10 (trm_v "i") (trm_v "i") ], trm_v "t")
let not_full_star_to = ([ starindex 0 4 (trm_v "i") (trm_v "i") ], trm_v "t")
let not_full_star = ((not_full_star_from, not_full_star_to), None)

let no_possible_subst_from =
  ([ starindex 0 10 (trm_v "i") (trm_add_int (trm_v "i") (trm_int 1)) ], trm_v "t")

let no_possible_subst_to = ([ Index (trm_v "c") ], trm_v "t")
let no_possible_subst = ((no_possible_subst_from, no_possible_subst_to), None)

let tests =
  [
    test1;
    simple_focus;
    general_simple_focus;
    complex_access;
    complex_access_2;
    several_focus;
    index_to_star;
    not_full_star;
    no_possible_subst;
  ]

(** [ALGO]*)

(* index -> trm_index *)

(** [is_focusable star index] : determines whether a given star can be focused on a specific element
    Criteria for focus: This fonction determines whether
    for i in r -> H(i) can be focused into H'
    The function returns Some(t) if H(t) unifies with H' meaning that the star on i is focused on index t *)
let is_focusable (range, formula) index : bool =
  let _inf, _sup, star_index = range in
  (* i is directly a var *)
  let var_star_index = trm_inv trm_var_inv star_index in
  let subst_find = ref false in
  (* trm_unify to adapt *)
  let rec aux t =
    let subst = trm_subst_var var_star_index t formula in
    if are_same_trm index subst then begin
      Printf.printf "%s \n" (print_trm_string subst);
      subst_find := true
    end else
          trm_iter aux t in
  aux index;
  !subst_find

let are_same_range (mini1, maxi1, ite1) (mini2, maxi2, ite2) : bool =
  are_same_trm mini1 mini2 && are_same_trm maxi1 maxi2 && are_same_trm ite1 ite2

let are_same_group_repr ((stars1, t1) : group_repr) ((stars2, t2) : group_repr) : bool =
  are_same_trm t1 t2
  && List.length stars1 = List.length stars2
  && List.for_all2
       (fun s1 s2 ->
         match (s1, s2) with
         | Index i1, Index i2 -> are_same_trm i1 i2
         | Star ((a1, b1, c1), i1), Star ((a2, b2, c2), i2) ->
           are_same_trm a1 a2 && are_same_trm b1 b2 && are_same_trm c1 c2 && are_same_trm i1 i2
         | _, _ -> false
       )
       stars1 stars2

let are_same_focus_list (result : focus_list) (expected : focus_list) : bool =
  List.length result = List.length expected
  && List.for_all2
       (fun (g1a, g1b) (g2a, g2b) -> are_same_group_repr g1a g2a && are_same_group_repr g1b g2b)
       result expected

let base_access t =
  let base, access = trm_inv trm_array_access_inv t in
  base

(* To comment  *)

(** [build focus list]: Tries to build a [focus_list], i.e list of pairs of [group repr] tha represents unitary focuses that allows to goes from [from_group] to [to _group] *
Criteria for focus is described in the [is_focusable] function
*)
let build_focus_list (from_group : group_repr) (to_group : group_repr) : focus_list option =
  let stars_from, t1 = from_group in
  let stars_to, t2 = to_group in
  if not (List.length stars_from == List.length stars_to && are_same_trm t1 t2) then
    raise
      (Invalid_argument
         "Build focus: lists must have the same size and base_trm must be identical in 'from' and \
          'to'"
      )
  else
    let folder
        (acc : (starindex list * focus_list * int) option)
        (si_from : starindex)
        (si_to : starindex) =
      match acc with
      | None -> None
      | Some (current_group, acc_focus, ind) -> (
        match (si_from, si_to) with
        | Index _, Star _ -> None
        | Star (range1, index1), Star (range2, index2) ->
          if are_same_range range1 range2 && are_same_trm index1 index2 then
            Some (current_group, acc_focus, ind + 1)
          else
            None
        | Index i1, Index i2 ->
          if are_same_trm i1 i2 then Some (current_group, acc_focus, ind + 1) else None
        | Star (range, index), Index i2 ->
          if is_focusable (range, index) i2 then begin
            let new_group = List.update_nth ind (fun t -> Index i2) current_group in
            Some (new_group, acc_focus @ [ ((current_group, t1), (new_group, t2)) ], ind + 1)
          end else
                None
      ) in
    Option.map
      (fun (a, b, c) -> b)
      (List.fold_left2 folder (Some (stars_from, [], 0)) stars_from stars_to)

let run_tests f =
  Printf.printf "Testing the tests\n";
  List.iter
    (fun ((x, y), expected) ->
      match (f x y, expected) with
      | None, None ->
        Printf.printf "OK (both None) for input (%s, %s)\n" (print_group_repr x) (print_group_repr y)
      | None, Some _ ->
        Printf.printf "FAIL: result is None but expected a value for (%s, %s)\n"
          (print_group_repr x) (print_group_repr y)
      | Some result, None ->
        Printf.printf "FAIL: got a value but expected None for (%s, %s)\n" (print_group_repr x)
          (print_group_repr y);
        Printf.printf "Result:\n";
        print_focus_list result
      | Some result, Some expected_val ->
        if are_same_focus_list result expected_val then (
          Printf.printf "OK for:\n";
          print_focus_list expected_val
        ) else (
          Printf.printf "FAIL for:\n";
          Printf.printf "Result:\n";
          print_focus_list result;
          Printf.printf "Expected:\n";
          print_focus_list expected_val;
          Printf.printf "\n"
        )
    )
    tests

let () =
  Run.script_cpp (fun _ ->
      run_tests build_focus_list;
      ()
  )
