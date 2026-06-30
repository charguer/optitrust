open Monolith

module R = Naive_alist
module A = Optitrust_ast.Alist

let check_alist (seq : int A.t) : unit =
  A.check seq

let checked (seq : int A.t) : int A.t =
  check_alist seq;
  seq

let classify_exn (exn : exn) : string =
  match exn with
  | Invalid_argument _ -> "Invalid_argument"
  | Failure _ -> "Failure"
  | Assert_failure _ -> "Assert_failure"
  | _ -> Printexc.exn_slot_name exn

let same_exn_kind e1 e2 =
  String.equal (classify_exn e1) (classify_exn e2)

let raises f =
  match f () with
  | exception _ -> true
  | _ -> false

module C = struct
  let empty =
    checked A.empty

  let of_list xs =
    checked (A.of_list xs)

  let of_array xs =
    checked (A.of_array xs)

  let to_list =
    A.to_list

  let length =
    A.length

  let is_empty =
    A.is_empty

  let nth_opt seq index =
    A.nth_opt seq index

  let nth seq index =
    A.nth seq index

  let iter_to_list seq =
    let items = ref [] in
    A.iter (fun item -> items := item :: !items) seq;
    List.rev !items

  let iteri_to_list seq =
    let items = ref [] in
    A.iteri (fun index item -> items := (index, item) :: !items) seq;
    List.rev !items

  let insert seq index item =
    checked (A.insert index item seq)

  let insert_list seq index inserted =
    checked (A.insert_list index inserted seq)

  let insert_array seq index inserted =
    checked (A.insert_array index inserted seq)

  let split seq index =
    let left, right = A.split index seq in
    checked left, checked right

  let merge seq1 seq2 =
    checked (A.merge seq1 seq2)

  let rev seq =
    checked (A.rev seq)

  let map_succ seq =
    checked (A.map succ seq)

  let mapi_add seq =
    checked (A.mapi (fun index item -> index + item) seq)

  let update_nth_succ seq index =
    checked (A.update_nth index succ seq)

  let fold_left_sum seq =
    A.fold_left ( + ) 0 seq

  let find_map_first_even seq =
    A.find_map (fun item -> if item mod 2 = 0 then Some item else None) seq

  let for_all_nonnegative seq =
    A.for_all (fun item -> item >= 0) seq
end

module Harness = struct
  let length =
    Gen.lt 80

  let element =
    int_within (Gen.closed_interval (-1000) 1000)

  let small_index =
    int_within (Gen.closed_interval (-8) 88)

  let element_list =
    list ~length element

  let element_array =
    naive_array ~length element

  let alist =
    let check _model = check_alist, constant "check_alist" in
    declare_abstract_type ~var:"xs" ~check ()

  let () =
    override_exn_eq (fun _eq -> same_exn_kind)

  let declare_all () =
    declare "C.empty" alist R.empty C.empty;

    let spec = element_list ^> alist in
    declare "C.of_list" spec R.of_list C.of_list;

    let spec = element_array ^> alist in
    declare "C.of_array" spec R.of_array C.of_array;

    let spec = alist ^> element_list in
    declare "C.to_list" spec R.to_list C.to_list;

    let spec = alist ^> int in
    declare "C.length" spec R.length C.length;

    let spec = alist ^> bool in
    declare "C.is_empty" spec R.is_empty C.is_empty;

    let spec = alist ^> small_index ^> option int in
    declare "C.nth_opt" spec R.nth_opt C.nth_opt;

    let spec = (fun xs -> R.length xs > 0) % alist ^>> fun xs -> lt (R.length xs) ^> int in
    declare "C.nth" spec R.nth C.nth;

    let spec = alist ^> element_list in
    declare "C.iter_to_list"
      spec
      (fun xs ->
        let items = ref [] in
        R.iter (fun item -> items := item :: !items) xs;
        List.rev !items)
      C.iter_to_list;

    let spec = alist ^> list (int *** int) in
    declare "C.iteri_to_list"
      spec
      (fun xs ->
        let items = ref [] in
        R.iteri (fun index item -> items := (index, item) :: !items) xs;
        List.rev !items)
      C.iteri_to_list;

    let spec = alist ^>> fun xs -> le (R.length xs) ^> element ^> alist in
    declare "C.insert"
      spec
      (fun xs index item -> R.insert index item xs)
      C.insert;

    let spec = alist ^>> fun xs -> le (R.length xs) ^> element_list ^> alist in
    declare "C.insert_list"
      spec
      (fun xs index inserted -> R.insert_list index inserted xs)
      C.insert_list;

    let spec = alist ^>> fun xs -> le (R.length xs) ^> element_array ^> alist in
    declare "C.insert_array"
      spec
      (fun xs index inserted -> R.insert_array index inserted xs)
      C.insert_array;

    let spec = alist ^>> fun xs -> le (R.length xs) ^> alist *** alist in
    declare "C.split"
      spec
      (fun xs index -> R.split index xs)
      C.split;

    let spec = alist ^> alist ^> alist in
    declare "C.merge" spec R.merge C.merge;

    let spec = alist ^> alist in
    declare "C.rev" spec R.rev C.rev;

    let spec = alist ^> alist in
    declare "C.map_succ" spec (R.map succ) C.map_succ;

    let spec = alist ^> alist in
    declare "C.mapi_add"
      spec
      (R.mapi (fun index item -> index + item))
      C.mapi_add;

    let spec = (fun xs -> R.length xs > 0) % alist ^>> fun xs -> lt (R.length xs) ^> alist in
    declare "C.update_nth_succ"
      spec
      (fun xs index -> R.update_nth index succ xs)
      C.update_nth_succ;

    let spec = alist ^> int in
    declare "C.fold_left_sum" spec (R.fold_left ( + ) 0) C.fold_left_sum;

    let spec = alist ^> option int in
    declare "C.find_map_first_even"
      spec
      (R.find_map (fun item -> if item mod 2 = 0 then Some item else None))
      C.find_map_first_even;

    let spec = alist ^> bool in
    declare "C.for_all_nonnegative"
      spec
      (R.for_all (fun item -> item >= 0))
      C.for_all_nonnegative
end

let invalid_index_regressions () =
  let empty_r = R.empty in
  let empty_c = C.empty in
  assert (R.nth_opt empty_r (-1) = C.nth_opt empty_c (-1));
  assert (R.nth_opt empty_r 0 = C.nth_opt empty_c 0);
  let seq_r = R.of_list [1; 2; 3] in
  let seq_c = C.of_list [1; 2; 3] in
  assert (R.nth_opt seq_r (-1) = C.nth_opt seq_c (-1));
  assert (R.nth_opt seq_r 3 = C.nth_opt seq_c 3);
  assert (same_exn_kind
    (try ignore (R.split (-1) seq_r); assert false with exn -> exn)
    (try ignore (A.split (-1) seq_c); assert false with exn -> exn));
  assert (raises (fun () -> ignore (R.update_nth 3 succ seq_r)));
  assert (raises (fun () -> ignore (A.update_nth 3 succ seq_c)))

let () =
  invalid_index_regressions ();
  Harness.declare_all ();
  main 12
