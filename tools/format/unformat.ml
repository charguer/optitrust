(* Algebraic type definition, one constructor per line *)
type t = | A of int | B of string (* optional comments on the fields *)

(* Record type definition *)
type r = {
  a : int; (* optional comments on the field *)
  b : string;
}

(* Alias type *)
type p = t * t -> unit

(* [IF THEN ELSE] *)
let if_short_expr x =
  if x > 0 then Printf.printf "test";
  ()

let if_mid_expr x =
  if x > 0 then Printf.printf "a mid expr test %d " x;
  x

let if_mid_expr x =
  if x > 0 then
    Printf.printf "a mid expr test %d " x;
  x

let if_else_mid_expr x =
  if x > 0 then
    Printf.printf "a mid expr test %d " x
  else
    Printf.printf "a mid expr %d " x;
  x

let let_binding_if_small_expr x =
  let _x = if x > 0 then 12 else 10 in
  ()

let let_binding_if_long_expr x =
  let _x =
    if x > 0 then (
      Printf.printf "test";
      Printf.printf "test";
      Printf.printf "test";
      12
    ) else (
      Printf.printf "test";
      Printf.printf "test";
      Printf.printf "test";
      10
    ) in
  _x

(* [END IF THEN ELSE] *)
(* [PATTERN MATCHING] *)
let _pattern p =
  match p with
  | None, None -> ()
  | None, Some v ->
    Printf.printf "test pattern_matching with a long expr so it doesnt fit one line, must be more"
  | Some v, None ->
    Printf.printf "test pattern_matching";
    Printf.printf "nothing more to test"
  | Some v, _ -> (
    (* Issue with the begin end here is it ok if it's parentheses ?  *)
    match v with
    | Some x -> ()
    | None -> ()
  )

let let_binding_pattern v =
  let _x = match v with None -> () | Some v -> () in
  Printf.printf "x";
  let _x =
    match v with
    | None -> Printf.printf "longer matching"
    | Some v -> Printf.printf "longer matching" in
  ()

(* END PATTERN *)
(* TRY BLOCK *)

let f () = ()

let test_try a t1 t2 =
  let _x = try f () with Not_found -> a in
  (* if short expr, and only one exception *)
  t1;
  let _x =
    try f () with
    | Invalid_argument x -> a
    | Not_found ->
      Printf.printf " a very long expr";
      a in
  t2;
  begin
    try
      (* in sequences, always wrap in begin-end *)
      f ()
    with Invalid_argument x -> a
  end
