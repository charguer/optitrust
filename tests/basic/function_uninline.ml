(*
----------------
UNIT TEST 1

Function_basic.uninline ~fct:[cFunDef "f"] [cLabel "test"]

// before:

void g(int x, int y) {}

void f(int x) {
  int a = x+1;
  g(a, x);
}

int main() {
  int r = 5;
  test:{
    int b = (r+2)+1;
    g(b, r+2);
  }
}

// after:

void g(int x, int y) {}

void f(int x) {
  int a = x+1;
  g(a, x);
}

int main() {
  int r = 5;
  test: {
    f(r+2);
  }
}

----------------
Implementation:

  essentially, we are calling [rule_match], with pattern the body of the function f:

    int a = x+1;
    g(a, x);

  and with term the contents of the targeted sequence:

    int b = (r+2)+1;
    g(b, r+2);

  and with the list of pattern variables ["x"], corresponding to the arguments of f.

  the list of pattern variables gets increased when we have a trm_let;
  for example, here we have "int a = ..." to match against "int b = ...",
  so we need to add "a" as additional pattern variables, and recall that it maps to "b";
  this way, we can later match occurences of "a" in the pattern with occurences of "b"
  in the processed term.

  At the end of rule_match, we have the following instantiation:
    x -> r+2
  // note that the binding (a -> b) is no longer visible when we exit the scope of the trm_seq.

  then, we look at the list of arguments of "f", namely the list ["x"],
  and we generate the function call [f(r+2)], because x is bound to [r+2].

  For all this to work, the current implementation of rule_match needs to be augmented
  with cases for trm_seq, trm_for, trm_let, etc.


----------------
UNIT TEST 2

Function_basic.uninline ~fct:[cFunDef "iter_nat_for"] [cFor "j"]

// before:

void iter_nat_for(int n, void body(int)) {
  for (int i = 0; i < n; i++) {
    body(i);
  }
}

int main() {
  int s = 0;
  for (int j = 0; j < n; j++) {
    { // with a nested sequence for the body
      s += 2*j;
      s -= j;
    }
  }
}

// after: // note that this is not valid C syntax

int main() {
  int s = 0;
  iter_nat_for(n, void body(int j) {
      s += 2*j;
      s -= j;
    });
}

----------------
Implementation:

  To support the example above, we need one more feature: the resolution of the
  matching of "body(i)" against the sequence {s += 2*j; s -= j;}, where body
  is a pattern variable, and its arguments are (by hypothesis) a list of variables.

  We have a flag in rule_match to indicate that we are willing to instantiate
  a variable such as "body" with a function that we synthesis on the fly.

  This synthesized function is a trm_let_fun, with name "body",
  with arguments ["j"], and with body {s += 2*j; s -= j;}.

  The argument is ["j"] because we are matching "body(i)" in a context where "i"
  is bound to "j" (from the time we entered the scope of the for loop and matched
  "for i" against "for j").


----------------
Pseudo code for rule_match

let rule_match (higher_order_inst : bool) (vars : typed_vars) (pat : trm) (t : trm) : tmap =

  (* [inst] maps each pattern variable to a term and to a type;
     when pattern variables are not yet instantiated,
     they are bound to the special term trm_uninitialized. *)
     (* LATER: we may need one day to introduce another special term Trm_uninstantiated  *)
  let inst = ref (List.fold_left (fun acc (x,ty) -> Trm_map.add x (ty, trm_uninitialized) acc) Trm_map.empty vars) in
  let check (b : bool) : unit =
    if not b then raise Rule_mismatch
    in
  let is_var (x : var) : bool =
    Trm_map.mem x !inst in
  let find_var (x : var) (u : trm) : unit =
    match Trm_map.find_opt x !inst with
    | None -> failwith "failure in rule_match: called find_var without first checking is_var"
    | Some (ty,t0) ->
        if is_trm_uninitialized t0
          then inst := Trm_map.add x (ty,u) !inst
          else check (Internal.same_trm t0 u)
    in
  let get_binding (x : var) : (typ * var) option =
    match Trm_map.find_opt x !inst with
    | None -> None
    | Some (ty,t0) -> match t0.desc with
       | Trm_var y -> Some (ty,y)
       | _ -> None
    in
  let with_binding (ty : typ) (x : var) (y : var) (f : unit -> unit) : unit =
     inst := Trm_map.add x (ty, trm_var y) !inst;
     f();
     inst := Trm_map.rem x !inst;
    (* Note: it would be incorrect to simply restore the map to its value before the call to [f],
       because other variables than [x] may have been instantiated during the call to [f]. *)
     in

  let rec aux (t1 : trm) (t2 : trm) : unit =
    let aux_list (ts1 : trms) (ts2 : trms) : unit =
      List.iter2 aux ts1 ts2 in
    (* [aux_with_bindings] is a function for matching two lists of terms,
       making sure to extend the [inst] map to take into account the
       variable names that are bound locally; for example,
         [int a = 3; return a]  should match  [int b = 3; return b]
      thus we need to introduce a binding from [a] to [b] using [with_binding]. *)
    let rec aux_with_bindings (ts1 : trms) (ts2 : trms) : unit =
      match ts1, ts2 with
      | [], [] -> ()
      | Trm_let (vk1, (x1,t1), init1) :: tr1,
        Trm_let (vk2, (x2,t2), init2) :: tr2 ->
          check (vk1 = vk2 && same_type t1 t2);
          aux init1 init2;
          with_binding x1 x2 (fun () -> aux_with_bindings tr1 tr2)
      (* LATER: add support for Trm_let_fun, to allow matching local function definitions. *)
      | t1 :: tr1, t2 :: tr2 ->
          aux t1 t2;
          aux_with_bindings tr1 tr2
      in

    match t1.desc, t2.desc with

    (* Case for treating a match against a pattern variable *)
    | Trm_var x, _ when is_var x -> find_var x t2

    (* Case for treating a match against a pattern such as [body(i)],
       where [body] is a pattern variable that corresponds to a function. *)
    | Trm_apps ({ desc = Trm_var x}, ts1), _ when higher_order_inst && is_var x ->
        let msg i = fail None (sprintf "rule_match: the %d-th argument of the higher-order function variable %s is not a local variable" i x) in
        let xargs = List.mapi (fun i t -> match t.desc with Trm_var x -> x | _ -> msg i) ts1 in
        let targs = List.mapi (fun i xi -> match get_binding xi with Some typed_yi -> typed_yi | None -> msg i) xargs in
        let body = t2 in
        trm_let_fun x typ_unit targs body
        (* LATER: it would be equivalent, but slightly nicer, to use the types coming from the function type associated with x,
           rather that to take the local types associated with the variables provided as arguments to x. *)

    | Trm_var x1, Trm_var x2 when x1 = x2 -> ()

    | Trm_val v1, Trm_val v2 when Internal.same_val v1 v2 -> ()

    | Trm_for (index1, dir1, start1, stop1, step1, body1),
      Trm_for (index2, dir2, start2, stop2, step2, body2) when dir1 = dir2 ->
        aux start1 start2;
        aux stop1 stop2;
        aux step1 step2;
        with_binding index1 index2 (fun () -> aux body1 body2)

    | Trm_for_c (init1, cond1, step1, body1), Trm_for_c (init2, cond2, step2, body2) ->
        aux_with_bindings [init1; cond1; step1; body1] [init2; cond2; step2; body2]

    | Trm_seq tl1, Trm_seq tl2 ->
        aux_with_bindings (MList.to_list tl1) (MList.to_list tl2)

    | Trm_apps (f1, ts1), Trm_apps (f2, ts2) ->
        aux f1 f2;
        aux_list ts1 ts2;

    | _ ->
      Tools.printf "Comparing %s with %s" (Ast_to_c.ast_to_string t1) (Ast_to_c.ast_to_string t2);
      (* Tools.printf "Comparing %s with %s" (Ast_to_text.ast_to_string t1) (Ast_to_text.ast_to_string t2); *)
      raise Rule_mismatch
  in
  aux pat t;
  !inst




----------------
// Note in passing (for LATER)
// here we may want to call  Function.bind_arg ["","?"] [cFun "iter_nat_for"]
//   where the "?" means that we should guess a good name to use for the
//   fresh variable; in case the argument is a trm_let_fun, we take the
//   existing name from that function. we get:

int main() {
  int s = 0;
  void body(int j) {
      s += 2*j;
      s -= j;
    });
  iter_nat_for(n, body);
}


----------------
At the combi level, the uninline operation should be able to automatically
perform a Sequence_intro, when given the target of the first instruction
of the body of the function

----------------
COMBI UNIT TEST 1

  Function.uninline ~fct:[cVarDef "f"] [cVarDef "b"]

// before:

void g(int x, int y) {}

void f(int x) {
  int a = x+1;
  g(a, x);
}

int main() {
  int r = 5;
  int b = (r+2)+1;
  g(b, r+2);
  int s = r;
}

// after:

void g(int x, int y) {}

void f(int x) {
  int a = x+1;
  g(a, x);
}

int main() {
  int r = 5;
  f(r+2);
  int s = r;
}

// The idea is that by looking at the body of the function [f], we see that we
// need 2 instructions, thus we can call  Sequence.intro   on the target [cVarDef "b"]
// with ~nb:2 and ~mark:m, for some fresh mark [m].
// Once this is done, we can call  Function_basic.uninline with the same function
// and with the target [cMark "m"].
// After this call, we can eliminate the sequence introduced.


*)