# Formatting of OCaml code

## Indentation

Indentation is 2-space.

## Type definition

```ocaml
(* Algebraic type definition, one constructor per line *)
type t = 
  | A of int
  | B of string (* optional comments on the fields *)

(* Record type definition *)
type r = 
  { a : int; (* optional comments on the field *)
    b : string; }

(* Alias type *)
type p = t * t -> unit
```

## Type annotation on function definitions

Both for top-level and local functions:

```ocaml
(* Recommended style *)
let f (x : int) (y : int) : unit = 
  t
(* tolerated legacy spacing found in various places: *)
let f (x:int) (y:int) : unit = 
  t
```

Always place a line break after the equal sign for function definitions,
it helps spot that it is a function, and makes it quick to spot the return type.

Exceptions to typing arguments:

- if the name of the argument is the same as the name of the type,
- if the argument has a complex function type, which can be easily seen on the call site,
- if a local function is named `aux` corresponds to an auxiliary recursive implementation
  of the surrounding top-level function, in this case only arguments that are not already
  present in the top-level function needs to be typed. Except the argument `acc`, which
  by convention should have the same type as the return type.

Example recursive function:

```ocaml
let f (n : int) (x : 'a list) : 'a list =
  let rec aux acc n x =
    ...
    in
  aux 0 n x
```

## Let-bindings 

Indentation:

```ocaml
  let x = short_expr in
  let x =
    long_expr in
  let x = 
    long_expr;
    long_expr in
  let f (x : int) : unit = (* always a line return for functions *)
    statement;
    statement;
    in (* recommended: 'in' alone on its line after a unit-statement *)
  let f (x : int) : int = (* always a line return for functions *)
    statement;
    return_value in (* 'in' on the same line as the return value *)
  let f () : int = (* space before unit argument in definitions *)
    body in
  f(); (* observe: no space in calls to unit functions *)
```

For calls to higher-order functions, it is hard to find a good general rule.
Sometimes it helps to introduce a line break after the arrow of the local function.

## Conditionals 

Conditionals in sequence:

```ocaml
  if cond then short_expr;
  t0;
  if cond 
    then mid_expr;
  t1;
  if cond
    then mid_expr
    else mid_expr;
  t2;
```

Let-binding with a conditional:

```ocaml
  let x = if cond then short_expr else short_expr in
  t1;
  let x =
    if cond 
      then mid_expr
      else mid_expr
    in
  t2;
  let x =
    if cond then begin
      long_expr
    end else begin
      long_expr
    end in
  t3;
```

## Pattern matching 

Pattern-matching in sequence:

```ocaml
  match p with
  | None, None -> short_expr
  | None, Some v ->
      long_expr;
  | Some v, None ->
      long_expr;
      long_expr;
  | Some v, _ -> 
      (* nested match must use begin-end, even if last branch *)
      begin match v with
      | Some 0 -> ..
      | None -> ..
      end
  
```

Let-binding with a pattern-matching:

```ocaml
  let x = match v with None -> short_expr | Some v -> short_expr in
  t1;
  let x =
    match v with
    | p1 -> t1
    | p2 -> t2
    in
```

Use conditionals where possible:

```ocaml
  (* BAD *)
  let x = match b with true -> .. | false -> .. in
  let x = match b with None -> .. | Some _ -> .. in
  let x = match b with [] -> .. | _ -> .. in
  let x = match n with 0 -> .. | _ -> .. in
  (* GOOD *)
  let x = if b then .. else .. in
  let x = if b = None then .. else .. in
  let x = if b <> None then .. else .. in
  let x = if b = [] then .. else .. in
  let x = if b <> [] then .. else .. in
  let x = if n = 0 then .. else .. in
  let x = if n <> 0 then .. else .. in
```

## Try-blocks

```ocaml
  let x = try f() with Not_found -> a in (* if short expr, and only one exception *)
  t1;
  let x = 
    try 
      f() 
    with 
    | Err1 -> a
    | Err2 -> 
        long_expr;
        b 
    in
  t2;
  begin try (* in sequences, always wrap in begin-end *)
    f() 
  with
    | Err1 -> a
  end
```
