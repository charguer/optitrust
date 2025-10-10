# Formatting of OCaml code

## Indentation

Indentation is 2-space.

The general idea is that if an expression fits on one line, it could go on that line,
but if an expression requires multiple lines, then it should appear as an indented block,
with a start of the block on a separate line.

```ocaml
  let my_variable = short_expr in
  let my_variable =
    long_expr_part_1
    long_expr_part_2 in
  let my_variable = another_short_expr in    
```

The rational is to make it easy to spot the lines on which new definitions begin,
and to make it obvious when starting to read the body of a definition if it is going
to end on the current line or if it is going to span over multiple lines.

## Type definition

```ocaml
(* Algebraic type definition, one constructor per line *)
type t = 
  | A of int
  | B of string (* optional comments on the fields *)

(* Record type definition *)
type r = {
  a : int; (* optional comments on the field *)
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

Particular case of unit arguments:

```ocaml
let f () : int = (* space before unit argument in definitions *)
  body in
f(); (* observe: no space in calls to unit functions *)
```

Spacing for optional and labeled arguments:

```ocaml
let step_backtrack ?(tags : string list = []) ?(discard_after = false) (f : unit -> 'a) : 'a =
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
  let x = short_expr in (* one-liner when possible *)
  let x = (* always a line return if the body does not fit on the current line *)
    long_expr_part_1
    long_expr_part_2 in
  let f (x : int) : int = (* function definitions: always a line return before the body, even it could fit *)
    statement;
    return_value in
```

For calls to higher-order functions, it is hard to find a good general rule.
Sometimes it helps to introduce a line break after the arrow of the local function.

Explicit record definitions:
```ocaml
  let p = { x = 3; y = 4 } in 
  let p = {
    x = 3;
    y = 4; } in
```  
Note that in most situations, the use of a smart-constructor is more flexible
and makes the code easier to maintain when modifying the fields.


## Conditionals 

Conditionals as statements appearing in a sequence:

```ocaml
  t0;
  if cond (* always a line return after 'if' as part of a statement *)
    then expr;
  t1;
  if cond
    then expr
    else expr;
  t2;
  if cond then begin (* use begin-end for non-trivial blocks *)
    long_expr
  end else begin
    long_expr
  end
```

Conditionals appearing as expressions:

```ocaml
  let x = if cond then short_expr else short_expr in
  t1;
  let x =
    if cond 
      then short_expr
      else short_expr
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
  | None, None -> (* line return also accepted here *)
      short_expr 
  | None, Some v ->
      long_expr;
  | Some v, None ->
      long_expr;
      long_expr;
  | Some v, _ -> 
      (* nested match must use begin-end, even when not strictly necessary *)
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

Or-patterns can be placed on the same line for simple patterns,
but are better placed on separated lines for complex patterns:

```ocaml
(* simple or-patterns *)
let _ =
  match x with
  | Step_root | Step_big | Step_small -> true
  | Step_typing | Step_io -> false

(* complex or-patterns *)
let _ =
  match x with
  | None, Some _
  | Some _, None -> true
  | _ -> false
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

Short expression, with a single exception caught:

```ocaml
  t1;
  let x = 
    try short_expr() 
    with Not_found -> a in
  t2;
```

Complex expression, with multiple exceptions caught:

```ocaml
  t1;
  let x = 
    try 
      long_expr;
      long_expr;
      retur_value
    with 
    | Err1 -> 
        a
    | Err2 -> 
        long_expr;
        b 
    in
  t2;
```

Try-block in the middle of a sequence must use begin-end:

```ocaml
  t1;
  begin 
    try
      f() 
    with
    | Err1 -> a
  end;
  t2
```


