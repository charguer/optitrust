On suppose une paires de ghosts g, g'

Il faut savoir les gérer sur les fissions, move_out, hoist, et swap

# fission

for i {
  t1;
  t2
}

for i {
  t1
}
for i {
  t2
}

si on a
for i {
  with g {
    t1;
    t2
  }
}

for i {
  with g { t1 };
  with g { t2 }
}

Intérêt de l'approche:
- On sait bien minimiser, en enlevant des with g


autre chemin
for i {
  g;
  t1;
  t2;
  g'
}

for i {
  g;
  t1
}
for i {
  t2;
  g'
}

# move_out

Critère A
pfor i {
  t1;
  t2
}

t1;
for i { t2 }

A chaque tour de boucle t1 redondant

Critière B
for i { // pas un pfor
  wt1;
  rt2
}

wt1;
for i { rt2 }

wt1 est WO

On pourrait A ou B en fonction de la reosurces


move_out se décompose en fission + élim boucle redondante

for i { with g { t1 } }
=>
with g { t1 }

# hoist

for i {
  with g {
    M = malloc(n);
    t
  }
}

M = malloc(n*m);
for i {
  M = &M[i];
  with g { t }
}

# swap

pfor i { for j { t } } --> for j { pfor i { t } }

pfor i { with g { for j { t } } } --> with { for j { g } } { for j { pfor i { t } } }

avec with de code arbitraire:
with F F' --> with { for j F } { for j F' }

# Enlever un with

with g { t1 } = t1
si t1 n'utilise rien de ce qui est produit par g

# Distribuer les with
with g1 { with g2 { t1; t2 } }
with g1 { with g2 { t1 }; with g2 { t2 } }
with g1 { with g2 { t1 }}; with

En C:
__SCOPE_GHOST(g) {

}

{
  __scope_ghost(g1, "ghost_args");
  __scope_ghost(g2, "ghost_args");

  ...
}
__scope_ghost(g, "ghost_args", [&]{ ... })

# Opérations sur les séquences

Une des deux solutions:
- Trm_seq contient une liste de ghost
- Ajouter Trm_scope_ghost


# Minimisation de boucle

enlève les morceaux de reads et modifies jamais utilisés
affaibli les modifies


=======================================================
# Bind

int f(int x)
  requires (a : int)
  ensures (b : int)


  f(1); __with("a := 2"); __bind("b as g");  // C syntax
  trm_call(f, [1], [("a", 2)])
  trm_call(f, [1], [("a", 2)], [("b", "g")]);
  trm_let(None, [("b", "g")],  trm_call(f, [1], [("a", 2)])) -> all nodes are let in a sequence

    val f : ~a:int -> ~b:int

  int a = f(1); -> target all:  cLet "a"  or  cLet ""; cCall "f"
  int a = f(1); -> target f:    cCall "f"



  int a = f(1); __with("b := 2"); __bind("b as g");  // C syntax


  int a = 3;  -> trm_let ("a", 3)
  3;   -> trm_let ("", 3) -> invisible let


for i

  f(1); -> target   cFor "i"; cStrict; cCall "f"
  let a = f(4)   -> cFor "i; cLet "a"; cCall "f"
  f(1); __with("a := 2"); __bind("b as g");  // C syntax


  let _ = f(1);
  let _ = f(2);

  let x = f(1)



  ghost(foo(g));


Design space:

  trm_ghost(trm_call(f, [a])) -> syntactic annotation
  trm_call_ghost(f, [a]) -> strictly less interesting than above
  trm_call(prim_ghost, trm_call(f, "x:=a"))) -> interest to avoid introducing a new trm node "trm_ghost"
  trm_call(f, [a])) -> leverages typing of f (its return type) to know if it is a ghost operation or not

Current plan:
    let tfghost be a shorthand for [trm_var "fghost"]
    let tfcode be a shorthand for [trm_var "fcode"]
  Let^ghost (None, [("g","b")], trm_call(tfghost, [], [("a", 1)]) --> ghost(fghost, "a:=1");__bind("b as g");
  Let^ghost (None, [], trm_call(tfghost, [], [("a", 1)]) --> ghost(fghost, "a:=1");
  Let^real  ("x", [("g","b")], trm_call(tfcode, [3], [("a",1)]))  --> let x = f(3); __with("a:=1"); __bind("b as g");
  Let^real  ("x", [], trm_call(tfcode, [3], []))                  --> let x = f(3)
  Let^real  (None, [], trm_call(tfcode, [3], []))                 --> f(3)

  Let^real (x, gargs, body) = Trm_let (Let_real, x, gargs, body)
  Let^ghost (x, gargs, body) = Trm_let (Let_ghost, x, gargs, body)
  // LATER: see if recflag ghost there too or not.
  // TEMPORARILY: can use annotations on let-terms to tag them as "^ghost".
  // TEMPORARILY: can use nesting in trm_call(prim_begin, ..)




  // Ghost variables bound by Let (ghost or real) scope within ghost_args of trm_calls but also in Let^ghost:>trm_call.


  // long term solution:

  GHOST_BEGIN(fend, fghost, "a:=1") --> Let^ghost (None, [("reverse","fend")], trm_call(fghost, [], [("a", 1)])
  GHOST_END(fend) --> Let^ghost (None, [], trm_call(fend, [], []))

  // short term solution
  // Currently: map a C/ghost function names "f"  to its specs "Sf"; interpreted in coq as [f_spec : Spec f Sf]

  GHOST_BEGIN(fend, fghost, "a:=1") --> Let^ghostbegin ("fend", [], trm_call(fghost, [], [("a", 1)])

    // with custom typing rule saying that fend is added to the specification map, with the reverse entailment.
  GHOST_END(fend) --> Let^ghostend (None, [], trm_call(trm_var "fend", [], []))

  // current implementation:

  GHOST_BEGIN(fend, fghost, "a:=1") --> Let ("fend", [], trm_call(trm_var "GHOST_BEGIN", trm_call(fghost, [], [("a", 1)]))@annot_ghost

    // with custom typing rule saying that fend is added to the specification map, with the reverse entailment.
  GHOST_END(fend) --> trm_call(trm_var "GHOST_END", trm_call(trm_var "fend", [], []))@annot_ghost

  __ghost(f, "a:=1"); --> trm_call(trm_var "f", [("a",1)])@annot_ghost

  ::: function is_trm_ghost


  // Choice 1: all ghost variables live in the same namespace as C variables
  // Choice 2: they live in different namespaces
  // Choice 3: ghost function names live in C namespace, the rest does not

  // Long term solution : all ghosts live in separate namespace (i.e. choice 2)

  GHOST(f) consumes H1 produces H2
  -->
  void __GHOSTDEF_f() { consumes H1; produces H2 }

  __ghost("f", "x:=a");  // variante 1
  __ghost("f(x:=a)");    // variante 2 -> slightly better
  __ghost("f(x:=a); bind g as b");
  __ghost("let { b := g } = f { x := a }"); // variante 3
  // take into account that we don't want to name all args or all ret values explicitly, only a subset.

  // forall a, H1 |- exists g, H2  --> to respect reading order:  __ghost("myconv(a); bind g as b")
  // or with ocaml syntax: __ghost("let { g := b } = myconv(a)")  __ghost("let { g } = myconv(a)")
  // H1 |- H1 \* (x <> null)  --> to respect order:  __ghost("let Hx = extract_nonnull(H1)")


=======================================================
# Ghost scoped

  ghost(f, "...") // general syntax


approach1

  ghost_begin("fend", f, "...");
  ...
  ghost_end("fend");

explaination of ghost_begin:

   ghost(f, "..."); __bind("fend := reverse");
   ghost(fend) // if is a name
   // encoded as  Trm_apps(Trm_apps(Trm_prim_ghost_call, "fend"), [])
   // could have challenges with variable checks?


approach 2: (==> try this first)

  GHOST_BEGIN(fend, f, "x := a")
  => // is macro for
  ghost_scope fend = ghost_begin(f, "x := a");

  // resource typing adds a spec for fend in the ghost_spec_table
  ...
  ghost_end(fend); // interpreted like ghost(fend) // currently represented as [fend()]

  // make sure that we have a proper id for fend




LATER: provide operation is_trm_ghost


alternative presentation: (with explicit args for reverse)

  ghost_begin("fend", f, "...");
  ...
  ghost_end("fend", f_reverse, "...");


alternative presentation:

  ghost_begin(f, "x, y");
  ...
  ghost_end(f_reverse, "y, x");


strategy for geenrated names:

  ghost_begin("f_scope1", f, "...");
  ...
  ghost_end("f_scope1");






where

GHOST_REVERSIBLE(f) {
  requires x
  consumes H1
  produces H2
}


means

GHOST(f) {
  requires x
  consumes H1
  produces H2
  ensures (reverse : ghost
    consumes H2
    produces H1)
}

GHOST(f_fwd) {
  requires x
  consumes H1
  produces H2
}

GHOST(f_back) {
  requires x
  consumes H1
  produces H2
}


// idea: ghost(rev(f))
// idea: ghost_rev(f)
// idea: GHOST_REVERSIBLE builds a class/struct with 3 functions
//    ghost(f.back), ghost(f), ghost(f.fwd) ==> looks good


ensures (
  fend : ghost_val;
  fend_spec :
    contract fend()
      consumes H2
      produces H1)
  )


TODO: do we want to bind
TODO: what to do with pure facts
TODO: what to do with justifications


GHOST_REVERSIBLE(f) {
  requires x
  consumes H1
  produces H2
}
should it generate also:
  GHOST(f_reverse) {
    requires x
    consumes H2
    produces H1
  }

Does it have a use to keep track of:

GHOST(f) {
  requires x
  consumes H1
  produces H2
  ensures (reverse : typeof f_reverse(x))
}

=======================================================
# Splitting scoped ghost => TO implement

{
  ghost_begin(T)
  ghost_begin(U)
  t1
  t2
  //cut point
  t3
  t4
  ghost_end(U)
  ghost_end(T)
}
=>

{
  ghost_begin(T)
  ghost_begin(U)
  t1
  t2
  ghost_end(U)
  ghost_end(T)
  ghost_begin(T)
  ghost_begin(U)
  t3
  t4
  ghost_end(U)
  ghost_end(T)
}

Algo: add to stack T and U,
when reaching cut point, producethe middle ghost ops,
and attempt to typecheck resources
(it could fail if resources produced by the ghost ops have changed)


=======================================================
# Minimize

3 choices for implementing cancel: => TO implement
  => cancel
  => move down
  => swap

  t0
  ghost_begin(T) <- target this
  t1
  t2
  ghost_end(T)
  t3

Are resources from T used in t1,t2?

=> union of t1,t2 uses disjoint from resources produced by T

// see loop_basic.fission



Operation take a ghost in a sequence, and move it either up or down as far as possible;
with an option to cancel it out if we reach the inverse, with two modes for cancel:
  - either cancel only pairs with similar token
  - or also cancel pairs with matching produces/consumes

  t1
  t2
  ghost(f)
  t3
  t4

-> move up

  t1 // instruction that produces a resource consumed by ghost(f)
  ghost(f)
  t2 // does not interfer
  t3
  t4


For a scoped ghost, we call "move_down" on "ghost_begin",
and "move_up" on "ghost_end",
(with option to cancel begin/end on same token activated)

  t1
  ghost_begin(T)
  t2 // not using resources produced by T
  t3 // not using resources produced by T
  ghost_end(T)
  t4
->
  t1
  t2
  t3
  t4







=======================================================
# Fission => unit test

Prelude of fission:
(1) attempt to split scoped ghost and minimize them,
(2) apply basic fission


for i
  ghost_begin(T)
  ghost_begin(U)
  t1
  t2
  // cut point
  t3
  t4
  ghost_end(U)
  ghost_end(T)

-> split scoped ghost

for i
  ghost_begin(T)
  ghost_begin(U)
  t1
  t2
  ghost_end(U)
  ghost_end(T)
  ghost_begin(T)
  ghost_begin(U)
  t3
  t4
  ghost_end(U)
  ghost_end(T)

-> fission loop

for i
  ghost_begin(T)
  ghost_begin(U)
  t1
  t2
  ghost_end(U)
  ghost_end(T)

for i
  ghost_begin(T)
  ghost_begin(U)
  t3
  t4
  ghost_end(U)
  ghost_end(T)

-> minimize scoped ghost

for i
  ghost_begin(T)
  t1
  t2
  ghost_end(T)

for i
  ghost_begin(U)
  t3
  t4
  ghost_end(U)

-> then minimize loop contract

NOTE: minimize loop contract should attempt to
 minimize scoped ghost systematically




=======================================================
# Descope ghost // NOT NEEDED YET

  ghost_begin(T, "..")
  t1
  t2
  ghost_end(T)

->

  ghost(T, "..")
  t1
  t2
  ghost(T_reverse, "..") // with the same arguments



=======================================================
# Swap

Transfo "scoped_ghost_hoist": // TO IMPLEMENT

pfor i
  ghost_begin(T, H1i, H2i)
  for j
    tij
  ghost_end(T)

->

ghost_begin(T', stars_i H1i, stars_i, H2i) {
  // justified by
  pfor i
    ghost_begin(T)
  }
pfor i
  for j
    tij
ghost_end(T')


Remark: check in the unit test that it works on nested ops
by applying

pfor i
  ghost_begin(T)
  ghost_begin(U)
  for j
    tij
  ghost_end(U)
  ghost_end(T)

Remark: we want to avoid for scoped-ghost to generate

pfor i
  ghost_begin(T)
pfor i
  for j
    tij
pfor i
  ghost_end(T)

We can nevertheless use this scheme for non-well-scoped ghost





=======
# contract for higher order functions

void f(takes function g as arg) : return function h {
  requires gspec :
    contract g(int x)
       consumes ..
       produces ..

  ensures resspec :
    contract res(int y)
       consumes ..
       produces ..



}





=========================
# Typing read variables

  how to type
  // t ~>RO Array
  y = t[i] + t[j]

  needs two focus
  // t[i] ~>RO Cell,  t[j] ~>RO Cell ,   +reverse-wand
  y = t[i] + t[j]

  presentation with on-the-fly focus
  y = with(array_focus, t[i]) + with(array_focus, t[j])


  alternative: ability to read with larger permissions
  t ->RO Array
  t[i]









