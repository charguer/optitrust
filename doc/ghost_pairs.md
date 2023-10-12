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
# Ghost scoped

ghost_begin("fend", f, "...");
...
ghost_end("fend");


=>

explaination of ghost_begin:

   ghost(f, "..."); __bind("fend := reverse");



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
# Splitting scoped ghost

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
# Fission

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
# Swap

Transfo "scoped_ghost_hoist":

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
by applygin

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



