On suppose une paires de ghosts g, g'

Il faut savoir les gérer sur les fissions, move_out et hoist

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


