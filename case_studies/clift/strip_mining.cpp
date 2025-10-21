t = n * m;

Matrix_tile : x[i]-- > x[i / m][i % m]
X[MINDEX(t,i)] -->
// need n*m = t , il faut trouver la resssource pure n*m =t dans le contexte
Grid_enumerate :
for (int l = 0 ; ..)
_smodifies (for i ...  x );
for (int i = 0; i < n*m; i++) {
  x[i]..
}
--> for (int j = 0; j < n; j++) {
  for (int k = 0; k < m; k++) {
    x[j][k] =
  }
}

Chemin 1 : Grid enum puis matrix tile for (int j = 0; j < n; j++) {
  for (int k = 0; k < m; k++) {
    x[recover_access(n, m, j, k)] =
    // i = j*m +k where 0 <= k < m
  }
  --> for (int j = 0; j < n; j++) {
    for (int k = 0; k < m; k++) {
      x[recover_access(n, m, j, k) / m][recover_access(n, m, j, k) % m] =
      // i = j*m +k where 0 <= k < m
      // Arith simple : recover_access(n, m, j, k) / m --> j ; recover_access(n, m, j, k) % m --> k
    }
  }

Chemin 2 : Matrix tile Grid Enum
for (int i = 0; i < n*m; i++) {
  x[MINDEX(n,m,i/m,i%m)] = ....
}
puis grid
// Transfo combi : Grid_enum + tile
// + clear : mha_q
// avec thomas : mecanisme d'appareillement mais on a pas g_b(A,B) -- g_e(B,A)
ghost(** x[l,i], ***x[...])
for (..)
__...
__sreads(***x[...])
ghost_end( ***x[...], ** x[l,i])

-->
__
for (..)
__s..(**x[l,i])

__ ..
