#include "../../include/optitrust.h"

int main() {
  int a, b;
  int x = 3;
  x--;
  for (int i = 0; i < 3; i++) {
    __strict();
    __smodifies("x ~> Cell");
    /*@mymark2, mymark1*/ x /*mymark2, mymark1@*/++;
  }
}
/*
CAPTURED STDOUT:
AST:

  int main ()  {
  int a, b;
  int x = 3;
  x--;
  for (int i = 0; i < 3; i++) {
    __strict();
    __smodifies("x ~> Cell");
    x++;
  }
}
for-trm-internal-desc: Trm_for (i,
  Trm_lit (Lit_int 0),
  Up,
  Trm_lit (Lit_int 3),
  Trm_lit (Lit_int 1),
  Trm_seq [
    Trm_apps (Trm_prim (Prim_unop Unop_post_inc), [ Trm_var(x) ])
  ])
desc: Trm_for
typ: target resolves to 3 paths
[occ #1] int
[occ #2] int
[occ #3] <no_typ>
marks: [mymark2; mymark1]
cstyle-item: annot: { trm_annot_file = Main_file; }

*/
