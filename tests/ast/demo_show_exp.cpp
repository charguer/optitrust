#include "../../include/optitrust.h"

int main() {
  int a, b;
  int x = 3;
  x--;
  for (int i = 0; i < 3; i++) {
    __sequentially_modifies("x ~> Cell");
    /*@mymark2, mymark1*/ x /*mymark2, mymark1@*/++;
  }
}
/*
CAPTURED STDOUT:
AST1: 

  int main (  )  {
  int a, b;
  int x = 3;
  x--;
  for ( int i = 0; i < 3; i++ ) {
    __sequentially_modifies("x ~> Cell");
    x++;
  }
}
AST2: 

  int main (  )  {
  int a, b;
  int x = 3;
  x--;
  for ( int i = 0; i < 3; i++ ) {
    __sequentially_modifies("x ~> Cell");
    x++;
  }
}
for trm: WARNING: trm: unsupported decoding of non root trm, falling back on printing encoded term
for ( int i = 0; i < 3; i++ ) { x++; }
desc: Trm_for
typ: target resolves to 2 paths
[occ #1] int
[occ #2] int
marks: [mymark2; mymark1]
cstyle-item: [ Stackvar ]
annot: {trm_annot_attributes = [  ]; trm_annot_marks = [  ];
  trm_annot_stringrepr = None; trm_annot_pragma = [  ]; trm_annot_cstyle = [

]; trm_annot_files = [ Main_file ]}

*/
