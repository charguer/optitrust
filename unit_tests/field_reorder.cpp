
// TODO: it would be nice to keep track in the AST of the order of the fields, in the form of a list of names, even though we use (and want to continue) using a map for describing the contents of the fields. This would allow printing fields in the same order as they were specified.

// TODO: this transformation for reordering is yet to implement:
// e.g.       fields_reorder "obj" ~fields:["m", "x", "y", "z"]
// or         fields_reorder "obj" ~move_before:"x" ~fields:["m"]
// or         fields_reorder "obj" ~move_after:"m" ~fields:["x", "y", "z"]
// should be three equivalent transformations.

typedef struct {
  int x;
  int y;
  int z;
  int m;
} obj;

int main() {
  obj a;
}


