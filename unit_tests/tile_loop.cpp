
int N;
int* t;

int BLOCK; // assumes BLOCK DIVIDES N

int main() {
  for (int i = 0; i < N; i++) {
    int i1 = i / BLOCK;
    int i2 = i % BLOCK;
    t[i] = i1 + i2;
  }
}

/*
NOTE:

At some point it would be nice to implement the transformation that
introduces the variables i1 and i2.
This transformation could reuse some of this example code:

      set_init_source "test_array_tiling/test_array_tiling.c";
      tile_array ~block_size:"2" "T";
      tile_array ~name:(fun x -> x ^ "_Stiled") ~block_size:"2" "S";
      let insert_before = [cSet ~rhs:[cStr "(2 * i)"] ()] in
      insert_and_fold ~insert_before ~name:"i1" ~value:"(i / 2)" ();
      insert_and_fold ~insert_before ~name:"i2" ~value:"(i % 2)" ();
      tile_loop [cFor ()];

*/