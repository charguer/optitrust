int *t;
int *u;

int main() {
  for (int i = 0; i < 10; i++) {
    int x;
    x = t[i];
    u[i] = x;
  }

  // TODO: this "detach" on the fly could be handled in the "combi" folder
  for (int j = 0; j < 10; j++) {
    int y ;
    y = t[j];
    u[j] = y + 1;
    y = u[j];
  }
}

// detach_split :
// takes the term  "int y = t[j]"
// returns the list of terms [ "int y"; "y = t[j]" ].
//  here call "list_update "index of int y" with "y = t[j]"
/*
  for (int j = 0; j < 10; j++) {
    y = t[j];
    u[j] = y + 1;
    y = u[j];
  }
  then call replace_trm
*/

/*

more complex example -> hoist x in

  int total = 0;
  for (int i = 0; i < 10; i++) {
    int a = i + 1;
    int x = a + 1;
    int y = x + 1;
    total += y;
  }


hoist v in

  int total = 0;
  for (int i = 0; i < 10; i++) {
    int a = i + 1;
    vect v = { a, a };
    int y = v.x + 1;
    total += y;
  }



*/