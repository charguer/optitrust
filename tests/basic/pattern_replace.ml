
(*

void iter_nat_for(int n, void body(int)) {
  for (int i = 0; i < n; i++) {
    body(i);
  }
}

void iter_nat_while(int n, void body(int)) {
  int i = 0;
  while (i < n) {
    body(i);
    i++;
  }
}

int main() {
  int s = 0;
  for (int j = 0; j < n; j++) {
    { // with a nested sequence for the body
      s += 2*j;
      s -= j;
    }
  }
}

Transformation:

  Pattern_basic.replace ~source:[cFunDef "iter_nat_for"] ~target:["cFunDef iter_nat_while"]  [cFor "j"];

performs the following chain of operations:


// after the fold operation (not valid C code):

int main() {
  int s = 0;
  iter_nat_for(n, void body(int j) {
      s += 2*j;
      s -= j;
    });
}

// after function replace

int main() {
  int s = 0;
  iter_nat_while(n, void body(int j) {
      s += 2*j;
      s -= j;
    });
}

// after function inlining
// note: in the inlining step, we have an optional argument [~vars] for performing
// renaming; this argument should be exposed in Pattern_basic.replace, so that
// the user can customize the names.

int main() {
  int s = 0;
  int i = 0;
  while (i < n) {
    (void body(int j) {
      s += 2*j;
      s -= j;
    }))(i);
    i++;
  }
}

// after beta reduction of the body operation (see function_beta.ml for details)

int main() {
  int s = 0;
  int i = 0;
  while (i < n) {
    s += 2*i;
    s -= i;
    i++;
  }
}

//----------------
// At the combi level, the Pattern.replace operation is also able
// As unit test, consider the reciprocal operation

  Pattern.replace ~source:[cFunDef "iter_nat_while"] ~target:["cFunDef iter_nat_for"]  [cVarDef "i"];

// with input :

int main() {
  int x = 0;
  int s = 0;
  int i = 0;
  while (i < n) {
    { s += 2*i;  // let's assume for simplicity this nested sequence is there
      s -= i; }
    i++;
  }
  x++;
}

// and output:

int main() {
  int x = 0;
  int s = 0;
  for (int j = 0; j < n; j++) {
    { s += 2*j;
      s -= j;
    }
  }
  x++;
}

// The idea is that by looking at the body of iter_nat_while, we see 2 instructions,
// thus we can call  Sequence.intro   on the target [cVarDef "i"]  with ~nb:2 and
// ~mark:m, for some fresh mark [m]. This puts the code in the form:

int main() {
  int x = 0;
  int s = 0;
  /*begin mark m:*/
  {
    int i = 0;
    while (i < n) {
      { s += 2*i;  // let's assume for simplicity this nested sequence is there
        s -= i; }
      i++;
    }
  } /*end mark m*/
  x++;
}

then, we can call Pattern_basic.replace [cMark "m"].


*)