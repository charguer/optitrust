(*

//----------------
// UNIT TEST 1


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

     val Pattern_basic.replace ~source:target ~target:target (tg:target);

  example:

     Pattern_basic.replace ~source:[cFunDef "iter_nat_for"] ~target:["cFunDef iter_nat_while"]  [cFor "j"];

performs the following chain of operations:


// after calling the function.uninline operation with arguments source and tg
// --see function_uninline.ml for details
// --note that the result is not valid C code (it will be valid in C23)

int main() {
  int s = 0;
  /*marked trm_app*/ iter_nat_for(n, void body(int j) {
      s += 2*j;
      s -= j;
    });
}

// after function.replace, at the mark of the function name associated with the [source] argument,
// with the function name associated with the [target] argument

int main() {
  int s = 0;
  /*marked trm_app*/ iter_nat_while(n, void body(int j) {
      s += 2*j;
      s -= j;
    });
}

// after function inlining of the function call at the marked node
//
// note: in this inlining step, we have an optional argument [~vars] for performing
// renaming; this argument should be exposed in Pattern_basic.replace, so that
// the user can customize the names.

int main() {
  int s = 0;
  int i = 0;
  /*marked nobrace sequence for the body*/{
    while (i < n) {
      (void body(int j) {
        s += 2*j;
        s -= j;
      }))(i);
      i++;
    }
  }
}

// after function.beta operation
// --the target for the beta reduction operation can be the marked sequence
// --- see function_beta.ml for details

int main() {
  int s = 0;
  int i = 0;
  /*marked nobrace sequence for the body*/{
    while (i < n) {
      s += 2*i;
      s -= i;
      i++;
    }
  }
}

//----------------
// UNIT TEST 2


  Pattern.replace ~source:[cFunDef "iter_nat_while"] ~target:["cFunDef iter_nat_for"] [cVarDef "i"];

// input :

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

// output:

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

*)