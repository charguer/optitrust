void g(int * tab) { tab[0] += 42; }

void f(int * tab1, int * tab2, int size) {
  int a = 2, b = 0;
  
  g(tab1);
  tab1[0] = tab2[0];
  tab1[2] = 2 + b;
  tab2[3] = 4 * tab1[3];
  tab2[4] = 4 * a;
  g(tab1);
  g(tab2);
}
