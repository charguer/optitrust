#define X 10
#define Y  5

void addtab1(int * tab, int i, int v) { tab[i] += v; }

int gettab1(int * tab, int i) { return tab[i]; }

void addtab2(int ** tab, int i, int j, int v) { tab[i][j] += v; }

int gettab2(int ** tab, int i, int j) { return tab[i][j]; }

void dyntab1_simple(int * tab, int size) {
  int end = size - 1;
  addtab1(tab, *tab, 42);
  
  tab[0] += 2;
  tab[1] = tab[0] * 2;
  
  tab[2] = X;
  addtab1(tab, 3, Y);
  tab[end - 2] = tab[0]++ + 3;
  
  int tmp = gettab1(tab, 2);
  addtab1(tab, 1, tmp);
}

void statab1_simple(int tab[X]) {
  tab[0] += 2;
  tab[1] = tab[0] * 2;
  
  tab[2] = X;
  tab[4] = tab[0]++ + 3;
  
  int tmp = tab[2];
  tab[1] = tmp;
}