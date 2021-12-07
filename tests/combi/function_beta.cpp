void f(int j) {
  int s = 0;
  s += 2*j;
  s -= j;

}


void g(int j) {
  int s1 = 0;
  s1 += 2*j;
  s1 -= j;

}

int main () {
  int i = 1;
  f(i);
  g(i);
  return 0;
}