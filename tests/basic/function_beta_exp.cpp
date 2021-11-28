int main() {
  int i = 1;
  /*@body*/ {
    int s = 0;
    s += (2 * i);
    s -= i;
  } /*body@*/
  return 0;
}
