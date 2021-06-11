int f(int n) {
  return n;
}


int main() {
  {
    int r1 = 1;
    int r2 = 2;
    int r3 = r2;
  }
  // calls
  int y = f(2);

}