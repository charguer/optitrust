int g(int x) {
  if (x > 0)
    return 1;
  else
    return 2;
}

int main() {
  int x = 3;
  int y = 10;
  const int z = g(x);
  int s = y + z;
  return 0;
}
