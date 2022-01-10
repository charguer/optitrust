int main() {
  int x = 0;
  if (true)
    for (int i = 0; (i < 10); i++) {
      x += 1;
      x += 2;
      x += 5;
    }
  else if (false)
    for (int i = 0; (i < 10); i++) {
      x += 1;
      x += 3;
      x += 5;
    }
  else
    for (int i = 0; (i < 10); i++) {
      x += 1;
      x += 4;
      x += 5;
    }
  return 0;
}
