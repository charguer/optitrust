int main() {
  int t;
  int f;

  if (true) {
    t += 1;
  }

  if (false) {
    f -= 1;
  }

  if (1 > 0) {
    t += 1;
  } else {
    t -= 1;
  }

  if (1 < 0) {
    f += 1;
  } else {
    t -= 1;
  }

  // TODO: if (false) {}

  return 0;
}
