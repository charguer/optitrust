int main() {
  int* const x_step = (int*)MALLOC1(4, sizeof(int));
  for (int i = 0; i < 4; i++) {
    int* const x = &x_step[MINDEX1(4, i)];
    x[MINDEX0()] = 2 * i;
  }
  MFREE1(4, x_step);
}
