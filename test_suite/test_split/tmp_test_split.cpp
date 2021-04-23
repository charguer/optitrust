int main() {
  int t[10] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
  {
    int x_split[10];
    int z_split[10];
    {
      for (int i = 0; (i < 10); i++) {
        int x = (i % 2);
        int y = (i / 2);
        int z = (i * 3);
        t[i] += (((i * (1 - x)) + y) - z);
        x = (1 - x);
        x_split[i] = x;
        z_split[i] = z;
      }
      for (int i = 0; (i < 10); i++) {
        int x = x_split[i];
        int z = z_split[i];
        t[(9 - i)] += ((i * (1 - x)) - z);
      }
    }
  }
return_instr:
  return 0;
}
