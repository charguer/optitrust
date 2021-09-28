int *t;

int *u;

int main() {
  int x_step[10];
  for (int i = 0; (i < 10); i++) {
    int &x = x_step[i];
    int x;
    x = t[i];
    int z;
    u[i] = x;
    z = x;
  }
  int z_step[10];
  for (int j = 0; (j < 10); j++) {
    int &y = z_step[j];
    int y;
    y = t[j];
    u[j] = (y + 1);
    y = u[j];
  }
  int total = 0;
  int x1_step[10];
  for (int k = 0; (k < 10); k++) {
    int a = (k + 1);
    int &x = x1_step[k];
    int x;
    x = (a + 1);
    int y = (x + 1);
    total += y;
  }
}