int *t;

int *u;

int main() {
  int x_step[10];
  for (int i = 0; (i < 10); i++) {
    int &x = x_step[i];
    x = t[i];
    u[i] = x;
    int z = x;
  }
  int y_step[10];
  for (int j = 0; (j < 10); j++) {
    int &y = y_step[j];
    y = t[j];
    u[j] = (y + 1);
    y = u[j];
  }
  int total = 0;
  int x_step[10];
  for (int k = 0; (k < 10); k++) {
    int a = (k + 1);
    int &x = x_step[k];
    x = (a + 1);
    int y = (x + 1);
    total += y;
  }
}
