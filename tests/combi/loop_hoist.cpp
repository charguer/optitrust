int *t;
int *u;

int main() {
  for (int i = 0; i < 10; i++) {
    int x = t[i];
    u[i] = x;
    int z = x;
  }

  for (int j = 0; j < 10; j++) {
    int y = t[j];
    u[j] = y + 1;
    y = u[j];
  }
  int total = 0;
  for (int k = 0; k < 10; k++) {
    int a = k + 1;
    int x = a + 1;
    int y = x + 1;
    total += y;
  }
}

