
int main() {
  int x = 3;
  int y = 2;
  int z = 5;
  int t = 6;
  int u = 7;
  int w = 9;
  for (int i = 0; i < 10; i++){
    w = i;
  }
  for (int j = 0; j < 10; j++){
    w = j;
  }

  int a = 0;
  for (int k = 0; k < 10; k++) {
    int a = 1;
    int b = a + a;
    int c = a + b;
  }

  for (int l = 0; l < 20; l++) {
    int a;
    a = 0;
    for (int l2 = 0; l2 < 10; l++) {
      if (l < l2) {
        a = l2;
      }
    }
  }

  return 0;
}

