void iter_nat_for(int n, void body(int)) {
  for (int i = 0; i < n; i++) {
    body(i);
  }
}

void test_ho() {
  int s = 0;
  int m = 3;
  hobody: {
    for (int j = 0; j < m; j++) {
      {
        s += 2*j;
        s -= j;
      }
    }
  }
}


