int *t;
int *u;

int main() {
  for (int i = 0; i < 10; i++) {
    int x = t[i];
    u[i] = x;
    int z = x;
  }
}

/*
Loop hoist on x:

-- step 1 detach init

int main() {
  for (int i = 0; i < 10; i++) {
    int x;
    x = t[i];
    u[i] = x;
    int z = x;
  }
}

-- step 2: extract variable as ref

int main() {
  int x_step[10];
  for (int i = 0; i < 10; i++) {
    int& x = x_step[i];
    x = t[i];
    u[i] = x;
    int z = x;
  }
}

-- step 3: optional (matches the result of extract_var)
  -> this will only be available at the combi level

int main() {
  int x_step[10];
  for (int i = 0; i < 10; i++) {
    x_step[i] = t[i];
    u[i] = x_step[i];
    int z = x_step[i];
  }
}
*/