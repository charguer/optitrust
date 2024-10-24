int test_simpl(int x) { return x; }

void my_test_simpl(int x, int* my_res) { *my_res = x; }

void test_simpl_dps(int x, int* res) { *res = x; }

int test_one_branch(int x) {
  if (x < 0) {
    return -x;
  }
  return x;
}

void test_one_branch_dps(int x, int* res) {
  if (x < 0) {
    *res = -x;
    return;
  }
  *res = x;
}

int test_branches(int x) {
  if (x > 0) {
    return x;
  } else {
    return -x;
  }
}

void test_branches_dps(int x, int* res) {
  if (x > 0) {
    *res = x;
  } else {
    *res = -x;
  }
}

int main() { return 0; }
