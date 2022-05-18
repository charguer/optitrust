int test_simpl(int x) { return x; }

void test_simpl_dsp(int x, int* res) { *res = x; }

int test_one_branch(int x) {
  if (x < 0) {
    return -x;
  }
  return x;
}

void test_one_branch_dsp(int x, int* res) {
  if (x < 0) {
    {
      *res = -x;
      goto exit_label;
    }
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

void test_branches_dsp(int x, int* res) {
  if (x > 0) {
    *res = x;
  } else {
    *res = -x;
  }
}

int main() { return 0; }
