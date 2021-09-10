void task_body(int &);

void gen_task(int &x) {
#pragma omp task
  task_body(x);
}

void test(int &y, int &z) {
#pragma omp parallel private(y)
  {
    y = (z + 2);
    gen_task(y);
    gen_task(z);
    y++;
    gen_task(y);
  }
}