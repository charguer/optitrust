void task_body (int &);
void gen_task (int &x) { // on orphaned task construct reference argument
  task_body (x);
}
void test (int &y, int &z) {
  {
    y = z + 2;
    gen_task (y); // no matter if the argument is determined private
    gen_task (z); // or shared in the enclosing context.

    y++;          // each thread has its own int object y refers to
    gen_task (y);
  }
}

