void long_running_task();
void loop_body(int i, int j);

void parallel_work() {
   {
    long_running_task(); // can execute concurrently
    for (int i = 0; i < 10000; i++) { // can execute concurrently           
       for (int j = 0; j < i; j++) {
          loop_body(i, j);
       }
    }
   }
}
