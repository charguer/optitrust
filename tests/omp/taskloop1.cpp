void long_running_task();
void loop_body(int i, int j);

void parallel_work() {
   int i, j;
   long_running_task(); // can execute concurrently
   for (i = 0; i < 10000; i++) { // can execute concurrently           
     for (j = 0; j < i; j++) {
        loop_body(i, j);
     }
   }
}
