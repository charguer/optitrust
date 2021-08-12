#include <omp.h>
#include <stdio.h>
void work() {
    omp_lock_t lock;
    {
      for (int i = 0; i < 100; i++) {
        {
        // lock is shared by default in the task
                // Capture data for the following task
        // Task Scheduling Point 1
          { printf ("do work here\n"); }
        }
      }
    }
}