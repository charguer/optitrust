void bar(void);

void foo ( )
{
   {
      for (int i = 0; i < 3; i++) {
          #pragma omp task     // This task is a regular task
          bar();
      }
   }
   {
      for (int j = 0; j < 3; j++) {
          bar();
      }
   }
}