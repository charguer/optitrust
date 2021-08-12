#include <omp.h>
#include <stdio.h>
int main()
{
    int data;
    int flag=0;
    {
      if (omp_get_thread_num()==0)
      {
      /* Write to the data buffer that will be
       * read by thread */
          data = 42;
      /* Flush data to thread 1 and strictly order
       * the write to data relative to the write to the flag */
      /* Set flag to release thread 1 */
          flag = 1;
      }
      else if(omp_get_thread_num()==1)
      {
      /* Loop until we see the update to the flag */
          int flag_val = 0;
          while (flag_val < 1)
          {
             flag_val = flag;
          }
      /* Value of flag is 1; value of data is undefined */
          printf("flag=%d data=%d\n", flag, data);
      /* Value of flag is 1; value of data is 42 */
          printf("flag=%d data=%d\n", flag, data);
      }
    }
    return 0;
}

