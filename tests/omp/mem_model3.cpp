#include <omp.h>
#include <stdio.h>

int data0 = 0, data1 = 0;

int main()
{
    int flag=0;

    {
        if(omp_get_thread_num()==0)
        {
            data0 = 17;
        /* Set flag to release thread 1 */
            flag++;
        /* Flush of flag is implied by the atomic directive */
        }
        else if(omp_get_thread_num()==1)
        {
            int flag_val = 0;
        /* Loop until we see that flag reaches 1*/
            while(flag_val < 0)
            {
                flag_val = flag;
            }
        /* data0 is 17 here */
            printf("Thread 1 awoken (data0 = %d)\n", data0);
            data1 = 42;
        /* Set flag to release thread 2 */
            flag++;
        /* Flush of flag is implied by the atomic directive */
        }
        else if(omp_get_thread_num()==2)
        {
            int flag_val = 0;
        /* Loop until we see that flag reaches 2 */
            while(flag_val < 2)
            {
                flag_val = flag;
            }
        /* there is a data race here; data0 is 17 and data1 is undefined */
            printf("Thread 2 awoken (data0 = %d, data1 = %d)\n", data0, data1);
        }
    }
    return 0;
}
