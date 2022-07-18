#include <math.h>
const int COLS = 100;

void gramSchmidt(float Q[][COLS], const int rows)
{
    int cols = COLS;
    for(int k=0; k < cols; k++)
    {
        double tmp = 0.0;

        for(int i=0; i < rows; i++)
            tmp += (Q[i][k] * Q[i][k]);

        tmp = 1/sqrt(tmp);

        for(int i=0; i < rows; i++)
            Q[i][k] *= tmp;
    }
}

/* Note:  The variable tmp is now mapped with tofrom, for correct 
   execution with 4.5 (and pre-4.5) compliant compilers. See Devices Intro. 
 */

