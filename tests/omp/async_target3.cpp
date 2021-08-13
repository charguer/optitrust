#include <stdio.h>

const int N = 1000000;      //N must be even
void init(int n, float *v1, float *v2);

int main(){
   int   i, n=N;
   int   chunk=1000;
   float v1[N],v2[N],vxv[N];

   init(n, v1,v2);

   {


      for(int i=0; i<n/2; i++){ vxv[i] = v1[i]*v2[i]; }

      for(int j =n/2; j<n; j++){ vxv[j] = v1[j]*v2[j]; }

   }
   printf(" vxv[0] vxv[n-1] %f %f\n", vxv[0], vxv[n-1]);
   return 0;
}