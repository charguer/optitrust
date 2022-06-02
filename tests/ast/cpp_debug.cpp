#include <stdio.h>
typedef struct { int k; int l; int a[2]; } T;
typedef struct { int i;  T t; } S;
T x = {.k = 42, .l = 43, .a[1] = 19, .a[0] = 18 };
 // x initialized to {42, 43, {18, 19} }
int main(void)
{
    S l = { 1,          // initializes l.i to 1
           .t = x,      // initializes l.t to {42, 43, {18, 19} }
          };
    printf("l.t.k is %d\n", l.t.k); // .t = x sets l.t.k to 42 explicitly
                                    // .t.l = 41 would zero out l.t.k implicitly
}