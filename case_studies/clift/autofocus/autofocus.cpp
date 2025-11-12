#include "optitrust.h"
void testo(int * x,int n){
  __modifies("x~>Matrix1(n)");

}
void test(int * x,int n){
  __modifies("x~>Matrix1(n)");
  testo(x,n);
}
