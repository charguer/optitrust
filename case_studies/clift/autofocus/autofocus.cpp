#include "optitrust.h"
void testo(int * YOYOYO,int n){
  __modifies("YOYOYO~>Matrix1(n)");

}
void test(int * YOYOYO,int n){
  __modifies("YOYOYO~>Matrix1(n)");
  testo(YOYOYO,n);
}
