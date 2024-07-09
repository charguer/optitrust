#include <optitrust.h>

void dummy() {
  int x = 5;
  int y = 2;
}

void f() {
  __pure();
lab2:
lab1:
  int a = 3;
  int b = 4;
  a += b;
  for (int i = 0; i < 4; i++) {
    __strict();
    __smodifies("&a ~> Cell");
    a++;
  }
}
/*
step_name: Label_basic.add
before: #include <optitrust.h>



  int dummy ()  {
  int x = 5;
  int y = 2;
}

  void f ()  {
  __pure();
  int a = 3;
  int b = 4;
  a += b;
  for (int i = 0; i < 4; i++) {
    __strict();
    __smodifies("&a ~> Cell");
    a++;
  }
}

diff: diff --git a/tmp/beforeb787ec.cpp b/tmp/afterf2dc8f.cpp
index 04c20936..a3e1a7df 100644
--- a/tmp/beforeb787ec.cpp
+++ b/tmp/afterf2dc8f.cpp
@@ -2,19 +2,19 @@



   int dummy ()  {
   int x = 5;
   int y = 2;
 }

   void f ()  {
   __pure();
-  int a = 3;
+  lab1: int a = 3;
   int b = 4;
   a += b;
   for (int i = 0; i < 4; i++) {
     __strict();
     __smodifies("&a ~> Cell");
     a++;
   }
 }


*/
