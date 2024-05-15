#include <optitrust.h>

int dummy() {
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
before:
I2luY2x1ZGUgPG9wdGl0cnVzdC5oPgoKCgogIGludCBkdW1teSAoKSAgewogIGludCB4ID0gNTsKICBpbnQgeSA9IDI7Cn0KCiAgdm9pZCBmICgpICB7CiAgX19wdXJlKCk7CiAgaW50IGEgPSAzOwogIGludCBiID0gNDsKICBhICs9IGI7CiAgZm9yIChpbnQgaSA9IDA7IGkgPCA0OyBpKyspIHsKICAgIF9fc3RyaWN0KCk7CiAgICBfX3Ntb2RpZmllcygiJmEgfj4gQ2VsbCIpOwogICAgYSsrOwogIH0KfQo=
diff:
ZGlmZiAtLWdpdCBhL3RtcF9iZWZvcmUuY3BwIGIvdG1wX2FmdGVyLmNwcAppbmRleCAwNGMyMDkzNi4uYTNlMWE3ZGYgMTAwNjQ0Ci0tLSBhL3RtcF9iZWZvcmUuY3BwCisrKyBiL3RtcF9hZnRlci5jcHAKQEAgLTIsMTkgKzIsMTkgQEAKIAogCiAKICAgaW50IGR1bW15ICgpICB7CiAgIGludCB4ID0gNTsKICAgaW50IHkgPSAyOwogfQogCiAgIHZvaWQgZiAoKSAgewogICBfX3B1cmUoKTsKLSAgaW50IGEgPSAzOworICBsYWIxOiBpbnQgYSA9IDM7CiAgIGludCBiID0gNDsKICAgYSArPSBiOwogICBmb3IgKGludCBpID0gMDsgaSA8IDQ7IGkrKykgewogICAgIF9fc3RyaWN0KCk7CiAgICAgX19zbW9kaWZpZXMoIiZhIH4+IENlbGwiKTsKICAgICBhKys7CiAgIH0KIH0K

*/
