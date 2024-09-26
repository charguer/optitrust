typedef struct {
  int* data;
} Type;

/*@global*/ int a = 1; /*global@*/

/*@global*/ char* b; /*global@*/

/*@global*/ float c; /*global@*/

/*@global*/ float d = 0.5f; /*global@*/

/*@global*/ Type e; /*global@*/

void f(Type p) {
  int tmp = 2;
  if (a < 1) {
    int f = 42 + *e.data;
    p.data = &a;
    *e.data = f;
    return;
  }
  int i;
  for (i = 0; i < a; i++) {
    *p.data++;
  }
}
