typedef struct {
  int * data;
} Type;

int a = 1;

char * b;

float c, d = 0.5f;

Type e;

void f(Type p) {
  int tmp = 2;
  if(a < 1) {
    int f = 42 + *e.data;
    p.data = &a;
    *e.data = f;
    return;
  }

  int i;
  for(i = 0; i < a; i++) {
    *p.data++;
  }
}
