
typedef struct { int x; int y; } vect;

int main() {
  int i,j;
  for ( i = 0; i < 10; i++) {
     for (j = 0; j < 5; j += 2) {
       i++;
     }
  }
  vect v = {0 , 0};
  vect v2 = v;
  v2 = {0 , 0};
}
