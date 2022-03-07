typedef struct {
  int x[5];
} int_5;

void test_pointer_arg(int* a, int b){
  *a =b ;
}

int main () {
  
  int x = 10;
  int y;
  y = x;
  return 0;
}