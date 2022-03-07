typedef struct {
  int x[5];
} int_5;

void test_pointer_arg(int* a, int b){
  *a =b ;
}

int main () {
  
  int x;
  int y;
  test_pointer_arg(&x, y);
  return 0;
}