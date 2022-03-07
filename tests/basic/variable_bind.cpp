typedef struct {
  int x[5];
} int_5;


int_5 test () {
  return {{0,1,2,3,4}};
}

void pointer_arg(int* a, int b){
  *a =b ;
}


int main () {

  int arr[5] = {0,1,2,3,4};
  int x;
  int y;
  pointer_arg(&x, y);
  return 0;
}