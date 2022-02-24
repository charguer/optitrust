typedef struct {
  int x[5];
} int_5;


int_5 test () {
  return {{0,1,2,3,4}};
}

int main () {

  int x[5] = {0,1,2,3,4};
  return 0;
}