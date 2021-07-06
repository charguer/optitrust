void h(int* p) {
        (*p)++;
      }

int main(){
  int *q = new int(3);
  h(q);
  return 0;
}
