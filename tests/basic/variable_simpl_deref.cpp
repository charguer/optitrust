int f(int x) { return (x + 1); }

int main() {
  int a = 1;
  int *b = &a;
  
  int* c;
  c = &(*b); 
 
  int d;
  d = *(&a);
}
