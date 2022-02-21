typedef struct {

  int x;
  int y;

} vect;



int main () {

  const vect  a = {0, 1};
  vect b;
  
  b = a;

  return 0;
}