
typedef struct {
  int x;
  int y;
} vect;

vect f(int a){
  return { a-1, a};
}

int main(){
  int p = f(3).x;
}
