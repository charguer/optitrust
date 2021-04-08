
typedef int T[2];
typedef struct {
  int x;
  int y;
} vect;

// Not supported yet
typedef struct {
  T t[2];
  int g;
} particle;

int main(){
  T t[2];
  
  vect v[2];
  v[0] = {1,2};
  v[1] = {3,4};

  return 0;
}