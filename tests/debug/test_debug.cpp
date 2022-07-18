void f(float a);

class T {
  int x;
  public:
    int get_x(){ return x;}
};

void f(T a);


int main(){
  T x;
  f(x);
  f(0.);
}