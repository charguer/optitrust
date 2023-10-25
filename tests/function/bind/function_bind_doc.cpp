
int f(int x){ 
  return x + 1;
}

int g(int a){ 
  return a - 1;
}

int main(){
  int u;
  int t = g(f(u));
}
