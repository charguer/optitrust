int f(int x) {
  if (x > 0) {
    return x;
  } else {
    return -x;
  }
}

void f_dsp(int x, int* res) {
  if (x > 0) {
    *res = x;
  } else {
    *res = -x;
  }
}


int main (){

  int r;
  int x = 1; 
  r = f(x);
  return 0;
}
