int main(){
  int a = 6, b = 10;
  const int B = 2;
  int sum = 0;
  for(int i = a; i < b; i += B){
    sum += i;
  }
}