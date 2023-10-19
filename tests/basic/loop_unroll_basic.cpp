int main(){
  int* t;
  int a = 2;
  for (int i = 0; i < 3; i++) {
    int a = i * 2;
    t[i] = a;
  }
  return 0;
}

void name_conflict_1() {
  for (int j = 0; j < 2; j++) {
    int a = j;
  }
}

void name_conflict_2() {
  for (int k = 0; k < 2; k++) {
    int a = k;
  }
}
