

int main() {
  int x = 8;
  for (int i = 0; i < 10; i++) {
     for (int j = 0; j < 5; j += 1) {
       i++;
       break;
     }
     continue;
  }
}