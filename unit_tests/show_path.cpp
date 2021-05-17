
int main() {

  int y = 5;
  int x = y;
  for (int i = 0; i < 3; i++) {

    if (true) {
      x++;
    } else {
      if (false) { 
        i++;
      }
    }
  }
  return 0;
}
