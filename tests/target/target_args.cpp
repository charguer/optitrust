
int main() {

  { const int x = 1;
    const int y = 2; }

  { int x = 1;
    int y = 2; }

  { int x = 1;
    int y = 2;
    int z = 3; }

  { int x = 1;
    int y = 2;
    x++; }

}