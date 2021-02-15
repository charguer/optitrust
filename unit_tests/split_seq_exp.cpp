int main() {
  int t;
  {
    int x = 4;
    int y = 3;
    x += y;
    t = y;
  }
  {
    int y = t;
    y++;
    int z = y + 2;
  }
}

