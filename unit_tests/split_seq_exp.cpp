int main() {
  int t;
  {
    int x = 4;
    int y = 3;
    x += y;
    y++;
    t = y;
  }
  {
    int y = t;
    int z = (y + 2);
  }
}
