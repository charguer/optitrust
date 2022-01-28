void f() {
  {
    int x = 4;
    int y = 3;
    x += y;
  }
  {
    y++;
    int z = (y + 2);
  }
}

int main() {
  {
    int x = 4;
    int y = 3;
    x += y;
  }
  {
    y++;
    int z = (y + 2);
  }
  return 0;
}
