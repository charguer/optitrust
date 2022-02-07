void warning() {
  {
    int x = 4;
    int y = x + 2;
  }
}

int main() {
  int x = 0;
  int y = 0;
  {
    x++;
    y++;
  }
  {
    x += 2;
    y += 2;
  }
  return 0;
}
