
void f () {
    int x = 4;
    int y = 3;
    x += y;
    // split point is here, using 't' as variable name for 'y' across the blocks
    y++;
    int z = y + 2;  
}

int main() {
  {
    int x = 4;
    int y = 3;
    x += y;
    // split point is here, using 't' as variable name for 'y' across the blocks
    y++;
    int z = y + 2;  
  }
  return 0;
}
