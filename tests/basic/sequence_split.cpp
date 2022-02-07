
void warning () {
    // WARNING: splitting after x results in invalid code, because x is not bound
    // in the second statement, thus it gets printed as (*x); in general, split
    // needs to be followed by other operations in order to obtain valid code,
    // when variable declarations are contained in the scope of the function
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
    x += 2;
    y += 2;
  }
  return 0;
}
