#include <iostream>

void printReference (int& x)
{
    int y;
    y = x;
}

void printReference1 (int&& x)
{   
    int y;
    y = x+1;
}


int main() {
  // inlining without removal of 'a'
  const int a = 2;
  const int b = a + a;

  // inlining with removal of 'c'
  const int c = 2;
  const int d = c + c;

  // inlinine of not a constant
  int x = 3;
  int y = x + x;

  // inlining in a return expression
  
  int &u = y;
  u = 5;
  int z = 4;

  return z;
}

