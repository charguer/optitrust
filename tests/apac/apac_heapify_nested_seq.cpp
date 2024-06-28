int main() {
  {
    int a = 0;
    int b[5] = {1, 2, 3, 4, 5};
    
    while (a < 10) {
      // FIXME: If we re-declare 'b' here as simple integer instead of using a
      // completely different variable name, e.g. 'c', OptiTrust says "does not
      // support array subscript base type 'Some int'". It looks like there is a
      // type binding problem somewhere!
      int c;
      c = 1;
      a++;
    }
    
    a = 1;
    b[1] = 2;
    
    return 1;
  }
  
  return 1;
}
