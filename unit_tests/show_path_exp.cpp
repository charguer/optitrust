int main() {
/*@0<*/ int x = 3; /*>0@*/

 for (int i = 0; (i < 3); i++) {
    if (true) {
      x++;
    } else {
      i++;
    }
  }
  
  return 0;
}

