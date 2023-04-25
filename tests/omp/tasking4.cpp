     int fib(int n) {
        int i, j;
        if (n<2)
          return n;
        else {
          i = fib(n-1);
          j = fib(n-2);
          return i+j;
        }
     }



