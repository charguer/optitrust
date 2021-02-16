int *t;
int *u;


int main() {
  
  for (int i = 0; i < 10; i++) {
    int x; // variable to be extracted
    x = t[i];
    u[i] = x; 
  }
}


// NOTE: we currently need the braces around the block