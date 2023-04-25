class Matrix {
  Matrix (int n){ 
    len = n;
  }
  ~Matrix(){
    // NOTE: delete map type shoudl be used, since the corresponding 
    // host data will cease to exist after the deconstructor is called.
    delete[] v;
  }
  private:
    double* v;
    int len;
};