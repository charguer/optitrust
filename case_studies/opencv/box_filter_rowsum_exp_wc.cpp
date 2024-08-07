void rowSum(const int kn, const T* S, ST* D, const int n, const int cn) {
  if (kn == 3) {
    for (int ic = 0; ic < n * cn; ic++) {
      D[ic / cn * cn + ic % cn] = (st)S[ic / cn * cn + ic % cn] +
                                  (st)S[(1 + ic / cn) * cn + ic % cn] +
                                  (st)S[(2 + ic / cn) * cn + ic % cn];
    }
  }
  else {
    if (kn == 5) {
      for (int ic = 0; ic < n * cn; ic++) {
        D[ic / cn * cn + ic % cn] = (st)S[ic / cn * cn + ic % cn] +
                                    (st)S[(1 + ic / cn) * cn + ic % cn] +
                                    (st)S[(2 + ic / cn) * cn + ic % cn] +
                                    (st)S[(3 + ic / cn) * cn + ic % cn] +
                                    (st)S[(4 + ic / cn) * cn + ic % cn];
      }
    }
    else {
      if (cn == 1) {
        st s = (st)0;
        for (int i = 0; i < kn; i++) {
          s = s + (st)S[i];
        }
        D[0] = s;
        for (int i = 1; i < n; i++) {
          s = s + (st)S[-1 + i + kn] - (st)S[-1 + i];
          D[i] = s;
        }
      }
      else {
        if (cn == 3) {
          st s = (st)0;
          st s5 = (st)0;
          st s6 = (st)0;
          for (int i = 0; i < kn; i++) {
            s = s + (st)S[3 * i];
            s5 = s5 + (st)S[1 + 3 * i];
            s6 = s6 + (st)S[2 + 3 * i];
          }
          D[0] = s;
          D[1] = s5;
          D[2] = s6;
          for (int i = 1; i < n; i++) {
            s = s + (st)S[3 * (-1 + i + kn)] - (st)S[3 * (-1 + i)];
            s5 = s5 + (st)S[1 + 3 * (-1 + i + kn)] -
                 (st)S[1 + 3 * (-1 + i)];
            s6 = s6 + (st)S[2 + 3 * (-1 + i + kn)] -
                 (st)S[2 + 3 * (-1 + i)];
            D[3 * i] = s;
            D[1 + 3 * i] = s5;
            D[2 + 3 * i] = s6;
          }
        }
        else {
          if (cn == 4) {
            st s = (st)0;
            st s7 = (st)0;
            st s8 = (st)0;
            st s9 = (st)0;
            for (int i = 0; i < kn; i++) {
              s = s + (st)S[4 * i];
              s7 = s7 + (st)S[1 + 4 * i];
              s8 = s8 + (st)S[2 + 4 * i];
              s9 = s9 + (st)S[3 + 4 * i];
            }
            D[0] = s;
            D[1] = s7;
            D[2] = s8;
            D[3] = s9;
            for (int i = 1; i < n; i++) {
              s = s + (st)S[4 * (-1 + i + kn)] -
                  (st)S[4 * (-1 + i)];
              s7 = s7 + (st)S[1 + 4 * (-1 + i + kn)] -
                   (st)S[1 + 4 * (-1 + i)];
              s8 = s8 + (st)S[2 + 4 * (-1 + i + kn)] -
                   (st)S[2 + 4 * (-1 + i)];
              s9 = s9 + (st)S[3 + 4 * (-1 + i + kn)] -
                   (st)S[3 + 4 * (-1 + i)];
              D[4 * i] = s;
              D[1 + 4 * i] = s7;
              D[2 + 4 * i] = s8;
              D[3 + 4 * i] = s9;
            }
          }
          else {
            for (int c = 0; c < cn; c++) {
              st s = (st)0;
              for (int i = 0; i < kn; i++) {
                s = s + (st)S[i * cn + c];
              }
              D[c] = s;
              for (int i = 1; i < n; i++) {
                s = s + (st)S[(-1 + i + kn) * cn + c] -
                    (st)S[(-1 + i) * cn + c];
                D[i * cn + c] = s;
              }
            }
          }
        }
      }
    }
  }
}
