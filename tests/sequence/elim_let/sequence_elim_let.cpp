int f() {
  const int ff = ({
    const int res = 0;
    res;
  });
  const int gg = ({
    const int k = 5;
    const int res = k*k;
    res;
  });
  return 2;
}
