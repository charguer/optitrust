typedef struct str_vect{
  int n;
} vect;

/* is equivalent to */
  /* struct str_vect{ */
  /*   int n; */
  /* }; */
  /* typedef struct str_vect vect; */

void useless1 (int n);

int useless2 (int n, int m);

const int SIZE = 10;

void useless3 (int t[][SIZE]);

int vect_get (vect v) {
  return v.n;
}

int main() {

  vect v = {1};
  v.n = 0;
  v.n++;

  vect* w;
  w->n = v.n;

  int n = vect_get(v);
  useless1(n);

  n++;

  const int m = 2;

  n = useless2(n, m);

  int *p = &n;
  n = *p;
  p = &n;
  *p = n + 1;

  if (n != 0) {
    return 1;
  }

  if (false)
    return 1;
  else {
    int k = 0;
    n = k + 1;
  }

  w->n = 3;
  const int b = w->n;

  // TODO: fix issue with const pointers
  /*const*/ int* r = &w->n;
  *r = 3;

  int** dp;
  **dp = **dp +1;

  int t[3] = {1, 2, 3};
  t[0] = 1;
  t[1] = t[0] + 1;

  while (true && 1 || false)
    n++;

  while (false) {
    int k = n--;
    if (n >= 0)
      break;
  }

  for (int i = 0; i < 15; i++) {
    // this is forbidden:
    // i += 2;
  }
  for (int i = 2 + 3; i >= 2; i -= 1)
    n *= i;

  vect arr[2];
  for (int i = 0; i < 2; i++) {
    arr[i].n = arr[i].n + 2;
    *(& arr[i].n) = *(& arr[i].n) + 3;
  }

  int *q = new int[SIZE];
  delete[] q;

  switch (n) {
    case 1:
      n++;
      break;
    case 2:
    case 3:
      n += 3;
      break;
    default:
      n--;
      break;
  }

  goto exit;

  exit:
    return 0;
}

typedef struct {
  int n, m;
  double d[SIZE];
} tri_vect;
