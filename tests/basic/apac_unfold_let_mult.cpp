// commented declaration does not work because of the different encoding of variable between Trm_let and Trm_let_mult
int f() {
  int i;
  int a = 1, b = i, &c = i, d[5] = {1, 2, 3, 4, 5}, *e = &i, /* *f = &a, */ *g = e, *const h = &i, /* *const j = &a, */ *const k = e;
  int const l = 1, m = i, &n = 1, &o = i, p[5] = {1, 2, 3, 4, 5}, *q = &i, /* *r = &l, */ *s = q, *const t = &i, /* *const u = &l, */ *const v = q;
}