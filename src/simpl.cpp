

void simpl_bool() {
  bool b;
  or_true_l: (true || b) = true;
  or_true_r: (b || true) = true; // only if b has no errors
  or_false_l: (false || b) = b;
  or_false_r: (b || false) = b;
  or_same: b || (b = b); // only if b has no effects
}

void simpl_int() {
  int n;
  int_mul_zero_l: (0 * n) = 0;
  int_mul_one_l: (1 * n) = n;
}

void rew_int() {
  int n1, n2, n3;
  int_comm: (n1 * n2) = (n2 * n1);
  int_assoc: (n1 * (n2 * n3)) = ((n1 * n2) * n3);
}

void simpl_bool() {
  double d;
  double_mul_zero_l: (0. * d) = 0.;
}

void simpl_if_true() {
  void e1, e2;
  source:
  if (true) { e1 /* or f("e1"); */ } else { e2 };
  target:
  e1;
}

void simpl_if_same() {
  void e1;
  source:
  if (true) { e1 } else { e1 };
  target:
  e1;
}
