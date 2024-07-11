int main() {
  int eu, n = 0, m = 1;
  eu = (n / m) * m + (n % m); // = n
  eu = m + (n % m) + m * (n / m); // = m + n
}
