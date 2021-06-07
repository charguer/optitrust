int main() {
  bool a = true;
  if ( a || (true || true) ){
    a = false || true;
  }
}