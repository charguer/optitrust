template <typename A, typename B>
struct Box {
  A key;
  B value;
};

class Box2 {
  Box<int, bool> b;
};

template <typename A, typename B>
void update(Box<A, B>* b, A key, B value) {
  b->key = key;
  b->value = value;
}

int main() {
  Box<int, bool> b;
  update<int, bool>(&b, 1, true);
  update<int, bool>(&b, 1, true);
  return 0;
}
