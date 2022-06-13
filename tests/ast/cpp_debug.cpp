template<typename A, typename B> struct Box 
  { 
    A key; 
    B value; 
  };

// typedef Box<int, bool> box;

class Box2 {
  Box<int,bool> b;
};
// b.update(1,true)


template<typename A, typename B>
void update(Box<A, B>* b, A key, B value) {
  b->key = key;
  b->value = value;
}

int main() {
  Box<int, bool> b;
  Box<float, bool> b1;
  update<int, bool> (&b,1,true);
  update<float, bool> (&b1,1.,true);
}
