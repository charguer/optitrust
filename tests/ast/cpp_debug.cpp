#include <vector>
#include <algorithm>


template<typename A, typename B> struct Box 
  { 
    A key; 
    B value; 
  };

// typedef Box<int, bool> box;



template<typename A, typename B>
void update(Box<A, B>* b, A key, B value) {
  b->key = key;
  b->value = value;
}

int main() {
  Box<int, bool> b;
  // update<int, bool> (&b,1,true);
  // update<int, bool> (&b,1,true);
}


// ---

// class box2 {
//   box<int,bool> b;
// }

// b.update(1,true)


/*
encodings of method calls:
  x.f(y)    function is an access whose LHS is a object (i.e. a value whose type is of some class)
-> 
  f(x,y) @ method_call
*/