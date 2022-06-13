#include <vector>
#include <algorithm>


class test_class {

private:

  int x;

public:

  
  void move(int d) {
     this->x += d;
  }
  
  // for each function, mark it as "public" or "private" as a trm_annot
  void move(int d) {
     x += d;
  }

  

  // encoded as:
  void move(test_class* this, int d) {
     this->x += d; // first move above this
    //  (this@implicit_this)->x += d; // second move above this
     // note: in clangml the base is empty in the second case
  }
 

  bool test_this() {
    return this->x == x;
  }

};



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