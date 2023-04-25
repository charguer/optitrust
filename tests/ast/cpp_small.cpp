
class test_static_class {

private:
  // "static" is a trm_annot
  static int foo(int x) {
     return x;
  }

public:

   static int bar(int x) {
     return x;
  }

};


class test_class {

private:

  int x;

public:

  
  void move_this(int d) {
     this->x += d;
  }
  
  // for each function, mark it as "public" or "private" as a trm_annot
  void move(int d) {
     x += d;
  }

  

  /* encoded as:
  void move(test_class* this, int d) {
     this->x += d; // first move above this
     (this@implicit_this)->x += d; // second move above this
     // note: in clangml the base is empty in the second case
  }
  */

  bool test_this() {
    return this->x == x;
  }

};


// we will consider templated functions and templated classes,
// only with simple typename arguments

// the function/class binds a list of type variables
template<typename T>
bool test_poly(T* x, T* y) { // occurence of T is a Typ_var
  return x == y;
}


#include <vector>
#include <algorithm>

// foo::bar is a "qualified variable",
// same as modules in ocaml M.N.x
// type qvar =  { qvar_var = "x"; qvar_path = ["M";"N"]; qvar_str = "M.N.x" in OCaml or "M::N::x" in C }

void test_vector() {
  std::vector<int> v;   // vector<int> is a typ_constr (typ_constrid, typ_int)
  v.push_back(3);  // encoded as push_back(v,3)
  int a = v[0];
}

int test_iterator(std::vector<int> v) {
  int r = 0;
  for (auto it = std::begin(v); it != std::end(v); it++) { // auto type needs to be supported
      r += *it;
  }
  return r;
}

void test_lambda(std::vector<int> v) {
  int r = 0;
  auto f = [&](int const& x) -> void { // we only support arguments by references [&]
     r += x; }; // trm_fun
  std::for_each(std::begin(v), std::end(v), f);
}

int test_lambda_inline(std::vector<int> v) {
  int r = 0;
  std::for_each(std::begin(v), std::end(v), [&](int const& x) { // we only support arguments by references [&]
     r += x; }); // trm_fun
  return r;
}


using namespace std;

void test_using() {
  vector<int> v;
  v.push_back(3);
  int a = v[0];
}

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
class Box2 {
  Box<int,bool> b;
  public:
    void update1 (int key, bool value){
      update<int, bool> (&b, key, value);
    }
};

template <typename A> 
class Inject {
  static void f(Inject x ) { 
    Inject<A> y;
    Inject z; // encoded as (Foo<A>@Annot_injected) y;
  }

};

class Test {
    int x;
  public:
    Test();
    Test(int x);
    int get();
    void set(int y);


};

Test :: Test(int y){}

Test :: Test(){
   x = 0;
}

int Test :: get(){
  return x;
}

void Test :: set(int y){
  x = y;
}

class A {
  int a;
  int b;

 public:
  A(int i) { a = i; }
  A(int i, int j) { a = i; }
  A(int i, int j, int k) { a = i; }
};

int main() {
  Box<int, bool> b;
  update<int, bool> (&b,1,true);

  Box<float, bool> b1;
  update<float, bool> (&b1,1.,true);

  Box2 b2;
  b2.update1(1, true);


  Test t(10);
  t.set(10);
  int y = t.get();

  A x1(1);
  A y1(1, 2);
  A z1(1, 2, 3);

  return 0;
}
