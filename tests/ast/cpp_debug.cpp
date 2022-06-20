template <typename A> 
class Inject {

  static void f(Inject x ) { 
    Inject<A> y;
    Inject z; // encoded as (Foo<A>@Annot_injected) y;
  }


};

int main(){}
