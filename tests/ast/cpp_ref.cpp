
int ref_on_mutable_int() {
  int x = 3;
  int& rx = x;
  rx = 4;
  return rx;
}
/* Should be represented in optitrust as:

    int* x = new_int(3);
    int rx = x;
    set(rx, 4);
    return get(rx);

   To achieve this, when processing the declaration of
   a reference type (such as [int&]), we process the right-hand
   side of the definition using the flag "as-left-value".
   Then, we add the reference (here [rx]) to the list of
   heap-allocated variables.
*/

int ref_on_immutable_int() {
  const int x = 3;  // same as [int const x = 3]
  const int& rx = x; // same as [int const& rx = x]
  return rx;
}

/* Should be represented in optitrust as:

    let x : const int = 3
    let rx : const int<annotation:"&"> = x;
    return rx;

   To achieve this, when we see a reference on a const type,
   here written as [const int&], but equivalently represented
   as [int const&], we

   //TODO: what does our code do if we process "as-left-value"
   // an occurence of a const variable  (here "x")?
   // Either it should return just "x"; or we should process
   // the contents as a r-value. I'm not sure.
*/


typedef struct { int x,y; } vect;

int ref_on_field() {
  vect v = { 2, 3 };
  int& vx = v.x;
  vx = 5;
  return vx;
}
/*
  In optitrust

    int ref_on_field() {
      vect* v = new vect{ 2, 3 };
      let vx : int* = struct_access(v,"x");
      set(vx, 5);
      return get(vx);
    }
*/



int ref_on_mutable_int_cell() {
  int t[2] = { 4, 5 };
  int* p0 = &t[0];
  int& r0 = t[0];
  *p0 = 6;
  r0 = 7;
  return *p0 + r0;
}
/* in OptiTrust:

  int ref_on_mutable_int_cell() {
    int[2] t = new_array([4; 5]);
    int* p0 = <annotation="addressof">(array_access(t,0))
    int*<annotation="&"> r0 = array_access(t,0);
      // here, r0 needs to be registered as "heap_allocated"s
    set(p0, 6)
    set(r0, 6);
    return get(p0) + get(r0);
  }
*/

int ref_on_immutable_int_cell() {
  int const t[2] = { 4, 5 };
  int const& r0 = t[0];

  vect const v[2] = { { 2, 3 }, { 4, 5 } };
  vect const& w = v[0];

  return r0 + w.x;
}
/* in OptiTrust:

  int ref_on_immutable_int_cell() {
    let t : (const int)[2] = array{ 4, 5 };
    let r0 : (const int)<annotation="&"> = array_get(t,0);
    let v : (const vect)[2] = ...
    let w : (const vect)<annotation="&"> = array_get(v,0)
    return r0 + struct_get(w,"x");
  }
*/


int ref_argument(int& x, int const& y, int const& z) {
  x = x + y;
  int& u = x;
  const int& v = y;
  return u + v + z;
}
/*
  in OptiTrust

  int ref_argument(int*<annotation="&"> x, (const int)<annotation="&"> y, (const int)<annotation="&"> z) {
    set(x, get(x) + y);
    int*<annotation=&> u = x;
    int<annotation="const&"> v = y;
    return get(u) + v + z;
  }

*/

int main() {
  int a = 3;
  int b = 4;
  const int c = 5;
  ref_argument(a, b, c);
}
/*
  in OptiTrust

  int* a = new int 3;
  int* b = new int 4;
  let c : int = 5
  ref_argument(a, virtual_get(b), c);

  // the "virtual_get" operation provides a "constant"
  representation of a mutable object, over a specific scope

  The trickiest part is the implicit conversion of [b] from
  [int] to [const int] during the call, which means that we
  need in OptiTrust to insert a [get] operation.
  The implementation thus needs to have access to the type
  of the functions for translation the calls to that function.
*/



//   particle& p = cells[idCell].data[idParticle];