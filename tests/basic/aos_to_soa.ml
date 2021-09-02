open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  !! Arrays_basic.aos_to_soa "vect2" "N";
)


(*

EXAMPLE

vect { int x }


vect2 { int x[N] }

vect2 t;
vect u[N];

void f_new(vect2 x) {
   t[i].x  -->  t.x[i]
}

void f(vect x) {
   u[i].x  --> remains
}

int main() {
  f(t);
  f(u);
}

*)