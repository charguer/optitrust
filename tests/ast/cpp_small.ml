(* A test for the parsing of C++ features *)

open Optitrust
open Target

let _ = Flags.set_dump_clang_ast()

let _ = Flags.use_light_diff := false

let _ = Run.script_cpp (fun () ->
  (* Trace.reparse(); *)
  ()
)

(*


RECORD
================

current:

  | Trm_struct of trm mlist (* { 4, 5.3 } as a record  *)


new:

  | Trm_record of (label option * trm) mlist
       (* in OCaml: { x = 4; y = 5 }
          in C:     { 5, 6 } or { .x = 5, .y = 6 }

          the OCaml construct { t1 with x = 4; y = 5 }
          is encoded as a call:    __with(t1, { x = 4; y = 5 })
         *)

CLASS
================

current:
  | Typdef_prod of bool * (label * typ) list (* for records / struct,
                                      e.g. [type 'a t = { f : 'a; g : int } *)

new:

  | Typdef_record of record_fields

  type record_fields = (record_field * record_field_annot) list
  type record_field =
    | Record_field_member of (label * typ)
    | Record_field_method of trm

  type record_field_annot = access_control

  and access_control =
    | Access_public
    | Access_private
    | Access_protected

val typedef_get_members : ?(access: access_control option) -> trm -> (label * typ) list
val typedef_get_methods : ?(access: access_control option) -> trm -> trm

type trm_annot = + "static"


===========================

THIS POINTER


class foo {
  int x;
  void f(int m) {
   x += m + this->x; // here "this" is a variable
   f(3);
  };
}
foo* v;
v->f(1);
foo w;
w.f(1);


becomes during an encoding phase method_intro/method_elim:


class foo {
  int x;
  static void f(foo* this, int m) { @ annotation "was a method"
    this->x += m + this->x;
    f(this, 3)
  };
}
foo* v;
f(v, 1); @ annotation "was a method"
foo w;
f(&w, 1); @ annotation "was a method"



==============================

TEMPLATES
--------------------------------------
Namespaces could be added as annotations:

Namespace of string

-----------------------------------------
Template declarations are already implemented/


--------------------------------------------
Specializations:

TemplateSpecialization
                         { name = NameTemplate ("vector");
                           args =
                           [Type
                            ({ cxtype = <opaque>; type_loc = <opaque>;
                               const = false; volatile = false;
                               restrict = false; desc = BuiltinType (Int) })]
                           }
                         }


we can add a new type Template_spec of (string * typ list)


================================
Methods accesses are translated as calls:

For example v.push_back(3) is translated as a call to v.push_back with argument 3.

Member accesses should be quite similar to struct accesses.


===================================
ITERATORS

std::begin is translated as a function call with attribute Namespace "std".
similar for std::end


====================================
LAMBDAS
Not sure



*)