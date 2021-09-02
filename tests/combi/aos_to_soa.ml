open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  !! Arrays.aos_to_soa [cVarDef "w"];
  (* PLAN for the basic level:
  1) the user creates a copy "vect2" of the type "vect" -- LATER: after pic demo is completed
      Typedef.copy "vect2" [cTypeDef "vect"]
  2) the user replaces "vect" with "vect2" where desired -- LATER: after pic demo is completed
      => TODO:   Types.replace "pattern" "replacement" target
      vect t;

      => the target gives the scope in which all occurences should be replaced
      Types.replace "vect" "vect2" [cVarDef "t"]
      Types.replace "vect" "vect2" [cVarDef ""]
      Types.replace "vect" "vect2" [cFunArgs ""]
      Types.replace "vect" "vect2" [cFunDef "f_new"] => replace all occurences inside f_new

      in the future
      Types.replace "vect" "vect2" [cBinding ""]
         cBinding = cVarDef | cFunDef | cArg


  3) the user requests copies of functions that have "vect" as argument type or return type
     --> ideally, the call sites would be found automatically
  4) the user calls aos-to-soa on the type "vect2"
  5) the resulting should typecheck, else error

  => LATER: at the combi level,
     high-level combinators to automate the process
    including the feature of "looking whether a copy of the type is needed".
    (in particular when changing only one variable) 
    
    
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
    
    
)


