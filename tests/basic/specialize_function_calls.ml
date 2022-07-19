open Optitrust
open Target


(* TODO: Fix the weird issue with occIndex *)


let _ = Run.doc_script_cpp (fun _ -> 


  !! Specialize_basic.funcalls "f1" [false] [cTopFunDef "main"; cFun "f"];

)
"
int f (int x){
  return x + 1;
}

int f1 () {
  f(1);
}
int main(){

  int a;
  a = f(1);

  return 0;
}
"

let _ = Run.script_cpp (fun _ -> 

     let main = cTopFunDef "main" in 
  
  !! Specialize_basic.funcalls "f1" [true; true] [occIndex 0; main;cFun "f"];

  !! Specialize_basic.funcalls "f2" [true; false] [occIndex 0; main;cFun "f"];

  !! Specialize_basic.funcalls "f3" [false; true] [occIndex 0; main;cFun "f"];

  !! Specialize_basic.funcalls "f4" [false; false] [occIndex 0; main; cFun "f"];


)
