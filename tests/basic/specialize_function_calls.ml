open Optitrust
open Target

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

     let call_tg = [occIndex 0; cTopFunDef "main"; cFun "f"]  in 
  
  !! Specialize_basic.funcalls "f1" [true; true] call_tg;

  !! Specialize_basic.funcalls "f2" [true; false] call_tg;

  !! Specialize_basic.funcalls "f3" [false; true] call_tg;

  !! Specialize_basic.funcalls "f4" [false; false] call_tg;


)
