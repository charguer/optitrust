open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ -> 

  !! Specialize.function_arg "f1" [false] [cFun "f"];

)
"
int f(int x){ 
  return x + 1
}

int main(){
  int a;
  a = f(1);
  return 0;
}
"



let _ = Run.script_cpp (fun _ -> 


    let tg = [occFirst; cTopFunDef "main"; cFun "f"] in
  !! Specialize.function_arg "f1" [true; true] tg;
  
  !! Specialize.function_arg "f2" [true; false] tg;

  !! Specialize.function_arg "f3" [false; true] tg;

  !! Specialize.function_arg "f4" [false; false] tg;
)
