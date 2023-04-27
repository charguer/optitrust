open Optitrust
open Target

let _ = Run.doc_script_cpp (fun () -> 


  !! Function.bind [cFun "f"];

)

"
int f(int x){ 
  return x + 1;
}

int g(int a){ 
  return a - 1;
}

int main(){
  int u;
  int t = g(f(u));
}
"

let _ = Run.script_cpp ~parser:CParsers.clang (fun _ ->
  !! Function.bind ~fresh_name:"r" ~args:["a";"";"b";""] [cFun "g"];

  (* default is to not name any of the arguments *)
  !! Trace.alternative (fun () ->
    Function.bind ~fresh_name:"r" [cFun "g"];
    !!();
  );

  (* default name is "res" for the result *)
  !! Trace.alternative (fun () ->
    Function.bind [cFun "g"];
    !!();
  )

)
