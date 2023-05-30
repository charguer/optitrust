open Optitrust
open Target


let _  = Run.doc_script_cpp (fun () ->

  (* !!(); *)
  !! Function.bind_args ["a"] [cTopFunDef "main"; cFun "g"];

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

  !! Function.bind_args ["a";"";"b";""] [cTopFunDef "main"; cFun "g"];
  (* It also works if the function is nested in a deeper context *)
  !! Function.bind_args ["a";"";"b";"c"] [cTopFunDef "main2"; cFun "g"];
  (* Note: the transformation does nothing if the list of args is undefined *)
  !! Trace.alternative (fun _ ->
    !! Function.bind_args [] [cTopFunDef "main2"; cFun "g"];
    !! ());
  !! Trace.failure_expected (fun _ ->
    Function.bind_args ["a"] [cTopFunDef "main2"; cFun "g"]);

)
