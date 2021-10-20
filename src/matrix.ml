include Matrix_basic

(* [biject fun_bij tg] expects the target [tg] to point to a function call to MINDEX 
      then it will replace its name with [fun_bij]
*)
let biject (fun_bij : string) : Target.Transfo.t = 
  Instr_basic.replace_fun fun_bij