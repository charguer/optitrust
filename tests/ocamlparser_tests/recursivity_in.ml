(* let multiply a b = a * b
 *)
(* let rec fact n =
  if n = 0
    then 1
    else
      let cont = (fact (n-1)) in
      n * cont
 *)

let rec fib n =
  if n <= 1
    then 1
    else
      let a = fib (n-1) in
      let b = fib (n-2) in
      a + b
