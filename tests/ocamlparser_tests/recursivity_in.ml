let x = 4

let y = "string"

let multiply a b = a * b

let rec fact n =
  if n = 0
    then 1
    else multiply n (n+1) (* n * (fact (n-1)) *)
