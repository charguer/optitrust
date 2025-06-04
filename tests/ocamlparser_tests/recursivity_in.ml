let x = 4

let y = "string"

let rec fact n =
  if n = 0
    then 1
    else n * (fact (n-1))
