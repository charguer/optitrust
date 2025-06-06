let x = 4

let y = "string"

let multiply a b = a * b

let rec fact n =
  if n = 0
    then 1
    else
      let cont = (fact (n-1)) in
      let cont2 = n * cont in
      cont2
