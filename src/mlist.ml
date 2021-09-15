
type mark = Mark.t

type 'a t = 
  { items : 'a list;
   marks : (mark list) list  }

let create () = 
  { items = []; 
    marks = [[]]}

let length (ml : 'a t) : int = 
  List.length ml.items

let of_list (l : 'a list) : 'a t = 
  { items = l; 
    marks = List.init (List.length l) (fun _i -> [])}

let to_list (ml : 'a t) : 'a list = 
  ml.items

