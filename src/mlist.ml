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


let mapi (map_fun : int -> 'a -> 'b) (ml : 'a t) : 'b t = 
  let new_items = List.mapi map_fun ml.items in
  {ml with items = new_items}

let map (map_fun : 'a -> 'b) (ml : 'a t) : 'b t =
  mapi (fun _i x -> map_fun x) ml 

let fold_left (acc_f : 'b -> 'a -> 'b) (acc : 'b) (ml : 'a t) : 'b =
  List.fold_left acc_f acc ml.items 

let nth (ml : 'a t) (index : int) : 'a = 
  List.nth ml.items index 

let foldi (acc_f : 'b -> 'a -> 'c -> 'b) (acc : 'b) (ml : 'a t) : 'b =
  Tools.foldi acc_f acc ml.items

let insert_at (index : int) (el : 'a) (ml : 'a t) : 'a t =
  { items = Tools.insert_at index el ml.items;
    marks = Tools.insert_at index [] ml.items}

let insert_sublist_at (index : int) (ml1 : 'a t ) (ml2 : 'a t) : 'a t =
   { items = Tools.insert_sublist_at index ml1.items ml1.items;
     marks = Tools.insert_sublist_at index ml1.marks ml2.marks }

let extract (start : int) (stop : int) (ml : 'a t) : ('a t * 'a t) = 
  let items1, items2 = Tools.extract start stop ml.items in
  let marks1, marks2 = Tools.extract start stop ml.marks in
  ({ items = items1; marks = marks1}, {items = items2; marks = marks2})

let merge (ml1 : 'a t) (ml2 : 'a t) : 'a t =
  { items = ml1.items @ ml2.items;
    marks = ml1.marks @ ml2.marks}

let split (index : int) (ml : 'a t) : 'a t * 'a t= 
  extract index ((length ml) -1) ml 

let remove (start : int) (stop : int) (ml : 'a t) : 'a t = 
  fst (extract start stop ml)

let rev (ml : 'a t) : 'a t = 
  { items = List.rev ml.items;
    marks = List.rev ml.marks}


let list_update_nth (transfo : 'a -> 'a) (ml : 'a t) (n : int) : 'a t =
  {ml with items = Tools.list_update_nth transfo ml.items n}



