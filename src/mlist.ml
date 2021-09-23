type mark = Mark.t

type 'a t = 
  { items : 'a list;
   marks : (mark list) list  }

let length (ml : 'a t) : int = 
  List.length ml.items

let of_list (l : 'a list) : 'a t = 
  { items = l; 
    marks = List.init (List.length l + 1) (fun _i -> [])}

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

let foldi (acc_f : int -> 'b -> 'a -> 'b) (acc : 'b) (ml : 'a t) : 'b =
  Tools.foldi acc_f acc ml.items

let insert_at (index : int) (el : 'a) (ml : 'a t) : 'a t =
  let sz = length ml in
  { items = Tools.insert_at index el ml.items;
    marks = if index = sz then ml.marks @ [[]] else Tools.insert_at index [] ml.marks}

let insert_sublist_at (index : int) (sl : 'a list ) (ml : 'a t) : 'a t =
   let sz = length ml in
   let empty_marks = List.map (fun _ -> []) sl in
   { items = Tools.insert_sublist_at index sl ml.items;
     marks = if index = sz then ml.marks @ empty_marks else Tools.insert_sublist_at index empty_marks ml.marks }

let replace_at (index : int) (el : 'a) (ml : 'a t) : 'a t =
  {ml with items = Tools.replace_at index el ml.items}

let insert_mark_at (index : int) (m : mark) (ml : 'a t) : 'a t =
  let m1 = List.nth ml.marks index in 
  {ml with marks = Tools.replace_at index (m :: m1) ml.marks}

let remove_mark (m : mark)(ml : 'a t) : 'a t =
  let new_marks = List.map (fun ms -> List.filter (fun x -> x <> m ) ms) ml.marks in
  {ml with marks = new_marks}

let extract (start : int) (stop : int) (ml : 'a t) : ('a t * 'a t) = 
  let items1, items2 = Tools.extract start stop ml.items in
  let temp_marks1, temp_marks2 = Tools.extract start (stop+1) ml.marks in
  let (mtg1, mtg2) = Tools.get_first_last temp_marks2 in
  let merged_marks = mtg1 @ mtg2 in
  let marks2 = temp_marks2 in
  let marks1 = Tools.insert_at start merged_marks temp_marks1 in
  ({ items = items1; marks = marks1}, {items = items2; marks = marks2})

let merge (ml1 : 'a t) (ml2 : 'a t) : 'a t =
  let marks1, tmp_marks1 =  Tools.extract (List.length ml1.marks) (List.length ml1.marks + 1) ml1.marks in
  let marks2, tmp_marks2 = Tools.extract 0 0 ml2.marks in
  let merged_marks = tmp_marks1 @ tmp_marks2 in
  { items = ml1.items @ ml2.items;
    marks = marks1 @ merged_marks @ marks2}

let split (index : int) (ml : 'a t) : 'a t * 'a t= 
  extract index ((length ml) -1) ml 

let remove (start : int) (stop : int) (ml : 'a t) : 'a t = 
  fst (extract start stop ml)

let rev (ml : 'a t) : 'a t = 
  { items = List.rev ml.items;
    marks = List.rev ml.marks}

let list_update_nth (transfo : 'a -> 'a) (ml : 'a t) (n : int) : 'a t =
  {ml with items = Tools.list_update_nth transfo ml.items n}

let marks_to_string (ml : 'a t) : string =
  Tools.list_to_string (List.map (fun ml1 -> Tools.list_to_string ml1) ml.marks)