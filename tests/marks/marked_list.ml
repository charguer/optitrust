open Optitrust
open Ast

let ml_items = ["a";"b";"c";"d";"e";"f"] 

let ml = Mlist.of_list ml_items

let _ = Tools.printf "Generated marked list is correct: %b\n" (ml.marks = List.init (List.length l + 1) (fun _i -> []) && ml.items = ml_items)

let insert_at_ml_1 = Mlist.insert_at 0 "m" ml

let _ = Tools.printf "Generated marked list is correct: %b\n" (ml.marks = List.init (List.length l + 1) (fun _i -> []) && ml.items = ml_items)

let insert_at_ml_2 = Mlist.insert_at 2 "n" ml

let insert_at_ml_2 = Mlist.insert_at 6 "p" ml

let insert_sublist_at_ml_1 = Mlist.sublist_at 0 ["m";"n";"p"] ml

let insert_sublist_at_ml_2 = Mlist.sublist_at 2 ["m";"n";"p"] ml

let insert_sublist_at_ml_1 = Mlist.sublist_at 6 ["m";"n";"p"] ml

let replace_at_ml_1 = Mlist.replace_at 0 "z" ml

let replace_at_ml_2 = Mlist.replace_at 2 "z" ml

let replace_at_ml_3 = Mlist.replace_at 5 "z" ml

(****************************TESTS FOR MARKS *************************)
let int_marks = [0;1;2;3;4;5;6]

let insert_mark_at_ml = List.fold_left (fun acc ind -> Mlist.insert_mark_at ind (string_of_int ind) acc) ml int_marks

let remove_mark_at_ml = List.fold_left (fun acc ind -> Mlist.remove_mark (string_of_int ind) acc) insert_mark_at_ml int_marks 

let extract_m1_ml, extract_s1_ml = Mlist.extract 0 2 inser_mark_at_ml 

let extract_m2_ml, extract_s2_ml = Mlist.extract 2 4 inser_mark_at_ml 

let extract_m3_ml, extract_s3_ml = Mlist.extract 3 5 inser_mark_at_ml 

let merge_m1_ml = Mlist.merge extract_s1_ml extract_m1_ml 

let merge_m2_ml = Mlist.merge extract_m2_ml extract_s2_ml (* TODO: Fix me *)

let merge_m3_ml = Mlist.merge extract_m3_ml extract_s3_ml

let split_l1_ml, split_r1_ml = Mlist.split_temp 0 ml

let split_l2_ml, split_r2_ml = Mlist.split_temp 2 ml

let split_l3_ml, split_r3_ml = Mlist.split_temp (Mlist.length ml - 1) ml

let merge_m1_ml = Mlist.merge split_l1_ml split_r1_ml 

let merge_m1_ml = Mlist.merge split_l2_ml split_r2_ml 

let merge_m1_ml = Mlist.merge split_l3_ml split_r3_ml 

let rev_ml = Mlist.rev ml

(* TODO: Finish this unit test *)

(* TODO: Arthur design a print function *)






