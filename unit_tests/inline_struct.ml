open ScriptTools

(*
    TODO: Implmeent pretty struct into struct inlining
*)


let _ = 
    run 
    ( fun _ -> 
        set_init_source"inline_struct.cpp";
        inline_struct [cType ~name:"obj"()] "vect" ~struct_fields:["pos"];
        (* (* internal not in scriptTools.mli *) inline_one_struct_in_struct ~name:"obj" "pos";
        inline_struct_in_struct ~name:"obj" ["pos"];
        inline_struct_in_struct ~name:"obj" ["pos" ;"speed"]; (* List.fold *)*)
        dump()
    )

(* bind F7 to
   CUR=inline_struct
    make install && cd unit_tests && make ${CUR}.out && meld ${CUR}.cpp ${CUR}.out *)
(*
  obj a = {a, {0,0}, s}   let i be the index of where "pos" was in "obj"
  obj a = {a, 0,0, s}    obtained by inlining the sublist at index i into the main list
    let rec inline_sublist_in_list i xs =
       match i, xs with
       | O, e::xs -> 
           begin match e with
            | trm_struct sublist -> sublist @ xs
            | trm_var x -> ?? (* not supported is fine *)
            end
       | ..
       | O, [] -> error
 
  obj p = {0, 0 }
  obj a = { 1, p, s}

 inlining when the sublist is not visible
  obj a' = { 1, p.x, p.y, s }



go over places such that   trm_struct el when t.typ = "obj" ->
  call inline_sublist_in_list



  any operation on a record involves:
  1) typedef        typedef struct { .. } post
  2) accesss_get    a.pos
  3) access_set     a.pos
  4) initialization    { 0, 0 }
  5) new               new obj //probably no change needed
*)