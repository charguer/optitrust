open Ast
open Transformations
open Translate_ast

(*
  transformation to swap the two first dimensions of an array
  name is used to name function copies
  assumption: x is a type variable that represents a multidimensional array type
  with >= 2 dimensions
  all variables of type x will be swapped
  assumption: x is not used in fun declarations
*)

(*
  swap the dimensions in the declaration of x
  swap all the accesses to arrays of type x
 *)
let rec swap_accesses (clog : out_channel) (x : typvar) (t : trm) : trm =
  match t.desc with
  | Trm_apps (f, tl) ->
     begin match f.desc with
     (* array accesses… *)
     | Trm_val (Val_prim (Prim_binop Binop_array_access))
       | Trm_val (Val_prim (Prim_binop Binop_array_get)) ->
        begin match tl with
        | [base; index] ->
           begin match base.desc with
           | Trm_apps (f', tl') ->
              begin match f'.desc with
              (* we look for two successive accesses to an array of type x *)
              | Trm_val (Val_prim (Prim_binop Binop_array_access))
                | Trm_val (Val_prim (Prim_binop Binop_array_get)) ->
                 begin match tl' with
                 | [base'; index'] ->
                    begin match base'.typ with
                    (* if we find such accesses, we swap the two indices *)
                    | Some {ty_desc = Typ_var x'; _} when x' = x ->
                       (* x might also be the type of arrays in indices… *)
                       let swapped_index = swap_accesses clog x index in
                       let swapped_index' = swap_accesses clog x index' in
                       trm_apps ~annot:t.annot ~loc:t.loc ~is_instr:t.is_instr
                         ~typ:t.typ f
                         [
                           trm_apps ~annot:base.annot ~loc:base.loc
                             ~is_instr:base.is_instr ~typ:base.typ f'
                             [
                               base';
                               swapped_index
                             ];
                           swapped_index'
                         ]
                    (*
                      otherwise we recursively call swap_accesses after removing
                      one dimension
                     *)
                    | _ ->
                       let swapped_l = List.map (swap_accesses clog x) tl in
                       trm_apps ~annot:t.annot ~loc:t.loc ~is_instr:t.is_instr
                         ~typ:t.typ f swapped_l
                    end
                 | _ ->
                    fail f'.loc ("swap_coordinates: array accesses should " ^
                                   "have 2 arguments");
                 end
              (*
                again, if we do not find two successive accesses, we
                recursively call swap_accesses
               *)
              | _ ->
                 let swapped_l = List.map (swap_accesses clog x) tl in
                 trm_apps ~annot:t.annot ~loc:t.loc ~is_instr:t.is_instr f
                   ~typ:t.typ swapped_l
              end
           (* again, … *)
           | _ ->
              let swapped_l = List.map (swap_accesses clog x) tl in
              trm_apps ~annot:t.annot ~loc:t.loc ~is_instr:t.is_instr f
                ~typ:t.typ swapped_l
           end
        | _ -> fail f.loc ("swap_coordinates: array accesses should have 2 " ^
                             "arguments");
        end
     (*
         for most other terms we only recursively call swap_accesses
         note: arrays of type x might appear in f now
      *)
     | _ ->
        let swapped_f = swap_accesses clog x f in
        let swapped_l = List.map (swap_accesses clog x) tl in
        trm_apps ~annot:t.annot ~loc:t.loc ~is_instr:t.is_instr ~typ:t.typ
          swapped_f swapped_l
     end
  (* declaration… *)
  | Trm_decl d ->
     begin match d with
     (* we look for the declaration of x *)
     | Def_typ (y, ty) when y = x ->
        let log : string =
          Printf.sprintf
           ("  - type\n%s\n" ^^
            "    is a multidimensional array type\n"
           )
           (typ_to_string ty)
        in
        write_log clog log;
        (*
          swap the two first coordinates of the multidimensional array type ty
         *)
        let rec swap_type (ty : typ) : typ =
          match ty.ty_desc with
          | Typ_array ({ty_desc = Typ_array (ty', s'); ty_annot; ty_attributes},
                       s) ->
             begin match ty'.ty_desc with
             (* we look for the 2 first coordinates… *)
             | Typ_array _ ->
                let t' =
                  swap_type {ty_desc = Typ_array (ty', s'); ty_annot;
                             ty_attributes}
                in
                {ty_desc = Typ_array (t', s); ty_annot = ty.ty_annot;
                 ty_attributes = ty.ty_attributes}
             (* once we reach them, we swap them *)
             | _ ->
                {ty_desc = Typ_array ({ty_desc = Typ_array (ty', s);
                                       ty_annot = ty.ty_annot;
                                       ty_attributes = ty.ty_attributes}, s');
                 ty_annot; ty_attributes}
             end
          | _ -> fail None ("swap_type: must be an array")
        in
        trm_decl ~annot:t.annot ~loc:t.loc ~is_instr:t.is_instr ~add:t.add
           (Def_typ (y, swap_type ty))
     (*
         all the interesting cases are covered now, we only have to do recursive
         calls
         var and fun decl first
      *)
     | _ -> trm_map (swap_accesses clog x) t
     end
  (*
     remaining cases: val, var, array, struct, if, seq, while, for, switch,
     abort, labelled
     inside values, array accesses may only happen in array sizes in types
     todo: currently ignored, is it reasonable to expect such things to happen?
   *)
  | _ -> trm_map (swap_accesses clog x) t

and swap (clog : out_channel) (name : var -> var) (x : typvar) (t : trm) : trm =
  (*
    3 things to change in t:
    - dimensions in the declaration of x
    - add a copy of each function taking an argument of type x and replace the
    function calls with these copies
    - array accesses on variables of type x
      includes initialisation (through loop)
      --> no initialisation by list for multidimensional arrays
   *)
  let ilsm = functions_with_arg_type x t in
  (* first add copies of the functions *)
  let t = insert_fun_copies name ilsm x t in
  (* then replace function calls *)
  let t = replace_fun_names name ilsm x t in
  (* finally adapt the declaration and accesses *)
  swap_accesses clog x t
