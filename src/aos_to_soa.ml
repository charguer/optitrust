open Ast
open Transformations
open Ast_to_c

(*
  Array of Structures to Structure of Arrays:²²²
  if s is struct {t1 field1; …; tm fieldm} and x is s[n], transforms x into s'
  where s' is struct {t1 field1[n]; …; tm fieldm[n]}
  arguments:
    - x: type variable representing the array type
    - name: to name function copies
  assumptions:
    - x is not used in function definitions, but only in var declarations
 *)

let swap_accesses (clog : out_channel) (x : typvar) (t : trm) : trm =
  let rec aux (global_trm : trm) (t :trm) : trm =
    match t.desc with
    (* declarations *)
    | Trm_decl d ->
       begin match d with
       (* we have to change the declaration of x *)
       | Def_typ (y, ty) when y = x ->
        let log : string =
          Printf.sprintf
           ("  - type\n%s\n" ^^
            "    is an array type\n"
           )
           (typ_to_string ty)
        in
        write_log clog log;
          (*
            ty must be an array type over a struct type denoted by a type var
           *)
          begin match ty.ty_desc with
          | Typ_array ({ty_desc = Typ_var y; _}, s) ->
             begin match aliased_type y global_trm with
             | None ->
                fail t.loc "swap_accesses: cannot find underlying struct type"
             | Some ty' ->
                let log : string =
                  Printf.sprintf
                    ("  - type\n%s\n" ^^
                       "    is a struct type\n"
                    )
                    (typ_to_string ty')
                in
                write_log clog log;
                begin match ty'.ty_desc with
                | Typ_struct (l,m, n) ->
                   let m =
                     Field_map.map
                       (fun ty'' ->
                         typ_array ~ty_attributes:ty.ty_attributes ty'' s) m
                   in
                   trm_decl (Def_typ (x, typ_struct l m n))
                | _ ->
                   fail t.loc "swap_accesses: expected underlying struct type"
                end
             end
          | _ -> fail t.loc "swap_accesses: expected array type declaration"
          end
       (*
         other cases: type declarations (not x), fun declarations, var
         declarations (not of type x)
         arrays of type x are heap allocated
        *)
       | _ -> trm_map (aux global_trm) t
       end
    (* accesses: y[i].f becomes (y.f)[i] *)
    | Trm_apps (f, [base]) ->
       begin match f.desc with
       | Trm_val (Val_prim (Prim_unop (Unop_struct_access _)))
         | Trm_val (Val_prim (Prim_unop (Unop_struct_get _))) ->
          begin match base.desc with
          | Trm_apps (f', [base'; index]) ->
             begin match f'.desc with
             | Trm_val (Val_prim (Prim_binop Binop_array_access))
               | Trm_val (Val_prim (Prim_binop Binop_array_get)) ->
                (*
                  swap accesses only if the type of base' is x (or x* in case of
                  an access on a heap allocated variable)
                 *)
                begin match base'.typ with
                | Some {ty_desc = Typ_var y; _} when y = x ->
                   (* x might appear both in index and in base' *)
                   let base' = aux global_trm base' in
                   let index = aux global_trm index in
                   (* keep outer annotations *)
                   trm_apps ~annot:t.annot ~loc:t.loc ~is_instr:t.is_instr
                     ~add:t.add ~typ:t.typ f' [trm_apps f [base']; index]
                | Some {ty_desc = Typ_ptr {ty_desc = Typ_var y; _}; _}
                     when y = x ->
                   (* x might appear both in index and in base' *)
                   let base' = aux global_trm base' in
                   let index = aux global_trm index in
                   (* keep outer annotations *)
                   trm_apps ~annot:t.annot ~loc:t.loc ~is_instr:t.is_instr
                     ~add:t.add ~typ:t.typ f' [trm_apps f [base']; index]
                | _ -> trm_map (aux global_trm) t
                end
             | _ -> trm_map (aux global_trm) t
             end
          | _ -> trm_map (aux global_trm) t
          end
       | _ -> trm_map (aux global_trm) t
       end
    (* other cases: recursive call *)
    | _ -> trm_map (aux global_trm) t
  in
  aux t t

let aos_to_soa (clog : out_channel) (name : var -> var) (x : typvar)
  (t : trm) : trm =
  (*
    changes:
    - declaration of x
    - add a copy of each function taking an argument of type x and replace the
    function calls with these copies
    - accesses on variables of type x
   *)
  let ilsm = functions_with_arg_type x t in
  (* first add copies of the functions *)
  let t = insert_fun_copies name ilsm x t in
  (* then replace function calls *)
  let t = replace_fun_names name ilsm x t in
  (* finally adapt the declaration and accesses *)
  swap_accesses clog x t
