open Optitrust
open Ast
open Trm
open Target
open Prelude

(** [apply_tiling block_size nb_blocks x index_dim t]: changes all the
    occurences of the array to the tiled form. [block_size] - size of the blocks
    used for tiling [nb_blocks] - number of blocks used for tilling [x] - var
    representing the array for which we want to modify the accesses [index_dim]
    \- index of the tiled array's dimension [t] - ast node located in the same
    level or deeper as the array declaration

    assumptions:
    - x is not used in function definitions, but only in var declarations
    - the array_size is block_size * nb_blocks *)
let rec apply_tiling (block_size : trm) (nb_blocks : trm) (x : typvar)
    (index_dim : int) (t : trm) : trm =
  let aux = apply_tiling block_size nb_blocks x index_dim in
  match Matrix_trm.access_inv t with
  | Some (base, dims, indices) -> (
      match trm_var_inv base with
      | Some y ->
          if var_eq y x then
            let dimfront, current_dim, dimback =
              List.get_item_and_its_relatives index_dim dims
            in
            let ifront, current_index, iback =
              List.get_item_and_its_relatives index_dim indices
            in
            Matrix_trm.access (trm_var x)
              (dimfront @ [ nb_blocks; block_size ] @ dimback)
              (ifront
              @ [
                  trm_trunc_div_int current_index block_size;
                  trm_trunc_mod_int current_index block_size;
                ]
              @ iback)
          else trm_map aux t
      | _ -> trm_map aux t)
  | _ -> trm_map aux t

(** [tile_at block_name block_size index t]: transform an array declaration from
    a normal shape into a tiled one, then call apply_tiling to change all the
    array occurrences into the correct form. [nb_blocks] - optional, use to
    indicate the nb of tiled blocks [block_size] - the size of the tile,

    [index] - the index of the instruction inside the sequence, [t] - ast of the
    outer sequence containing the array declaration.

    Ex: t[i] -> t[i/B][i%B] *)
let tile_at ?(nb_blocks : trm option) ~(block_size : trm) ~(index_dim : int)
    (index : int) (t : trm) : trm =
  let tl, result = trm_inv trm_seq_inv t in
  let lfront, d, lback = Mlist.get_item_and_its_relatives index tl in
  let array_var, typ, alloc_trm =
    trm_inv ~error:"Matrix_basic.tile_at : Expected an array declaration"
      trm_let_inv d
  in
  (* Comment faire si on a pas de const ? Doit-on gérer le cas ?  *)
  let typ_alloc, trms, init =
    trm_inv ~error:"Matrix_basic.tile_at : Expected an array declaration"
      Matrix_trm.alloc_inv alloc_trm
  in
  if List.length trms <= index_dim then
    trm_fail t
      "Matrix_basic.tile_at: index_dim is greater than the number of dimensions"
  else
    let dimfront, current_dim, dimback =
      List.get_item_and_its_relatives index_dim trms
    in

    let lfront, nb_blocks =
      match nb_blocks with
      | Some x -> (lfront, x)
      | None ->
          let new_block_var = name_to_var (array_var.name ^ "_blocks") in
          let new_block =
            trm_let (new_block_var, typ_int)
              (trm_trunc_div_int
                 (trm_add_int current_dim (trm_sub_int block_size (trm_int 1)))
                 block_size)
          in
          (Mlist.push_back new_block lfront, trm_var new_block_var)
    in
    let new_dims = dimfront @ [ nb_blocks; block_size ] @ dimback in
    let new_d =
      trm_let (array_var, typ)
        (Matrix_trm.alloc ~zero_init:init typ_alloc new_dims)
    in
    let lback =
      Mlist.map
        (fun t -> (apply_tiling block_size nb_blocks array_var index_dim) t)
        lback
    in
    let new_tl = Mlist.push_back new_d lfront in
    let new_tl = Mlist.merge new_tl lback in

    trm_seq ?result new_tl

(** [tile ~block_type block_size tg]: expects the target [tg] to point at an
    array declaration. Then it takes that declaration and transforms it into a
    tiled array. All the accesses of the targeted array are handled as well.
    [block_size] - size of the block of tiles. [index_dim] - Index of the
    dimension to tile [nb_blocks] - optional, numbers of blocks in the tiled
    array Note : If nb_blocks is not given, and that the array size N is not
    divsible by block

    _size, then nb_blocks is computed as the upper part of N / block_size, this
    will extend the array and that might incorrect if this array is used in
    other functions Example : float * a = malloc(MSIZE1(10)) -> float * a =
    malloc(MSIZE2(10/2,2)) *)
let tile ?(nb_blocks : trm option) ~(block_size : trm) ~(index_dim : int)
    (tg : target) : unit =
  apply_at_target_paths_in_seq (tile_at ~block_size ?nb_blocks ~index_dim) tg

let _ =
  Run.script_cpp (fun _ ->
      !!tile ~block_size:(trm_int 2) ~index_dim:0
        [ cFunDef "test"; cVarDef "a" ];
      !!tile ~block_size:(trm_int 2) ~index_dim:0
        [ cFunDef "main"; cVarDefs [ "a"; "b" ] ];
      !!tile ~block_size:(trm_int 2) ~index_dim:2
        [ cFunDef "main"; cVarDef "c" ];
      !!tile ~block_size:(trm_int 2) ~index_dim:0
        [ cFunDef "main2"; cVarDefs [ "a"; "b"; "c" ] ];
      !!tile ~block_size:(trm_int 2) ~index_dim:0
        [ cFunDef "main3"; cVarDef "a" ];
      let block_size, _ = find_var "block_size" [ cFunDef "main4" ] in
      !!tile ~block_size:(trm_var block_size)
        ~nb_blocks:(trm_find_var "nb_blocks" [ cFunDef "main4" ])
        ~index_dim:0
        [ cFunDef "main4"; cVarDef "a" ];
      !!tile
        ~block_size:(trm_find_var "block_size" [ cFunDef "main5" ])
        ~index_dim:0
        [ cFunDef "main5"; cVarDef "a" ])
