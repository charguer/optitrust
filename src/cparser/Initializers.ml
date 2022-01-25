open AST
open Archi
open BinInt
open BinNums
open Cop
open Coqlib
open Csyntax
open Ctypes
open Datatypes
open Errors
open Floats
open Integers
open List0
open Maps
open Memdata
open Memory
open Values

(** val do_cast : coq_val -> coq_type -> coq_type -> coq_val res **)

let do_cast v t1 t2 =
  match sem_cast v t1 t2 Mem.empty with
  | Some v' -> OK v'
  | None ->
    Error
      (msg
        ('u'::('n'::('d'::('e'::('f'::('i'::('n'::('e'::('d'::(' '::('c'::('a'::('s'::('t'::[])))))))))))))))

(** val lookup_composite : composite_env -> ident -> composite res **)

let lookup_composite ce id =
  match PTree.get id ce with
  | Some co -> OK co
  | None ->
    Error ((MSG
      ('U'::('n'::('d'::('e'::('f'::('i'::('n'::('e'::('d'::(' '::('s'::('t'::('r'::('u'::('c'::('t'::(' '::('o'::('r'::(' '::('u'::('n'::('i'::('o'::('n'::(' '::[]))))))))))))))))))))))))))) :: ((CTX
      id) :: []))

(** val constval : composite_env -> expr -> coq_val res **)

let rec constval ce = function
| Eval (v, _) ->
  (match v with
   | Vundef ->
     Error
       (msg
         ('i'::('l'::('l'::('e'::('g'::('a'::('l'::(' '::('c'::('o'::('n'::('s'::('t'::('a'::('n'::('t'::[])))))))))))))))))
   | Vptr (_, _) ->
     Error
       (msg
         ('i'::('l'::('l'::('e'::('g'::('a'::('l'::(' '::('c'::('o'::('n'::('s'::('t'::('a'::('n'::('t'::[])))))))))))))))))
   | _ -> OK v)
| Evar (x, _) -> OK (Vptr (x, Ptrofs.zero))
| Efield (l, f, _) ->
  (match match typeof l with
         | Tstruct (id, _) ->
           (match lookup_composite ce id with
            | OK x -> field_offset ce f x.co_members
            | Error msg0 -> Error msg0)
         | Tunion (id, _) ->
           (match lookup_composite ce id with
            | OK x -> union_field_offset ce f x.co_members
            | Error msg0 -> Error msg0)
         | _ ->
           Error
             (msg
               ('i'::('l'::('l'::('-'::('t'::('y'::('p'::('e'::('d'::(' '::('f'::('i'::('e'::('l'::('d'::(' '::('a'::('c'::('c'::('e'::('s'::('s'::[]))))))))))))))))))))))) with
   | OK p ->
     let (x, y) = p in
     (match constval ce l with
      | OK x0 ->
        (match y with
         | Full ->
           OK
             (if ptr64
              then Val.addl x0 (Vlong (Int64.repr x))
              else Val.add x0 (Vint (Int.repr x)))
         | Bits (_, _, _, _) ->
           Error
             (msg
               ('t'::('a'::('k'::('i'::('n'::('g'::(' '::('t'::('h'::('e'::(' '::('a'::('d'::('d'::('r'::('e'::('s'::('s'::(' '::('o'::('f'::(' '::('a'::(' '::('b'::('i'::('t'::('f'::('i'::('e'::('l'::('d'::[]))))))))))))))))))))))))))))))))))
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Evalof (l, ty) ->
  (match access_mode ty with
   | By_value _ ->
     Error
       (msg
         ('d'::('e'::('r'::('e'::('f'::('e'::('r'::('e'::('n'::('c'::('i'::('n'::('g'::(' '::('o'::('f'::(' '::('a'::('n'::(' '::('l'::('-'::('v'::('a'::('l'::('u'::('e'::[]))))))))))))))))))))))))))))
   | By_nothing ->
     Error
       (msg
         ('d'::('e'::('r'::('e'::('f'::('e'::('r'::('e'::('n'::('c'::('i'::('n'::('g'::(' '::('o'::('f'::(' '::('a'::('n'::(' '::('l'::('-'::('v'::('a'::('l'::('u'::('e'::[]))))))))))))))))))))))))))))
   | _ -> constval ce l)
| Ederef (r, _) -> constval ce r
| Eaddrof (l, _) -> constval ce l
| Eunop (op, r1, _) ->
  (match constval ce r1 with
   | OK x ->
     (match sem_unary_operation op x (typeof r1) Mem.empty with
      | Some v -> OK v
      | None ->
        Error
          (msg
            ('u'::('n'::('d'::('e'::('f'::('i'::('n'::('e'::('d'::(' '::('u'::('n'::('a'::('r'::('y'::(' '::('o'::('p'::('e'::('r'::('a'::('t'::('i'::('o'::('n'::[])))))))))))))))))))))))))))
   | Error msg0 -> Error msg0)
| Ebinop (op, r1, r2, _) ->
  (match constval ce r1 with
   | OK x ->
     (match constval ce r2 with
      | OK x0 ->
        (match sem_binary_operation ce op x (typeof r1) x0 (typeof r2)
                 Mem.empty with
         | Some v -> OK v
         | None ->
           Error
             (msg
               ('u'::('n'::('d'::('e'::('f'::('i'::('n'::('e'::('d'::(' '::('b'::('i'::('n'::('a'::('r'::('y'::(' '::('o'::('p'::('e'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Ecast (r, ty) ->
  (match constval ce r with
   | OK x -> do_cast x (typeof r) ty
   | Error msg0 -> Error msg0)
| Eseqand (r1, r2, _) ->
  (match constval ce r1 with
   | OK x ->
     (match constval ce r2 with
      | OK x0 ->
        (match bool_val x (typeof r1) Mem.empty with
         | Some b ->
           if b then do_cast x0 (typeof r2) type_bool else OK (Vint Int.zero)
         | None ->
           Error
             (msg
               ('u'::('n'::('d'::('e'::('f'::('i'::('n'::('e'::('d'::(' '::('&'::('&'::(' '::('o'::('p'::('e'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Eseqor (r1, r2, _) ->
  (match constval ce r1 with
   | OK x ->
     (match constval ce r2 with
      | OK x0 ->
        (match bool_val x (typeof r1) Mem.empty with
         | Some b ->
           if b then OK (Vint Int.one) else do_cast x0 (typeof r2) type_bool
         | None ->
           Error
             (msg
               ('u'::('n'::('d'::('e'::('f'::('i'::('n'::('e'::('d'::(' '::('|'::('|'::(' '::('o'::('p'::('e'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Econdition (r1, r2, r3, ty) ->
  (match constval ce r1 with
   | OK x ->
     (match constval ce r2 with
      | OK x0 ->
        (match constval ce r3 with
         | OK x1 ->
           (match bool_val x (typeof r1) Mem.empty with
            | Some b ->
              if b
              then do_cast x0 (typeof r2) ty
              else do_cast x1 (typeof r3) ty
            | None ->
              Error
                (msg
                  ('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::(' '::('i'::('s'::(' '::('u'::('n'::('d'::('e'::('f'::('i'::('n'::('e'::('d'::[]))))))))))))))))))))))))
         | Error msg0 -> Error msg0)
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Esizeof (ty1, _) -> OK (coq_Vptrofs (Ptrofs.repr (sizeof ce ty1)))
| Ealignof (ty1, _) -> OK (coq_Vptrofs (Ptrofs.repr (alignof ce ty1)))
| Ecomma (r1, r2, _) ->
  (match constval ce r1 with
   | OK _ -> constval ce r2
   | Error msg0 -> Error msg0)
| Eparen (r, tycast, _) ->
  (match constval ce r with
   | OK x -> do_cast x (typeof r) tycast
   | Error msg0 -> Error msg0)
| _ ->
  Error
    (msg
      ('n'::('o'::('t'::(' '::('a'::(' '::('c'::('o'::('m'::('p'::('i'::('l'::('e'::('-'::('t'::('i'::('m'::('e'::(' '::('c'::('o'::('n'::('s'::('t'::('a'::('n'::('t'::[]))))))))))))))))))))))))))))

(** val constval_cast : composite_env -> expr -> coq_type -> coq_val res **)

let constval_cast ce a ty =
  match constval ce a with
  | OK x -> do_cast x (typeof a) ty
  | Error msg0 -> Error msg0

type state = { init : init_data list; curr : coq_Z; total_size : coq_Z }

(** val initial_state : coq_Z -> state **)

let initial_state sz =
  { init = []; curr = Z0; total_size = sz }

(** val int_of_byte : Byte.int -> Int.int **)

let int_of_byte b =
  Int.repr (Byte.unsigned b)

(** val coq_Init_byte : Byte.int -> init_data **)

let coq_Init_byte b =
  Init_int8 (int_of_byte b)

(** val add_rev_bytes : Byte.int list -> init_data list -> init_data list **)

let rec add_rev_bytes l il =
  match l with
  | [] -> il
  | b :: l0 -> add_rev_bytes l0 ((coq_Init_byte b) :: il)

(** val add_zeros : coq_Z -> init_data list -> init_data list **)

let add_zeros n il =
  Z.iter n (fun l -> (Init_int8 Int.zero) :: l) il

(** val normalize : init_data list -> coq_Z -> init_data list res **)

let rec normalize il depth =
  if zle depth Z0
  then OK il
  else (match il with
        | [] ->
          Error
            (msg
              ('n'::('o'::('r'::('m'::('a'::('l'::('i'::('z'::('e'::(':'::(' '::('e'::('m'::('p'::('t'::('y'::(' '::('l'::('i'::('s'::('t'::[]))))))))))))))))))))))
        | i :: il0 ->
          (match i with
           | Init_int8 n ->
             (match normalize il0 (Z.sub depth (Zpos Coq_xH)) with
              | OK x -> OK ((Init_int8 n) :: x)
              | Error msg0 -> Error msg0)
           | Init_int16 n ->
             (match normalize il0 (Z.sub depth (Zpos (Coq_xO Coq_xH))) with
              | OK x ->
                OK (add_rev_bytes (encode_int (S (S O)) (Int.unsigned n)) x)
              | Error msg0 -> Error msg0)
           | Init_int32 n ->
             (match normalize il0
                      (Z.sub depth (Zpos (Coq_xO (Coq_xO Coq_xH)))) with
              | OK x ->
                OK
                  (add_rev_bytes
                    (encode_int (S (S (S (S O)))) (Int.unsigned n)) x)
              | Error msg0 -> Error msg0)
           | Init_int64 n ->
             (match normalize il0
                      (Z.sub depth (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH))))) with
              | OK x ->
                OK
                  (add_rev_bytes
                    (encode_int (S (S (S (S (S (S (S (S O))))))))
                      (Int64.unsigned n)) x)
              | Error msg0 -> Error msg0)
           | Init_float32 f ->
             (match normalize il0
                      (Z.sub depth (Zpos (Coq_xO (Coq_xO Coq_xH)))) with
              | OK x ->
                OK
                  (add_rev_bytes
                    (encode_int (S (S (S (S O))))
                      (Int.unsigned (Float32.to_bits f))) x)
              | Error msg0 -> Error msg0)
           | Init_float64 f ->
             (match normalize il0
                      (Z.sub depth (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH))))) with
              | OK x ->
                OK
                  (add_rev_bytes
                    (encode_int (S (S (S (S (S (S (S (S O))))))))
                      (Int64.unsigned (Float.to_bits f))) x)
              | Error msg0 -> Error msg0)
           | Init_space n ->
             let n0 = Z.max Z0 n in
             if zle n0 depth
             then (match normalize il0 (Z.sub depth n0) with
                   | OK x -> OK (add_zeros n0 x)
                   | Error msg0 -> Error msg0)
             else OK (add_zeros depth ((Init_space (Z.sub n0 depth)) :: il0))
           | Init_addrof (_, _) ->
             Error
               (msg
                 ('n'::('o'::('r'::('m'::('a'::('l'::('i'::('z'::('e'::(':'::(' '::('I'::('n'::('i'::('t'::('_'::('a'::('d'::('d'::('r'::('o'::('f'::[])))))))))))))))))))))))))

(** val decompose_rec :
    Byte.int list -> init_data list -> coq_Z -> (Byte.int list * init_data
    list) res **)

let rec decompose_rec accu il depth =
  if zle depth Z0
  then OK (accu, il)
  else (match il with
        | [] ->
          Error
            (msg
              ('d'::('e'::('c'::('o'::('m'::('p'::('o'::('s'::('e'::(':'::(' '::('w'::('r'::('o'::('n'::('g'::(' '::('s'::('h'::('a'::('p'::('e'::[])))))))))))))))))))))))
        | i :: il0 ->
          (match i with
           | Init_int8 n ->
             decompose_rec ((Byte.repr (Int.unsigned n)) :: accu) il0
               (Z.sub depth (Zpos Coq_xH))
           | _ ->
             Error
               (msg
                 ('d'::('e'::('c'::('o'::('m'::('p'::('o'::('s'::('e'::(':'::(' '::('w'::('r'::('o'::('n'::('g'::(' '::('s'::('h'::('a'::('p'::('e'::[])))))))))))))))))))))))))

(** val decompose :
    init_data list -> coq_Z -> (Byte.int list * init_data list) res **)

let decompose il depth =
  decompose_rec [] il depth

(** val trisection :
    init_data list -> coq_Z -> coq_Z -> ((Byte.int list * Byte.int
    list) * init_data list) res **)

let trisection il depth sz =
  match normalize il (Z.add depth sz) with
  | OK x ->
    (match decompose x depth with
     | OK p ->
       let (x0, y) = p in
       (match decompose y sz with
        | OK p0 -> let (x1, y0) = p0 in OK ((x0, x1), y0)
        | Error msg0 -> Error msg0)
     | Error msg0 -> Error msg0)
  | Error msg0 -> Error msg0

(** val pad_to : state -> coq_Z -> state **)

let pad_to s pos =
  if zle pos s.curr
  then s
  else { init = ((Init_space (Z.sub pos s.curr)) :: s.init); curr = pos;
         total_size = s.total_size }

(** val store_data : state -> coq_Z -> init_data -> state res **)

let store_data s pos i =
  let sz = init_data_size i in
  if (&&) ((fun x -> x) (zle Z0 pos))
       ((fun x -> x) (zle (Z.add pos sz) s.total_size))
  then if zle s.curr pos
       then OK { init =
              (i :: (if zlt s.curr pos
                     then (Init_space (Z.sub pos s.curr)) :: s.init
                     else s.init)); curr = (Z.add pos sz); total_size =
              s.total_size }
       else let s' = pad_to s (Z.add pos sz) in
            (match trisection s'.init (Z.sub s'.curr (Z.add pos sz)) sz with
             | OK x ->
               let (p, il2) = x in
               let (bytes1, _) = p in
               OK { init = (add_rev_bytes bytes1 (i :: il2)); curr = s'.curr;
               total_size = s'.total_size }
             | Error msg0 -> Error msg0)
  else assertion_failed

(** val init_data_for_carrier : intsize -> Int.int -> init_data **)

let init_data_for_carrier isz n =
  match isz with
  | I16 -> Init_int16 n
  | I32 -> Init_int32 n
  | _ -> Init_int8 n

(** val store_int : state -> coq_Z -> intsize -> Int.int -> state res **)

let store_int s pos isz n =
  store_data s pos (init_data_for_carrier isz n)

(** val load_int : state -> coq_Z -> intsize -> Int.int res **)

let load_int s pos isz =
  let chunk = chunk_for_carrier isz in
  let sz = size_chunk chunk in
  if (&&) ((fun x -> x) (zle Z0 pos))
       ((fun x -> x) (zle (Z.add pos sz) s.total_size))
  then let s' = pad_to s (Z.add pos sz) in
       (match trisection s'.init (Z.sub s'.curr (Z.add pos sz)) sz with
        | OK x ->
          let (p, _) = x in
          let (_, bytes2) = p in OK (Int.repr (decode_int bytes2))
        | Error msg0 -> Error msg0)
  else assertion_failed

(** val init_data_list_of_state : state -> init_data list res **)

let init_data_list_of_state s =
  if zle s.curr s.total_size
  then let s' = pad_to s s.total_size in OK (rev' s'.init)
  else assertion_failed

type coq_initializer =
| Init_single of expr
| Init_array of initializer_list
| Init_struct of initializer_list
| Init_union of ident * coq_initializer
and initializer_list =
| Init_nil
| Init_cons of coq_initializer * initializer_list

(** val length_initializer_list : initializer_list -> coq_Z **)

let length_initializer_list il =
  let rec length accu = function
  | Init_nil -> accu
  | Init_cons (_, il1) -> length (Z.succ accu) il1
  in length Z0 il

(** val transl_init_single :
    composite_env -> coq_type -> expr -> init_data res **)

let transl_init_single ce ty a =
  match constval_cast ce a ty with
  | OK x ->
    (match x with
     | Vundef ->
       Error
         (msg
           ('u'::('n'::('d'::('e'::('f'::('i'::('n'::('e'::('d'::(' '::('o'::('p'::('e'::('r'::('a'::('t'::('i'::('o'::('n'::(' '::('i'::('n'::(' '::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::('r'::[])))))))))))))))))))))))))))))))))))
     | Vint n ->
       (match ty with
        | Tint (i, _, _) ->
          (match i with
           | I16 -> OK (Init_int16 n)
           | I32 -> OK (Init_int32 n)
           | _ -> OK (Init_int8 n))
        | Tpointer (_, _) ->
          if negb ptr64 then OK (Init_int32 n) else assertion_failed
        | _ ->
          Error
            (msg
              ('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::(' '::('i'::('n'::(' '::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::('r'::[]))))))))))))))))))))))))))))))
     | Vlong n ->
       (match ty with
        | Tlong (_, _) -> OK (Init_int64 n)
        | Tpointer (_, _) ->
          if ptr64 then OK (Init_int64 n) else assertion_failed
        | _ ->
          Error
            (msg
              ('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::(' '::('i'::('n'::(' '::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::('r'::[]))))))))))))))))))))))))))))))
     | Vfloat f ->
       (match ty with
        | Tfloat (f0, _) ->
          (match f0 with
           | F32 ->
             Error
               (msg
                 ('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::(' '::('i'::('n'::(' '::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::('r'::[])))))))))))))))))))))))))))))
           | F64 -> OK (Init_float64 f))
        | _ ->
          Error
            (msg
              ('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::(' '::('i'::('n'::(' '::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::('r'::[]))))))))))))))))))))))))))))))
     | Vsingle f ->
       (match ty with
        | Tfloat (f0, _) ->
          (match f0 with
           | F32 -> OK (Init_float32 f)
           | F64 ->
             Error
               (msg
                 ('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::(' '::('i'::('n'::(' '::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::('r'::[]))))))))))))))))))))))))))))))
        | _ ->
          Error
            (msg
              ('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::(' '::('i'::('n'::(' '::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::('r'::[]))))))))))))))))))))))))))))))
     | Vptr (id, ofs) ->
       (match ty with
        | Tint (i, _, _) ->
          (match i with
           | I32 ->
             if negb ptr64
             then OK (Init_addrof (id, ofs))
             else assertion_failed
           | _ ->
             Error
               (msg
                 ('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::(' '::('i'::('n'::(' '::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::('r'::[]))))))))))))))))))))))))))))))
        | Tlong (_, _) ->
          if ptr64 then OK (Init_addrof (id, ofs)) else assertion_failed
        | Tpointer (_, _) -> OK (Init_addrof (id, ofs))
        | _ ->
          Error
            (msg
              ('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::(' '::('i'::('n'::(' '::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::('r'::[])))))))))))))))))))))))))))))))
  | Error msg0 -> Error msg0

(** val transl_init_bitfield :
    composite_env -> state -> coq_type -> intsize -> coq_Z -> coq_Z ->
    coq_initializer -> coq_Z -> state res **)

let transl_init_bitfield ce s ty sz p w i pos =
  match i with
  | Init_single a ->
    (match constval_cast ce a ty with
     | OK x ->
       (match x with
        | Vundef ->
          Error
            (msg
              ('u'::('n'::('d'::('e'::('f'::('i'::('n'::('e'::('d'::(' '::('o'::('p'::('e'::('r'::('a'::('t'::('i'::('o'::('n'::(' '::('i'::('n'::(' '::('b'::('i'::('t'::('f'::('i'::('e'::('l'::('d'::(' '::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::('r'::[]))))))))))))))))))))))))))))))))))))))))))))
        | Vint n ->
          (match load_int s pos sz with
           | OK x0 ->
             let c' = Int.bitfield_insert (first_bit sz p w) w x0 n in
             store_int s pos sz c'
           | Error msg0 -> Error msg0)
        | _ ->
          Error
            (msg
              ('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::(' '::('i'::('n'::(' '::('b'::('i'::('t'::('f'::('i'::('e'::('l'::('d'::(' '::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::('r'::[])))))))))))))))))))))))))))))))))))))))
     | Error msg0 -> Error msg0)
  | _ ->
    Error
      (msg
        ('b'::('i'::('t'::('f'::('i'::('e'::('l'::('d'::(' '::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::('d'::(' '::('b'::('y'::(' '::('c'::('o'::('m'::('p'::('o'::('s'::('i'::('t'::('e'::(' '::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::('r'::[]))))))))))))))))))))))))))))))))))))))))))))))

(** val member_not_initialized : member -> bool **)

let member_not_initialized = function
| Member_plain (_, _) -> false
| Member_bitfield (_, _, _, _, w, p) -> (||) p ((fun x -> x) (zle w Z0))

(** val transl_init_rec :
    composite_env -> state -> coq_type -> coq_initializer -> coq_Z -> state
    res **)

let rec transl_init_rec ce s ty i pos =
  match i with
  | Init_single a ->
    (match transl_init_single ce ty a with
     | OK x -> store_data s pos x
     | Error msg0 -> Error msg0)
  | Init_array il ->
    (match ty with
     | Tarray (tyelt, nelt, _) ->
       if zle (length_initializer_list il) (Z.max Z0 nelt)
       then transl_init_array ce s tyelt il pos
       else assertion_failed
     | _ ->
       Error
         (msg
           ('w'::('r'::('o'::('n'::('g'::(' '::('t'::('y'::('p'::('e'::(' '::('f'::('o'::('r'::(' '::('c'::('o'::('m'::('p'::('o'::('u'::('n'::('d'::(' '::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::('r'::[])))))))))))))))))))))))))))))))))))))
  | Init_struct il ->
    (match ty with
     | Tstruct (id, _) ->
       (match lookup_composite ce id with
        | OK x ->
          (match x.co_su with
           | Struct -> transl_init_struct ce s x.co_members il pos Z0
           | Union ->
             Error ((MSG
               ('s'::('t'::('r'::('u'::('c'::('t'::('/'::('u'::('n'::('i'::('o'::('n'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::(' '::('o'::('n'::(' '::[])))))))))))))))))))))))))) :: ((CTX
               id) :: [])))
        | Error msg0 -> Error msg0)
     | _ ->
       Error
         (msg
           ('w'::('r'::('o'::('n'::('g'::(' '::('t'::('y'::('p'::('e'::(' '::('f'::('o'::('r'::(' '::('c'::('o'::('m'::('p'::('o'::('u'::('n'::('d'::(' '::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::('r'::[])))))))))))))))))))))))))))))))))))))
  | Init_union (f, i1) ->
    (match ty with
     | Tunion (id, _) ->
       (match lookup_composite ce id with
        | OK x ->
          (match x.co_su with
           | Struct ->
             Error ((MSG
               ('u'::('n'::('i'::('o'::('n'::('/'::('s'::('t'::('r'::('u'::('c'::('t'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::(' '::('o'::('n'::(' '::[])))))))))))))))))))))))))) :: ((CTX
               id) :: []))
           | Union ->
             (match field_type f x.co_members with
              | OK x0 ->
                (match union_field_offset ce f x.co_members with
                 | OK p ->
                   let (x1, y) = p in
                   (match y with
                    | Full -> transl_init_rec ce s x0 i1 (Z.add pos x1)
                    | Bits (sz, _, p0, w) ->
                      transl_init_bitfield ce s x0 sz p0 w i1 (Z.add pos x1))
                 | Error msg0 -> Error msg0)
              | Error msg0 -> Error msg0))
        | Error msg0 -> Error msg0)
     | _ ->
       Error
         (msg
           ('w'::('r'::('o'::('n'::('g'::(' '::('t'::('y'::('p'::('e'::(' '::('f'::('o'::('r'::(' '::('c'::('o'::('m'::('p'::('o'::('u'::('n'::('d'::(' '::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::('r'::[])))))))))))))))))))))))))))))))))))))

(** val transl_init_array :
    composite_env -> state -> coq_type -> initializer_list -> coq_Z -> state
    res **)

and transl_init_array ce s tyelt il pos =
  match il with
  | Init_nil -> OK s
  | Init_cons (i1, il') ->
    (match transl_init_rec ce s tyelt i1 pos with
     | OK x -> transl_init_array ce x tyelt il' (Z.add pos (sizeof ce tyelt))
     | Error msg0 -> Error msg0)

(** val transl_init_struct :
    composite_env -> state -> members -> initializer_list -> coq_Z -> coq_Z
    -> state res **)

and transl_init_struct ce s ms il base pos =
  match il with
  | Init_nil -> OK s
  | Init_cons (i1, il') ->
    let rec init0 ms0 pos0 =
      match ms0 with
      | [] ->
        Error
          (msg
            ('t'::('o'::('o'::(' '::('m'::('a'::('n'::('y'::(' '::('e'::('l'::('e'::('m'::('e'::('n'::('t'::('s'::(' '::('i'::('n'::(' '::('s'::('t'::('r'::('u'::('c'::('t'::(' '::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::('r'::[]))))))))))))))))))))))))))))))))))))))))
      | m :: ms' ->
        if member_not_initialized m
        then init0 ms' (next_field ce pos0 m)
        else (match layout_field ce pos0 m with
              | OK p ->
                let (x, y) = p in
                (match match y with
                       | Full ->
                         transl_init_rec ce s (type_member m) i1
                           (Z.add base x)
                       | Bits (sz, _, p0, w) ->
                         transl_init_bitfield ce s (type_member m) sz p0 w i1
                           (Z.add base x) with
                 | OK x0 ->
                   transl_init_struct ce x0 ms' il' base
                     (next_field ce pos0 m)
                 | Error msg0 -> Error msg0)
              | Error msg0 -> Error msg0)
    in init0 ms pos

(** val transl_init :
    composite_env -> coq_type -> coq_initializer -> init_data list res **)

let transl_init ce ty i =
  let s0 = initial_state (sizeof ce ty) in
  (match transl_init_rec ce s0 ty i Z0 with
   | OK x -> init_data_list_of_state x
   | Error msg0 -> Error msg0)
