open AST
open BinNums
open BinPos
open Conventions1
open Datatypes
open Errors
open Floats
open Integers
open Iteration
open Linear
open List0
open Locations
open Machregs
open Maps

type debuginfo = loc builtin_arg

(** val normalize_debug_1 : loc builtin_arg -> debuginfo option **)

let normalize_debug_1 = function
| BA x -> Some (BA x)
| BA_int n -> Some (BA_int n)
| BA_long n -> Some (BA_long n)
| BA_float n -> Some (BA_float n)
| BA_single n -> Some (BA_single n)
| BA_splitlong (hi0, lo0) ->
  (match hi0 with
   | BA hi ->
     (match lo0 with
      | BA lo -> Some (BA_splitlong ((BA hi), (BA lo)))
      | _ -> None)
   | _ -> None)
| _ -> None

(** val normalize_debug : loc builtin_arg list -> debuginfo option **)

let rec normalize_debug = function
| [] -> None
| a :: l' ->
  (match a with
   | BA_int _ ->
     (match normalize_debug l' with
      | Some i -> Some i
      | None -> normalize_debug_1 a)
   | BA_long _ ->
     (match normalize_debug l' with
      | Some i -> Some i
      | None -> normalize_debug_1 a)
   | BA_float _ ->
     (match normalize_debug l' with
      | Some i -> Some i
      | None -> normalize_debug_1 a)
   | BA_single _ ->
     (match normalize_debug l' with
      | Some i -> Some i
      | None -> normalize_debug_1 a)
   | _ -> normalize_debug_1 a)

type avail = (ident * debuginfo) list

(** val set_state : ident -> debuginfo -> avail -> avail **)

let rec set_state v i s = match s with
| [] -> (v, i) :: []
| vi' :: s' ->
  let (v', _) = vi' in
  (match Pos.compare v v' with
   | Eq -> (v, i) :: s'
   | Lt -> (v, i) :: s
   | Gt -> vi' :: (set_state v i s'))

(** val remove_state : ident -> avail -> avail **)

let rec remove_state v s = match s with
| [] -> []
| vi' :: s' ->
  let (v', _) = vi' in
  (match Pos.compare v v' with
   | Eq -> s'
   | Lt -> s
   | Gt -> vi' :: (remove_state v s'))

(** val set_debug_info : ident -> loc builtin_arg list -> avail -> avail **)

let set_debug_info v info s =
  match normalize_debug info with
  | Some a -> set_state v a s
  | None -> remove_state v s

(** val arg_no_overlap : loc builtin_arg -> loc -> bool **)

let rec arg_no_overlap a l =
  match a with
  | BA l' -> (fun x -> x) (Loc.diff_dec l' l)
  | BA_splitlong (hi, lo) -> (&&) (arg_no_overlap hi l) (arg_no_overlap lo l)
  | _ -> true

(** val kill : loc -> avail -> avail **)

let kill l s =
  filter (fun vi -> arg_no_overlap (snd vi) l) s

(** val kill_res : mreg builtin_res -> avail -> avail **)

let rec kill_res r s =
  match r with
  | BR r0 -> kill (R r0) s
  | BR_none -> s
  | BR_splitlong (hi, lo) -> kill_res hi (kill_res lo s)

(** val arg_preserved : loc builtin_arg -> bool **)

let rec arg_preserved = function
| BA x ->
  (match x with
   | R r -> negb ((fun x -> x) (in_dec mreg_eq r destroyed_at_call))
   | S (_, _, _) -> true)
| BA_splitlong (hi, lo) -> (&&) (arg_preserved hi) (arg_preserved lo)
| _ -> true

(** val kill_at_call : avail -> avail **)

let kill_at_call s =
  filter (fun vi -> arg_preserved (snd vi)) s

(** val eq_arg : loc builtin_arg -> loc builtin_arg -> bool **)

let rec eq_arg b x0 =
  match b with
  | BA x -> (match x0 with
             | BA x1 -> Loc.eq x x1
             | _ -> false)
  | BA_int n -> (match x0 with
                 | BA_int n0 -> Int.eq_dec n n0
                 | _ -> false)
  | BA_long n -> (match x0 with
                  | BA_long n0 -> Int64.eq_dec n n0
                  | _ -> false)
  | BA_float f ->
    (match x0 with
     | BA_float f0 -> Float.eq_dec f f0
     | _ -> false)
  | BA_single f ->
    (match x0 with
     | BA_single f0 -> Float32.eq_dec f f0
     | _ -> false)
  | BA_loadstack (chunk, ofs) ->
    (match x0 with
     | BA_loadstack (chunk0, ofs0) ->
       if chunk_eq chunk chunk0 then Ptrofs.eq_dec ofs ofs0 else false
     | _ -> false)
  | BA_addrstack ofs ->
    (match x0 with
     | BA_addrstack ofs0 -> Ptrofs.eq_dec ofs ofs0
     | _ -> false)
  | BA_loadglobal (chunk, id, ofs) ->
    (match x0 with
     | BA_loadglobal (chunk0, id0, ofs0) ->
       if chunk_eq chunk chunk0
       then if ident_eq id id0 then Ptrofs.eq_dec ofs ofs0 else false
       else false
     | _ -> false)
  | BA_addrglobal (id, ofs) ->
    (match x0 with
     | BA_addrglobal (id0, ofs0) ->
       if ident_eq id id0 then Ptrofs.eq_dec ofs ofs0 else false
     | _ -> false)
  | BA_splitlong (hi, lo) ->
    (match x0 with
     | BA_splitlong (hi0, lo0) ->
       if eq_arg hi hi0 then eq_arg lo lo0 else false
     | _ -> false)
  | BA_addptr (a1, a2) ->
    (match x0 with
     | BA_addptr (a4, a5) -> if eq_arg a1 a4 then eq_arg a2 a5 else false
     | _ -> false)

(** val eq_debuginfo : debuginfo -> debuginfo -> bool **)

let eq_debuginfo =
  eq_arg

(** val join : avail -> avail -> avail **)

let rec join s1 s2 =
  match s1 with
  | [] -> []
  | vi1 :: s1' ->
    let (v1, i1) = vi1 in
    let rec join2 s3 = match s3 with
    | [] -> []
    | vi2 :: s2' ->
      let (v2, i2) = vi2 in
      (match Pos.compare v1 v2 with
       | Eq ->
         if eq_debuginfo i1 i2 then vi1 :: (join s1' s2') else join s1' s2'
       | Lt -> join s1' s3
       | Gt -> join2 s2')
    in join2 s2

(** val eq_state : avail -> avail -> bool **)

let eq_state s1 s2 =
  list_eq_dec (fun x y ->
    let (x0, x1) = x in
    let (i, d) = y in if ident_eq x0 i then eq_debuginfo x1 d else false) s1
    s2

(** val top : avail **)

let top =
  []

type labelmap = avail PTree.t * bool

(** val get_label : label -> labelmap -> avail option **)

let get_label lbl lm =
  PTree.get lbl (fst lm)

(** val update_label : label -> avail -> labelmap -> labelmap * avail **)

let update_label lbl s1 lm =
  match get_label lbl lm with
  | Some s2 ->
    let s = join s1 s2 in
    if eq_state s s2
    then (lm, s2)
    else (((PTree.set lbl s (fst lm)), true), s)
  | None -> (((PTree.set lbl s1 (fst lm)), true), s1)

(** val update_labels : label list -> avail -> labelmap -> labelmap **)

let rec update_labels lbls s lm =
  match lbls with
  | [] -> lm
  | lbl1 :: lbls0 -> update_labels lbls0 s (fst (update_label lbl1 s lm))

(** val is_debug_setvar : external_function -> ident option **)

let is_debug_setvar = function
| EF_debug (kind, txt, _) ->
  (match kind with
   | Coq_xO p -> (match p with
                  | Coq_xH -> Some txt
                  | _ -> None)
   | _ -> None)
| _ -> None

(** val is_builtin_debug_setvar : instruction -> ident option **)

let is_builtin_debug_setvar = function
| Lbuiltin (ef, _, b) ->
  (match b with
   | BR_none -> is_debug_setvar ef
   | _ -> None)
| _ -> None

(** val transfer :
    labelmap -> avail option -> instruction -> labelmap * avail option **)

let transfer lm before i =
  match before with
  | Some s ->
    (match i with
     | Lgetstack (_, _, _, rd) -> (lm, (Some (kill (R rd) s)))
     | Lsetstack (_, sl, ofs, ty) -> (lm, (Some (kill (S (sl, ofs, ty)) s)))
     | Lop (_, _, dst) -> (lm, (Some (kill (R dst) s)))
     | Lload (_, _, _, dst) -> (lm, (Some (kill (R dst) s)))
     | Lstore (_, _, _, _) -> (lm, before)
     | Lcall (_, _) -> (lm, (Some (kill_at_call s)))
     | Lbuiltin (ef, args, res0) ->
       let s' =
         match is_debug_setvar ef with
         | Some v -> set_debug_info v args s
         | None -> s
       in
       (lm, (Some (kill_res res0 s')))
     | Llabel lbl -> let (lm1, s1) = update_label lbl s lm in (lm1, (Some s1))
     | Lgoto lbl -> let (lm1, _) = update_label lbl s lm in (lm1, None)
     | Lcond (_, _, lbl) ->
       let (lm1, _) = update_label lbl s lm in (lm1, before)
     | Ljumptable (_, lbls) -> ((update_labels lbls s lm), None)
     | _ -> (lm, None))
  | None ->
    (match i with
     | Llabel lbl -> (lm, (get_label lbl lm))
     | _ -> (lm, None))

(** val ana_code : labelmap -> avail option -> code -> labelmap **)

let rec ana_code lm before = function
| [] -> lm
| i :: c0 -> let (lm1, after) = transfer lm before i in ana_code lm1 after c0

(** val ana_iter : code -> labelmap -> (labelmap, labelmap) sum **)

let ana_iter c lm =
  let lm' = ana_code ((fst lm), false) (Some top) c in
  if snd lm' then Coq_inr lm' else Coq_inl lm

(** val ana_function : coq_function -> labelmap option **)

let ana_function f =
  PrimIter.iterate (ana_iter f.fn_code) (PTree.empty, false)

(** val diff : avail -> avail -> avail **)

let rec diff s1 s2 =
  match s1 with
  | [] -> []
  | vi1 :: s1' ->
    let (v1, i1) = vi1 in
    let rec diff2 s3 = match s3 with
    | [] -> s1
    | p :: s2' ->
      let (v2, i2) = p in
      (match Pos.compare v1 v2 with
       | Eq ->
         if eq_debuginfo i1 i2 then diff s1' s2' else vi1 :: (diff s1' s2')
       | Lt -> vi1 :: (diff s1' s3)
       | Gt -> diff2 s2')
    in diff2 s2

(** val delta_state : avail option -> avail option -> avail * avail **)

let delta_state before after =
  match before with
  | Some b ->
    (match after with
     | Some a -> ((diff b a), (diff a b))
     | None -> (b, []))
  | None -> (match after with
             | Some a -> ([], a)
             | None -> ([], []))

(** val add_start_range : (ident * debuginfo) -> code -> code **)

let add_start_range vi c =
  let (v, i) = vi in
  (Lbuiltin ((EF_debug ((Coq_xI Coq_xH), v, [])), (i :: []), BR_none)) :: c

(** val add_end_range : (ident * debuginfo) -> code -> code **)

let add_end_range vi c =
  let (v, _) = vi in
  (Lbuiltin ((EF_debug ((Coq_xO (Coq_xO Coq_xH)), v, [])), [], BR_none)) :: c

(** val add_delta_ranges : avail option -> avail option -> code -> code **)

let add_delta_ranges before after c =
  let (killed, born) = delta_state before after in
  fold_right add_end_range (fold_right add_start_range c born) killed

(** val skip_debug_setvar :
    labelmap -> avail option -> code -> avail option **)

let rec skip_debug_setvar lm before = function
| [] -> before
| i :: c' ->
  (match is_builtin_debug_setvar i with
   | Some _ -> skip_debug_setvar lm (snd (transfer lm before i)) c'
   | None -> before)

(** val transf_code : labelmap -> avail option -> code -> code **)

let rec transf_code lm before = function
| [] -> []
| i :: c' ->
  (match i with
   | Lgoto lbl1 ->
     (match c' with
      | [] ->
        let after = skip_debug_setvar lm (snd (transfer lm before i)) c' in
        i :: (add_delta_ranges before after (transf_code lm after c'))
      | i0 :: c'0 ->
        (match i0 with
         | Llabel lbl2 ->
           let after = get_label lbl2 lm in
           (Lgoto lbl1) :: ((Llabel
           lbl2) :: (add_delta_ranges before after (transf_code lm after c'0)))
         | _ ->
           let after = skip_debug_setvar lm (snd (transfer lm before i)) c' in
           i :: (add_delta_ranges before after (transf_code lm after c'))))
   | _ ->
     let after = skip_debug_setvar lm (snd (transfer lm before i)) c' in
     i :: (add_delta_ranges before after (transf_code lm after c')))

(** val transf_function : coq_function -> coq_function res **)

let transf_function f =
  match ana_function f with
  | Some lm ->
    OK { fn_sig = f.fn_sig; fn_stacksize = f.fn_stacksize; fn_code =
      (transf_code lm (Some top) f.fn_code) }
  | None ->
    Error
      (msg
        ('D'::('e'::('b'::('u'::('g'::('v'::('a'::('r'::(':'::(' '::('a'::('n'::('a'::('l'::('y'::('s'::('i'::('s'::(' '::('d'::('i'::('v'::('e'::('r'::('g'::('e'::('s'::[]))))))))))))))))))))))))))))

(** val transf_fundef : fundef -> fundef res **)

let transf_fundef fd =
  transf_partial_fundef transf_function fd

(** val transf_program : program -> program res **)

let transf_program p =
  transform_partial_program transf_fundef p
