open Alphabet
open Automaton
open Datatypes
open Grammar
open Int0
open Nat
open Specif
open Validator_complete

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

module Make =
 functor (A:Automaton.T) ->
 functor (Inter:sig
  module ValidSafe :
   sig
    val singleton_state_pred : A.state -> A.state -> bool

    val past_state_of_state : A.state -> (A.state -> bool) list

    val head_symbs_of_state : A.state -> A.Gram.symbol list

    val head_states_of_state : A.state -> (A.state -> bool) list

    val is_prefix : A.Gram.symbol list -> A.Gram.symbol list -> bool

    val is_prefix_pred :
      (A.state -> bool) list -> (A.state -> bool) list -> bool

    val is_state_valid_after_pop :
      A.state -> A.Gram.symbol list -> (A.state -> bool) list -> bool

    val is_safe : unit -> bool
   end

  type coq_Decidable = bool

  val decide : coq_Decidable -> bool

  val comparable_decidable_eq :
    'a1 coq_Comparable -> 'a1 -> 'a1 -> coq_Decidable

  val list_decidable_eq :
    ('a1 -> 'a1 -> coq_Decidable) -> 'a1 list -> 'a1 list -> coq_Decidable

  val cast : 'a1 -> 'a1 -> (unit -> coq_Decidable) -> 'a2 -> 'a2

  type buffer = __buffer Lazy.t
  and __buffer =
  | Buf_cons of A.Gram.token * buffer

  val buf_head : buffer -> A.Gram.token

  val buf_tail : buffer -> buffer

  val app_buf : A.Gram.token list -> buffer -> buffer

  type noninitstate_type = A.Gram.symbol_semantic_type

  type stack = (A.noninitstate, noninitstate_type) sigT list

  val state_of_stack : A.initstate -> stack -> A.state

  val state_stack_of_stack : A.initstate -> stack -> (A.state -> bool) list

  val symb_stack_of_stack : stack -> A.Gram.symbol list

  val pop : A.Gram.symbol list -> stack -> 'a1 arrows_right -> stack * 'a1

  type step_result =
  | Fail_sr_full of A.state * A.Gram.token
  | Accept_sr of A.Gram.symbol_semantic_type * buffer
  | Progress_sr of stack * buffer

  val step_result_rect :
    A.initstate -> (A.state -> A.Gram.token -> 'a1) ->
    (A.Gram.symbol_semantic_type -> buffer -> 'a1) -> (stack -> buffer ->
    'a1) -> step_result -> 'a1

  val step_result_rec :
    A.initstate -> (A.state -> A.Gram.token -> 'a1) ->
    (A.Gram.symbol_semantic_type -> buffer -> 'a1) -> (stack -> buffer ->
    'a1) -> step_result -> 'a1

  val reduce_step :
    A.initstate -> stack -> A.Gram.production -> buffer -> step_result

  val step : A.initstate -> stack -> buffer -> step_result

  val parse_fix : A.initstate -> stack -> buffer -> nat -> step_result

  type 'a parse_result =
  | Fail_pr_full of A.state * A.Gram.token
  | Timeout_pr
  | Parsed_pr of 'a * buffer

  val parse_result_rect :
    (A.state -> A.Gram.token -> 'a2) -> 'a2 -> ('a1 -> buffer -> 'a2) -> 'a1
    parse_result -> 'a2

  val parse_result_rec :
    (A.state -> A.Gram.token -> 'a2) -> 'a2 -> ('a1 -> buffer -> 'a2) -> 'a1
    parse_result -> 'a2

  val parse :
    A.initstate -> buffer -> nat -> A.Gram.symbol_semantic_type parse_result
 end) ->
 struct
  module Valid = Make(A)

  type pt_zipper =
  | Top_ptz
  | Cons_ptl_ptz of A.Gram.symbol list * A.Gram.token list
     * A.GramDefs.parse_tree_list * A.Gram.symbol * A.Gram.token list
     * ptl_zipper
  and ptl_zipper =
  | Non_terminal_pt_ptlz of A.Gram.production * A.Gram.token list * pt_zipper
  | Cons_ptl_ptlz of A.Gram.symbol list * A.Gram.token list * A.Gram.symbol
     * A.Gram.token list * A.GramDefs.parse_tree * ptl_zipper

  (** val pt_zipper_rect :
      A.initstate -> A.Gram.token list -> 'a1 -> (A.Gram.symbol list ->
      A.Gram.token list -> A.GramDefs.parse_tree_list -> A.Gram.symbol ->
      A.Gram.token list -> ptl_zipper -> 'a1) -> A.Gram.symbol ->
      A.Gram.token list -> pt_zipper -> 'a1 **)

  let pt_zipper_rect _ _ f f0 _ _ = function
  | Top_ptz -> f
  | Cons_ptl_ptz (x, x0, x1, x2, x3, x4) -> f0 x x0 x1 x2 x3 x4

  (** val pt_zipper_rec :
      A.initstate -> A.Gram.token list -> 'a1 -> (A.Gram.symbol list ->
      A.Gram.token list -> A.GramDefs.parse_tree_list -> A.Gram.symbol ->
      A.Gram.token list -> ptl_zipper -> 'a1) -> A.Gram.symbol ->
      A.Gram.token list -> pt_zipper -> 'a1 **)

  let pt_zipper_rec _ _ f f0 _ _ = function
  | Top_ptz -> f
  | Cons_ptl_ptz (x, x0, x1, x2, x3, x4) -> f0 x x0 x1 x2 x3 x4

  (** val ptl_zipper_rect :
      A.initstate -> A.Gram.token list -> (A.Gram.production -> A.Gram.token
      list -> pt_zipper -> 'a1) -> (A.Gram.symbol list -> A.Gram.token list
      -> A.Gram.symbol -> A.Gram.token list -> A.GramDefs.parse_tree ->
      ptl_zipper -> 'a1 -> 'a1) -> A.Gram.symbol list -> A.Gram.token list ->
      ptl_zipper -> 'a1 **)

  let rec ptl_zipper_rect init full_word f f0 _ _ = function
  | Non_terminal_pt_ptlz (p0, word, p1) -> f p0 word p1
  | Cons_ptl_ptlz (head_symbolsq, wordq, head_symbolt, wordt, p0, p1) ->
    f0 head_symbolsq wordq head_symbolt wordt p0 p1
      (ptl_zipper_rect init full_word f f0 (head_symbolt :: head_symbolsq)
        (app wordq wordt) p1)

  (** val ptl_zipper_rec :
      A.initstate -> A.Gram.token list -> (A.Gram.production -> A.Gram.token
      list -> pt_zipper -> 'a1) -> (A.Gram.symbol list -> A.Gram.token list
      -> A.Gram.symbol -> A.Gram.token list -> A.GramDefs.parse_tree ->
      ptl_zipper -> 'a1 -> 'a1) -> A.Gram.symbol list -> A.Gram.token list ->
      ptl_zipper -> 'a1 **)

  let rec ptl_zipper_rec init full_word f f0 _ _ = function
  | Non_terminal_pt_ptlz (p0, word, p1) -> f p0 word p1
  | Cons_ptl_ptlz (head_symbolsq, wordq, head_symbolt, wordt, p0, p1) ->
    f0 head_symbolsq wordq head_symbolt wordt p0 p1
      (ptl_zipper_rec init full_word f f0 (head_symbolt :: head_symbolsq)
        (app wordq wordt) p1)

  type pt_dot =
  | Reduce_ptd of A.Gram.production * A.Gram.token list
     * A.GramDefs.parse_tree_list * pt_zipper
  | Shift_ptd of A.Gram.token * A.Gram.symbol list * A.Gram.token list
     * A.GramDefs.parse_tree_list * ptl_zipper

  (** val pt_dot_rect :
      A.initstate -> A.Gram.token list -> (A.Gram.production -> A.Gram.token
      list -> A.GramDefs.parse_tree_list -> pt_zipper -> 'a1) ->
      (A.Gram.token -> A.Gram.symbol list -> A.Gram.token list ->
      A.GramDefs.parse_tree_list -> ptl_zipper -> 'a1) -> pt_dot -> 'a1 **)

  let pt_dot_rect _ _ f f0 = function
  | Reduce_ptd (x, x0, x1, x2) -> f x x0 x1 x2
  | Shift_ptd (x, x0, x1, x2, x3) -> f0 x x0 x1 x2 x3

  (** val pt_dot_rec :
      A.initstate -> A.Gram.token list -> (A.Gram.production -> A.Gram.token
      list -> A.GramDefs.parse_tree_list -> pt_zipper -> 'a1) ->
      (A.Gram.token -> A.Gram.symbol list -> A.Gram.token list ->
      A.GramDefs.parse_tree_list -> ptl_zipper -> 'a1) -> pt_dot -> 'a1 **)

  let pt_dot_rec _ _ f f0 = function
  | Reduce_ptd (x, x0, x1, x2) -> f x x0 x1 x2
  | Shift_ptd (x, x0, x1, x2, x3) -> f0 x x0 x1 x2 x3

  (** val ptlz_sem :
      A.initstate -> A.Gram.token list -> A.Gram.symbol list -> A.Gram.token
      list -> ptl_zipper -> (__ -> __ arrows_right -> __) ->
      A.Gram.symbol_semantic_type **)

  let ptlz_sem _ _ =
    let rec ptlz_sem0 _ _ ptlz k =
      match ptlz with
      | Non_terminal_pt_ptlz (prod, word, ptz) ->
        ptz_sem0 (A.Gram.NT (A.Gram.prod_lhs prod)) word ptz
          (k __ (A.Gram.prod_action prod))
      | Cons_ptl_ptlz (head_symbolsq, wordq, head_symbolt, wordt, pt, ptlz0) ->
        Obj.magic ptlz_sem0 (head_symbolt :: head_symbolsq) (app wordq wordt)
          ptlz0 (fun _ f ->
          k __ (f (A.GramDefs.pt_sem head_symbolt wordt pt)))
    and ptz_sem0 _ _ ptz sem =
      match ptz with
      | Top_ptz -> Obj.magic sem
      | Cons_ptl_ptz (head_symbolsq, wordq, ptl, head_symbolt, wordt, ptlz) ->
        Obj.magic ptlz_sem0 (head_symbolt :: head_symbolsq) (app wordq wordt)
          ptlz (fun _ f -> A.GramDefs.ptl_sem head_symbolsq wordq ptl (f sem))
    in ptlz_sem0

  (** val ptz_sem :
      A.initstate -> A.Gram.token list -> A.Gram.symbol -> A.Gram.token list
      -> pt_zipper -> A.Gram.symbol_semantic_type ->
      A.Gram.symbol_semantic_type **)

  let ptz_sem _ _ =
    let rec ptlz_sem0 _ _ ptlz k =
      match ptlz with
      | Non_terminal_pt_ptlz (prod, word, ptz) ->
        ptz_sem0 (A.Gram.NT (A.Gram.prod_lhs prod)) word ptz
          (k __ (A.Gram.prod_action prod))
      | Cons_ptl_ptlz (head_symbolsq, wordq, head_symbolt, wordt, pt, ptlz0) ->
        Obj.magic ptlz_sem0 (head_symbolt :: head_symbolsq) (app wordq wordt)
          ptlz0 (fun _ f ->
          k __ (f (A.GramDefs.pt_sem head_symbolt wordt pt)))
    and ptz_sem0 _ _ ptz sem =
      match ptz with
      | Top_ptz -> sem
      | Cons_ptl_ptz (head_symbolsq, wordq, ptl, head_symbolt, wordt, ptlz) ->
        Obj.magic ptlz_sem0 (head_symbolt :: head_symbolsq) (app wordq wordt)
          ptlz (fun _ f -> A.GramDefs.ptl_sem head_symbolsq wordq ptl (f sem))
    in ptz_sem0

  (** val ptd_sem :
      A.initstate -> A.Gram.token list -> pt_dot ->
      A.Gram.symbol_semantic_type **)

  let ptd_sem init full_word = function
  | Reduce_ptd (prod, word, ptl, ptz) ->
    ptz_sem init full_word (A.Gram.NT (A.Gram.prod_lhs prod)) word ptz
      (A.GramDefs.ptl_sem (A.Gram.prod_rhs_rev prod) word ptl
        (A.Gram.prod_action prod))
  | Shift_ptd (tok, symbolsq, wordq, ptl, ptlz) ->
    ptlz_sem init full_word ((A.Gram.T (A.Gram.token_term tok)) :: symbolsq)
      (app wordq (tok :: [])) ptlz (fun _ f ->
      A.GramDefs.ptl_sem symbolsq wordq ptl
        (Obj.magic f (A.Gram.token_sem tok)))

  (** val ptlz_buffer :
      A.initstate -> A.Gram.token list -> Inter.buffer -> A.Gram.symbol list
      -> A.Gram.token list -> ptl_zipper -> Inter.buffer **)

  let ptlz_buffer _ _ buffer_end =
    let rec ptlz_buffer0 _ _ = function
    | Non_terminal_pt_ptlz (p, word, ptz) ->
      ptz_buffer0 (A.Gram.NT (A.Gram.prod_lhs p)) word ptz
    | Cons_ptl_ptlz (head_symbolsq, wordq, head_symbolt, wordt, _, ptlz') ->
      Inter.app_buf wordt
        (ptlz_buffer0 (head_symbolt :: head_symbolsq) (app wordq wordt) ptlz')
    and ptz_buffer0 _ _ = function
    | Top_ptz -> buffer_end
    | Cons_ptl_ptz (head_symbolsq, wordq, _, head_symbolt, wordt, ptlz) ->
      ptlz_buffer0 (head_symbolt :: head_symbolsq) (app wordq wordt) ptlz
    in ptlz_buffer0

  (** val ptz_buffer :
      A.initstate -> A.Gram.token list -> Inter.buffer -> A.Gram.symbol ->
      A.Gram.token list -> pt_zipper -> Inter.buffer **)

  let ptz_buffer _ _ buffer_end =
    let rec ptlz_buffer0 _ _ = function
    | Non_terminal_pt_ptlz (p, word, ptz) ->
      ptz_buffer0 (A.Gram.NT (A.Gram.prod_lhs p)) word ptz
    | Cons_ptl_ptlz (head_symbolsq, wordq, head_symbolt, wordt, _, ptlz') ->
      Inter.app_buf wordt
        (ptlz_buffer0 (head_symbolt :: head_symbolsq) (app wordq wordt) ptlz')
    and ptz_buffer0 _ _ = function
    | Top_ptz -> buffer_end
    | Cons_ptl_ptz (head_symbolsq, wordq, _, head_symbolt, wordt, ptlz) ->
      ptlz_buffer0 (head_symbolt :: head_symbolsq) (app wordq wordt) ptlz
    in ptz_buffer0

  (** val ptd_buffer :
      A.initstate -> A.Gram.token list -> Inter.buffer -> pt_dot ->
      Inter.buffer **)

  let ptd_buffer init full_word buffer_end = function
  | Reduce_ptd (prod, word, _, ptz) ->
    ptz_buffer init full_word buffer_end (A.Gram.NT (A.Gram.prod_lhs prod))
      word ptz
  | Shift_ptd (tok, symbolsq, wordq, _, ptlz) ->
    lazy (Inter.Buf_cons (tok,
      (ptlz_buffer init full_word buffer_end ((A.Gram.T
        (A.Gram.token_term tok)) :: symbolsq) (app wordq (tok :: [])) ptlz)))

  (** val ptlz_prod :
      A.initstate -> A.Gram.token list -> A.Gram.symbol list -> A.Gram.token
      list -> ptl_zipper -> A.Gram.production **)

  let rec ptlz_prod init full_word _ _ = function
  | Non_terminal_pt_ptlz (prod, _, _) -> prod
  | Cons_ptl_ptlz (head_symbolsq, wordq, head_symbolt, wordt, _, ptlz') ->
    ptlz_prod init full_word (head_symbolt :: head_symbolsq)
      (app wordq wordt) ptlz'

  (** val ptlz_future :
      A.initstate -> A.Gram.token list -> A.Gram.symbol list -> A.Gram.token
      list -> ptl_zipper -> A.Gram.symbol list **)

  let rec ptlz_future init full_word _ _ = function
  | Non_terminal_pt_ptlz (_, _, _) -> []
  | Cons_ptl_ptlz (head_symbolsq, wordq, s, wordt, _, ptlz') ->
    s :: (ptlz_future init full_word (s :: head_symbolsq) (app wordq wordt)
           ptlz')

  (** val ptlz_lookahead :
      A.initstate -> A.Gram.token list -> Inter.buffer -> A.Gram.symbol list
      -> A.Gram.token list -> ptl_zipper -> A.Gram.terminal **)

  let rec ptlz_lookahead init full_word buffer_end _ _ = function
  | Non_terminal_pt_ptlz (p, word, ptz) ->
    A.Gram.token_term
      (Inter.buf_head
        (ptz_buffer init full_word buffer_end (A.Gram.NT (A.Gram.prod_lhs p))
          word ptz))
  | Cons_ptl_ptlz (head_symbolsq, wordq, head_symbolt, wordt, _, ptlz') ->
    ptlz_lookahead init full_word buffer_end (head_symbolt :: head_symbolsq)
      (app wordq wordt) ptlz'

  (** val build_pt_dot_from_pt :
      A.initstate -> A.Gram.token list -> A.Gram.symbol -> A.Gram.token list
      -> A.GramDefs.parse_tree -> pt_zipper -> pt_dot **)

  let build_pt_dot_from_pt _ _ =
    let rec build_pt_dot_from_pt0 _ _ pt ptz =
      match pt with
      | A.GramDefs.Terminal_pt tok ->
        let x =
          match ptz with
          | Top_ptz -> assert false (* absurd case *)
          | Cons_ptl_ptz (head_symbolsq, wordq, ptl, _, _, ptlz) ->
            Coq_existT (head_symbolsq, (Coq_existT (wordq, (ptl, ptlz))))
        in
        Shift_ptd (tok, (projT1 x), (projT1 (projT2 x)),
        (fst (projT2 (projT2 x))), (snd (projT2 (projT2 x))))
      | A.GramDefs.Non_terminal_pt (prod, word0, ptl) ->
        let is_notnil =
          match ptl with
          | A.GramDefs.Nil_ptl -> None
          | A.GramDefs.Cons_ptl (_, _, _, _, _, _) -> Some __
        in
        (match is_notnil with
         | Some _ ->
           build_pt_dot_from_pt_rec0 (A.Gram.prod_rhs_rev prod) word0 ptl __
             (Non_terminal_pt_ptlz (prod, word0, ptz))
         | None -> Reduce_ptd (prod, word0, ptl, ptz))
    and build_pt_dot_from_pt_rec0 _ _ ptl _ ptlz =
      match ptl with
      | A.GramDefs.Nil_ptl -> assert false (* absurd case *)
      | A.GramDefs.Cons_ptl (_, _, ptl', head_symbolt, wordt, pt) ->
        (match ptl' with
         | A.GramDefs.Nil_ptl ->
           build_pt_dot_from_pt0 head_symbolt wordt pt (Cons_ptl_ptz ([], [],
             A.GramDefs.Nil_ptl, head_symbolt, wordt, ptlz))
         | A.GramDefs.Cons_ptl (head_symbolsq, wordq, _, head_symbolt0,
                                wordt0, _) ->
           build_pt_dot_from_pt_rec0 (head_symbolt0 :: head_symbolsq)
             (app wordq wordt0) ptl' __ (Cons_ptl_ptlz
             ((head_symbolt0 :: head_symbolsq), (app wordq wordt0),
             head_symbolt, wordt, pt, ptlz)))
    in build_pt_dot_from_pt0

  (** val build_pt_dot_from_pt_rec :
      A.initstate -> A.Gram.token list -> A.Gram.symbol list -> A.Gram.token
      list -> A.GramDefs.parse_tree_list -> ptl_zipper -> pt_dot **)

  let build_pt_dot_from_pt_rec _ _ symbs word ptl ptlz =
    let rec build_pt_dot_from_pt0 _ _ pt ptz =
      match pt with
      | A.GramDefs.Terminal_pt tok ->
        let x =
          match ptz with
          | Top_ptz -> assert false (* absurd case *)
          | Cons_ptl_ptz (head_symbolsq, wordq, ptl0, _, _, ptlz0) ->
            Coq_existT (head_symbolsq, (Coq_existT (wordq, (ptl0, ptlz0))))
        in
        Shift_ptd (tok, (projT1 x), (projT1 (projT2 x)),
        (fst (projT2 (projT2 x))), (snd (projT2 (projT2 x))))
      | A.GramDefs.Non_terminal_pt (prod, word0, ptl0) ->
        let is_notnil =
          match ptl0 with
          | A.GramDefs.Nil_ptl -> None
          | A.GramDefs.Cons_ptl (_, _, _, _, _, _) -> Some __
        in
        (match is_notnil with
         | Some _ ->
           build_pt_dot_from_pt_rec0 (A.Gram.prod_rhs_rev prod) word0 ptl0
             (Non_terminal_pt_ptlz (prod, word0, ptz))
         | None -> Reduce_ptd (prod, word0, ptl0, ptz))
    and build_pt_dot_from_pt_rec0 _ _ ptl0 ptlz0 =
      match ptl0 with
      | A.GramDefs.Nil_ptl -> assert false (* absurd case *)
      | A.GramDefs.Cons_ptl (_, _, ptl', head_symbolt, wordt, pt) ->
        (match ptl' with
         | A.GramDefs.Nil_ptl ->
           build_pt_dot_from_pt0 head_symbolt wordt pt (Cons_ptl_ptz ([], [],
             A.GramDefs.Nil_ptl, head_symbolt, wordt, ptlz0))
         | A.GramDefs.Cons_ptl (head_symbolsq, wordq, _, head_symbolt0,
                                wordt0, _) ->
           build_pt_dot_from_pt_rec0 (head_symbolt0 :: head_symbolsq)
             (app wordq wordt0) ptl' (Cons_ptl_ptlz
             ((head_symbolt0 :: head_symbolsq), (app wordq wordt0),
             head_symbolt, wordt, pt, ptlz0)))
    in build_pt_dot_from_pt_rec0 symbs word ptl ptlz

  (** val build_pt_dot_from_ptl :
      A.initstate -> A.Gram.token list -> A.Gram.symbol list -> A.Gram.token
      list -> A.GramDefs.parse_tree_list -> ptl_zipper -> pt_dot **)

  let build_pt_dot_from_ptl init full_word _ _ ptl = function
  | Non_terminal_pt_ptlz (p, word0, ptz) -> Reduce_ptd (p, word0, ptl, ptz)
  | Cons_ptl_ptlz (head_symbolsq, wordq, head_symbolt, wordt, pt, ptlz0) ->
    build_pt_dot_from_pt init full_word head_symbolt wordt pt (Cons_ptl_ptz
      (head_symbolsq, wordq, ptl, head_symbolt, wordt, ptlz0))

  (** val next_ptd :
      A.initstate -> A.Gram.token list -> pt_dot -> pt_dot option **)

  let next_ptd init full_word = function
  | Reduce_ptd (prod, word, ptl, ptz) ->
    let x = A.GramDefs.Non_terminal_pt (prod, word, ptl) in
    (match ptz with
     | Top_ptz -> None
     | Cons_ptl_ptz (head_symbolsq, wordq, ptl', head_symbolt, wordt, ptlz) ->
       Some
         (build_pt_dot_from_ptl init full_word
           (head_symbolt :: head_symbolsq) (app wordq wordt)
           (A.GramDefs.Cons_ptl (head_symbolsq, wordq, ptl', head_symbolt,
           wordt, x)) ptlz))
  | Shift_ptd (tok, symbolsq, wordq, ptl, ptlz) ->
    Some
      (build_pt_dot_from_ptl init full_word ((A.Gram.T
        (A.Gram.token_term tok)) :: symbolsq) (app wordq (tok :: []))
        (A.GramDefs.Cons_ptl (symbolsq, wordq, ptl, (A.Gram.T
        (A.Gram.token_term tok)), (tok :: []), (A.GramDefs.Terminal_pt tok)))
        ptlz)

  (** val next_ptd_iter :
      A.initstate -> A.Gram.token list -> pt_dot -> nat -> pt_dot option **)

  let rec next_ptd_iter init full_word ptd = function
  | O -> next_ptd init full_word ptd
  | S log_n_steps0 ->
    (match next_ptd_iter init full_word ptd log_n_steps0 with
     | Some ptd0 -> next_ptd_iter init full_word ptd0 log_n_steps0
     | None -> None)

  (** val ptlz_cost :
      A.initstate -> A.Gram.token list -> A.Gram.symbol list -> A.Gram.token
      list -> ptl_zipper -> nat **)

  let ptlz_cost _ _ =
    let rec ptlz_cost0 _ _ = function
    | Non_terminal_pt_ptlz (p, word, ptz) ->
      ptz_cost0 (A.Gram.NT (A.Gram.prod_lhs p)) word ptz
    | Cons_ptl_ptlz (head_symbolsq, wordq, head_symbolt, wordt, pt, ptlz') ->
      add (A.GramDefs.pt_size head_symbolt wordt pt)
        (ptlz_cost0 (head_symbolt :: head_symbolsq) (app wordq wordt) ptlz')
    and ptz_cost0 _ _ = function
    | Top_ptz -> O
    | Cons_ptl_ptz (head_symbolsq, wordq, _, head_symbolt, wordt, ptlz') ->
      add (S O)
        (ptlz_cost0 (head_symbolt :: head_symbolsq) (app wordq wordt) ptlz')
    in ptlz_cost0

  (** val ptz_cost :
      A.initstate -> A.Gram.token list -> A.Gram.symbol -> A.Gram.token list
      -> pt_zipper -> nat **)

  let ptz_cost _ _ =
    let rec ptlz_cost0 _ _ = function
    | Non_terminal_pt_ptlz (p, word, ptz) ->
      ptz_cost0 (A.Gram.NT (A.Gram.prod_lhs p)) word ptz
    | Cons_ptl_ptlz (head_symbolsq, wordq, head_symbolt, wordt, pt, ptlz') ->
      add (A.GramDefs.pt_size head_symbolt wordt pt)
        (ptlz_cost0 (head_symbolt :: head_symbolsq) (app wordq wordt) ptlz')
    and ptz_cost0 _ _ = function
    | Top_ptz -> O
    | Cons_ptl_ptz (head_symbolsq, wordq, _, head_symbolt, wordt, ptlz') ->
      add (S O)
        (ptlz_cost0 (head_symbolt :: head_symbolsq) (app wordq wordt) ptlz')
    in ptz_cost0

  (** val ptd_cost : A.initstate -> A.Gram.token list -> pt_dot -> nat **)

  let ptd_cost init full_word = function
  | Reduce_ptd (prod, word, _, ptz) ->
    ptz_cost init full_word (A.Gram.NT (A.Gram.prod_lhs prod)) word ptz
  | Shift_ptd (tok, symbolsq, wordq, _, ptlz) ->
    add (S O)
      (ptlz_cost init full_word ((A.Gram.T
        (A.Gram.token_term tok)) :: symbolsq) (app wordq (tok :: [])) ptlz)
 end
