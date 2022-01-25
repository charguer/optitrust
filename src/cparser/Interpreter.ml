open Alphabet
open Automaton
open Datatypes
open Grammar
open List0
open Specif
open Validator_safe

module Make =
 functor (A:Automaton.T) ->
 struct
  module ValidSafe = Make(A)

  type coq_Decidable = bool

  (** val decide : coq_Decidable -> bool **)

  let decide decidable =
    decidable

  (** val comparable_decidable_eq :
      'a1 coq_Comparable -> 'a1 -> 'a1 -> coq_Decidable **)

  let comparable_decidable_eq c x y =
    let c0 = compare c x y in (match c0 with
                               | Eq -> true
                               | _ -> false)

  (** val list_decidable_eq :
      ('a1 -> 'a1 -> coq_Decidable) -> 'a1 list -> 'a1 list -> coq_Decidable **)

  let rec list_decidable_eq x l1 l2 =
    match l1 with
    | [] -> (match l2 with
             | [] -> true
             | _ :: _ -> false)
    | y :: l ->
      (match l2 with
       | [] -> false
       | t :: l0 -> if x y t then list_decidable_eq x l l0 else false)

  (** val cast : 'a1 -> 'a1 -> (unit -> coq_Decidable) -> 'a2 -> 'a2 **)

  let cast _ _ _ a =
    a

  type buffer = __buffer Lazy.t
  and __buffer =
  | Buf_cons of A.Gram.token * buffer

  (** val buf_head : buffer -> A.Gram.token **)

  let buf_head b =
    let Buf_cons (buf_head0, _) = Lazy.force b in buf_head0

  (** val buf_tail : buffer -> buffer **)

  let buf_tail b =
    let Buf_cons (_, buf_tail0) = Lazy.force b in buf_tail0

  (** val app_buf : A.Gram.token list -> buffer -> buffer **)

  let rec app_buf l buf =
    match l with
    | [] -> buf
    | t :: q -> lazy (Buf_cons (t, (app_buf q buf)))

  type noninitstate_type = A.Gram.symbol_semantic_type

  type stack = (A.noninitstate, noninitstate_type) sigT list

  (** val state_of_stack : A.initstate -> stack -> A.state **)

  let state_of_stack init = function
  | [] -> A.Init init
  | s0 :: _ -> let Coq_existT (s, _) = s0 in A.Ninit s

  (** val state_stack_of_stack :
      A.initstate -> stack -> (A.state -> bool) list **)

  let state_stack_of_stack init stack0 =
    app
      (map (fun cell ->
        ValidSafe.singleton_state_pred (A.Ninit (projT1 cell))) stack0)
      ((ValidSafe.singleton_state_pred (A.Init init)) :: [])

  (** val symb_stack_of_stack : stack -> A.Gram.symbol list **)

  let symb_stack_of_stack stack0 =
    map (fun cell -> A.last_symb_of_non_init_state (projT1 cell)) stack0

  (** val pop :
      A.Gram.symbol list -> stack -> 'a1 arrows_right -> stack * 'a1 **)

  let rec pop symbols_to_pop stk action =
    match symbols_to_pop with
    | [] -> (stk, (Obj.magic action))
    | t :: q ->
      (match stk with
       | [] -> assert false (* absurd case *)
       | s :: stack_rec ->
         let Coq_existT (state_cur, sem) = s in
         let sem_conv =
           cast (A.last_symb_of_non_init_state state_cur) t (fun _ ->
             comparable_decidable_eq
               A.Gram.coq_SymbolAlph.coq_AlphabetComparable
               (A.last_symb_of_non_init_state state_cur) t) sem
         in
         pop q stack_rec (Obj.magic action sem_conv))

  type step_result =
  | Fail_sr_full of A.state * A.Gram.token
  | Accept_sr of A.Gram.symbol_semantic_type * buffer
  | Progress_sr of stack * buffer

  (** val step_result_rect :
      A.initstate -> (A.state -> A.Gram.token -> 'a1) ->
      (A.Gram.symbol_semantic_type -> buffer -> 'a1) -> (stack -> buffer ->
      'a1) -> step_result -> 'a1 **)

  let step_result_rect _ f f0 f1 = function
  | Fail_sr_full (x, x0) -> f x x0
  | Accept_sr (x, x0) -> f0 x x0
  | Progress_sr (x, x0) -> f1 x x0

  (** val step_result_rec :
      A.initstate -> (A.state -> A.Gram.token -> 'a1) ->
      (A.Gram.symbol_semantic_type -> buffer -> 'a1) -> (stack -> buffer ->
      'a1) -> step_result -> 'a1 **)

  let step_result_rec _ f f0 f1 = function
  | Fail_sr_full (x, x0) -> f x x0
  | Accept_sr (x, x0) -> f0 x x0
  | Progress_sr (x, x0) -> f1 x x0

  (** val reduce_step :
      A.initstate -> stack -> A.Gram.production -> buffer -> step_result **)

  let reduce_step init stk prod buffer0 =
    let (stk', sem) =
      pop (A.Gram.prod_rhs_rev prod) stk (A.Gram.prod_action prod)
    in
    (match A.goto_table (state_of_stack init stk') (A.Gram.prod_lhs prod) with
     | Some s -> Progress_sr (((Coq_existT (s, sem)) :: stk'), buffer0)
     | None ->
       let sem0 =
         cast (A.Gram.NT (A.Gram.prod_lhs prod)) (A.Gram.NT
           (A.start_nt init)) (fun _ ->
           comparable_decidable_eq
             A.Gram.coq_SymbolAlph.coq_AlphabetComparable (A.Gram.NT
             (A.Gram.prod_lhs prod)) (A.Gram.NT (A.start_nt init))) sem
       in
       Accept_sr (sem0, buffer0))

  (** val step : A.initstate -> stack -> buffer -> step_result **)

  let step init stk buffer0 =
    match A.action_table (state_of_stack init stk) with
    | A.Default_reduce_act prod -> reduce_step init stk prod buffer0
    | A.Lookahead_act awt ->
      (match awt (A.Gram.token_term (buf_head buffer0)) with
       | A.Shift_act state_new ->
         let sem_conv = A.Gram.token_sem (buf_head buffer0) in
         Progress_sr (((Coq_existT (state_new, sem_conv)) :: stk),
         (buf_tail buffer0))
       | A.Reduce_act prod -> reduce_step init stk prod buffer0
       | A.Fail_act ->
         Fail_sr_full ((state_of_stack init stk), (buf_head buffer0)))

  (** val parse_fix : A.initstate -> stack -> buffer -> nat -> step_result **)

  let rec parse_fix init stk buffer0 = function
  | O -> step init stk buffer0
  | S log_n_steps0 ->
    (match parse_fix init stk buffer0 log_n_steps0 with
     | Progress_sr (stk0, buffer1) -> parse_fix init stk0 buffer1 log_n_steps0
     | x -> x)

  type 'a parse_result =
  | Fail_pr_full of A.state * A.Gram.token
  | Timeout_pr
  | Parsed_pr of 'a * buffer

  (** val parse_result_rect :
      (A.state -> A.Gram.token -> 'a2) -> 'a2 -> ('a1 -> buffer -> 'a2) ->
      'a1 parse_result -> 'a2 **)

  let parse_result_rect f f0 f1 = function
  | Fail_pr_full (x, x0) -> f x x0
  | Timeout_pr -> f0
  | Parsed_pr (x, x0) -> f1 x x0

  (** val parse_result_rec :
      (A.state -> A.Gram.token -> 'a2) -> 'a2 -> ('a1 -> buffer -> 'a2) ->
      'a1 parse_result -> 'a2 **)

  let parse_result_rec f f0 f1 = function
  | Fail_pr_full (x, x0) -> f x x0
  | Timeout_pr -> f0
  | Parsed_pr (x, x0) -> f1 x x0

  (** val parse :
      A.initstate -> buffer -> nat -> A.Gram.symbol_semantic_type parse_result **)

  let parse init buffer0 log_n_steps =
    match parse_fix init [] buffer0 log_n_steps with
    | Fail_sr_full (st, tok) -> Fail_pr_full (st, tok)
    | Accept_sr (sem, buffer') -> Parsed_pr (sem, buffer')
    | Progress_sr (_, _) -> Timeout_pr
 end
