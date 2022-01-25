open Alphabet
open Automaton
open Datatypes
open List0

module Make =
 functor (A:T) ->
 struct
  (** val singleton_state_pred : A.state -> A.state -> bool **)

  let singleton_state_pred state0 state' =
    match compare A.coq_StateAlph.coq_AlphabetComparable state0 state' with
    | Eq -> true
    | _ -> false

  (** val past_state_of_state : A.state -> (A.state -> bool) list **)

  let past_state_of_state = function
  | A.Init _ -> []
  | A.Ninit nis -> A.past_state_of_non_init_state nis

  (** val head_symbs_of_state : A.state -> A.Gram.symbol list **)

  let head_symbs_of_state = function
  | A.Init _ -> []
  | A.Ninit s ->
    (A.last_symb_of_non_init_state s) :: (A.past_symb_of_non_init_state s)

  (** val head_states_of_state : A.state -> (A.state -> bool) list **)

  let head_states_of_state state0 =
    (singleton_state_pred state0) :: (past_state_of_state state0)

  (** val is_prefix : A.Gram.symbol list -> A.Gram.symbol list -> bool **)

  let rec is_prefix l1 l2 =
    match l1 with
    | [] -> true
    | t1 :: q1 ->
      (match l2 with
       | [] -> false
       | t2 :: q2 ->
         (&&)
           (compare_eqb A.Gram.coq_SymbolAlph.coq_AlphabetComparable t1 t2)
           (is_prefix q1 q2))

  (** val is_prefix_pred :
      (A.state -> bool) list -> (A.state -> bool) list -> bool **)

  let rec is_prefix_pred l1 l2 =
    match l1 with
    | [] -> true
    | f1 :: q1 ->
      (match l2 with
       | [] -> false
       | f2 :: q2 ->
         (&&)
           (forallb (fun x -> implb (f2 x) (f1 x))
             (all_list A.coq_StateAlph.coq_AlphabetFinite))
           (is_prefix_pred q1 q2))

  (** val is_state_valid_after_pop :
      A.state -> A.Gram.symbol list -> (A.state -> bool) list -> bool **)

  let rec is_state_valid_after_pop state0 to_pop = function
  | [] -> true
  | p :: pl ->
    (match to_pop with
     | [] -> p state0
     | _ :: sl -> is_state_valid_after_pop state0 sl pl)

  (** val is_safe : unit -> bool **)

  let is_safe _ =
    if forallb (fun x ->
         match A.action_table x with
         | A.Default_reduce_act _ -> true
         | A.Lookahead_act l ->
           forallb (fun x0 ->
             match l x0 with
             | A.Shift_act s ->
               is_prefix (A.past_symb_of_non_init_state s)
                 (head_symbs_of_state x)
             | _ -> true)
             (all_list A.Gram.coq_TerminalAlph.coq_AlphabetFinite))
         (all_list A.coq_StateAlph.coq_AlphabetFinite)
    then if forallb (fun x ->
              forallb (fun x0 ->
                match A.goto_table x x0 with
                | Some s ->
                  is_prefix (A.past_symb_of_non_init_state s)
                    (head_symbs_of_state x)
                | None -> true)
                (all_list A.Gram.coq_NonTerminalAlph.coq_AlphabetFinite))
              (all_list A.coq_StateAlph.coq_AlphabetFinite)
         then if forallb (fun x ->
                   match A.action_table x with
                   | A.Default_reduce_act _ -> true
                   | A.Lookahead_act l ->
                     forallb (fun x0 ->
                       match l x0 with
                       | A.Shift_act s ->
                         is_prefix_pred (A.past_state_of_non_init_state s)
                           (head_states_of_state x)
                       | _ -> true)
                       (all_list A.Gram.coq_TerminalAlph.coq_AlphabetFinite))
                   (all_list A.coq_StateAlph.coq_AlphabetFinite)
              then if forallb (fun x ->
                        forallb (fun x0 ->
                          match A.goto_table x x0 with
                          | Some s ->
                            is_prefix_pred (A.past_state_of_non_init_state s)
                              (head_states_of_state x)
                          | None -> true)
                          (all_list
                            A.Gram.coq_NonTerminalAlph.coq_AlphabetFinite))
                        (all_list A.coq_StateAlph.coq_AlphabetFinite)
                   then forallb (fun x ->
                          match A.action_table x with
                          | A.Default_reduce_act p ->
                            if is_prefix (A.Gram.prod_rhs_rev p)
                                 (head_symbs_of_state x)
                            then forallb (fun x0 ->
                                   if is_state_valid_after_pop x0
                                        (A.Gram.prod_rhs_rev p)
                                        (head_states_of_state x)
                                   then (match A.goto_table x0
                                                 (A.Gram.prod_lhs p) with
                                         | Some _ -> true
                                         | None ->
                                           (match x0 with
                                            | A.Init i ->
                                              compare_eqb
                                                A.Gram.coq_NonTerminalAlph.coq_AlphabetComparable
                                                (A.Gram.prod_lhs p)
                                                (A.start_nt i)
                                            | A.Ninit _ -> false))
                                   else true)
                                   (all_list
                                     A.coq_StateAlph.coq_AlphabetFinite)
                            else false
                          | A.Lookahead_act l ->
                            forallb (fun x0 ->
                              match l x0 with
                              | A.Reduce_act p ->
                                if is_prefix (A.Gram.prod_rhs_rev p)
                                     (head_symbs_of_state x)
                                then forallb (fun x1 ->
                                       if is_state_valid_after_pop x1
                                            (A.Gram.prod_rhs_rev p)
                                            (head_states_of_state x)
                                       then (match A.goto_table x1
                                                     (A.Gram.prod_lhs p) with
                                             | Some _ -> true
                                             | None ->
                                               (match x1 with
                                                | A.Init i ->
                                                  compare_eqb
                                                    A.Gram.coq_NonTerminalAlph.coq_AlphabetComparable
                                                    (A.Gram.prod_lhs p)
                                                    (A.start_nt i)
                                                | A.Ninit _ -> false))
                                       else true)
                                       (all_list
                                         A.coq_StateAlph.coq_AlphabetFinite)
                                else false
                              | _ -> true)
                              (all_list
                                A.Gram.coq_TerminalAlph.coq_AlphabetFinite))
                          (all_list A.coq_StateAlph.coq_AlphabetFinite)
                   else false
              else false
         else false
    else false
 end
