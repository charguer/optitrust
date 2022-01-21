open Allocation
open Asm
open Asmgen
open BinNums
open CSE
open CleanupLabels
open Clight
open Cminor
open Cminorgen
open Compopts
open Constprop
open Cshmgen
open Csyntax
open Deadcode
open Debugvar
open Errors
open Inlining
open LTL
open Linearize
open Mach
open RTL
open RTLgen
open Renumber
open Selection
open SimplExpr
open SimplLocals
open Stacking
open Tailcall
open Tunneling
open Unusedglob

(** val print_Clight : Clight.program -> unit **)

let print_Clight = PrintClight.print_if

(** val print_Cminor : Cminor.program -> unit **)

let print_Cminor = PrintCminor.print_if

(** val print_RTL : coq_Z -> program -> unit **)

let print_RTL = PrintRTL.print_if

(** val print_LTL : LTL.program -> unit **)

let print_LTL = PrintLTL.print_if

(** val print_Mach : Mach.program -> unit **)

let print_Mach = PrintMach.print_if

(** val apply_total : 'a1 Errors.res -> ('a1 -> 'a2) -> 'a2 Errors.res **)

let apply_total x f =
  match x with
  | Errors.OK x1 -> Errors.OK (f x1)
  | Errors.Error msg -> Errors.Error msg

(** val apply_partial :
    'a1 Errors.res -> ('a1 -> 'a2 Errors.res) -> 'a2 Errors.res **)

let apply_partial x f =
  match x with
  | Errors.OK x1 -> f x1
  | Errors.Error msg -> Errors.Error msg

(** val print : ('a1 -> unit) -> 'a1 -> 'a1 **)

let print = fun (f: 'a -> unit) (x: 'a) -> f x; x

(** val time : char list -> ('a1 -> 'a2) -> 'a1 -> 'a2 **)

let time = Timing.time_coq

(** val total_if : (unit -> bool) -> ('a1 -> 'a1) -> 'a1 -> 'a1 **)

let total_if flag f prog =
  if flag () then f prog else prog

(** val partial_if :
    (unit -> bool) -> ('a1 -> 'a1 Errors.res) -> 'a1 -> 'a1 Errors.res **)

let partial_if flag f prog =
  if flag () then f prog else Errors.OK prog

(** val transf_rtl_program : program -> Asm.program Errors.res **)

let transf_rtl_program f =
  apply_partial
    (apply_total
      (apply_partial
        (apply_partial
          (apply_total
            (apply_partial
              (apply_total
                (apply_total
                  (apply_partial
                    (apply_total
                      (apply_partial
                        (apply_total
                          (apply_partial
                            (apply_total
                              (apply_partial
                                (apply_total
                                  (apply_total
                                    (apply_total
                                      (apply_total
                                        (apply_total
                                          (apply_total
                                            (apply_total
                                              (apply_partial
                                                (apply_total
                                                  (apply_total
                                                    (apply_total (Errors.OK
                                                      f)
                                                      (print (print_RTL Z0)))
                                                    (total_if optim_tailcalls
                                                      (time
                                                        ('T'::('a'::('i'::('l'::(' '::('c'::('a'::('l'::('l'::('s'::[]))))))))))
                                                        transf_program)))
                                                  (print
                                                    (print_RTL (Zpos Coq_xH))))
                                                (time
                                                  ('I'::('n'::('l'::('i'::('n'::('i'::('n'::('g'::[]))))))))
                                                  Inlining.transf_program))
                                              (print
                                                (print_RTL (Zpos (Coq_xO
                                                  Coq_xH)))))
                                            (time
                                              ('R'::('e'::('n'::('u'::('m'::('b'::('e'::('r'::('i'::('n'::('g'::[])))))))))))
                                              Renumber.transf_program))
                                          (print
                                            (print_RTL (Zpos (Coq_xI Coq_xH)))))
                                        (total_if optim_constprop
                                          (time
                                            ('C'::('o'::('n'::('s'::('t'::('a'::('n'::('t'::(' '::('p'::('r'::('o'::('p'::('a'::('g'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))
                                            Constprop.transf_program)))
                                      (print
                                        (print_RTL (Zpos (Coq_xO (Coq_xO
                                          Coq_xH))))))
                                    (total_if optim_constprop
                                      (time
                                        ('R'::('e'::('n'::('u'::('m'::('b'::('e'::('r'::('i'::('n'::('g'::[])))))))))))
                                        Renumber.transf_program)))
                                  (print
                                    (print_RTL (Zpos (Coq_xI (Coq_xO
                                      Coq_xH))))))
                                (partial_if optim_CSE
                                  (time ('C'::('S'::('E'::[])))
                                    CSE.transf_program)))
                              (print
                                (print_RTL (Zpos (Coq_xO (Coq_xI Coq_xH))))))
                            (partial_if optim_redundancy
                              (time
                                ('R'::('e'::('d'::('u'::('n'::('d'::('a'::('n'::('c'::('y'::(' '::('e'::('l'::('i'::('m'::('i'::('n'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))
                                Deadcode.transf_program)))
                          (print (print_RTL (Zpos (Coq_xI (Coq_xI Coq_xH))))))
                        (time
                          ('U'::('n'::('u'::('s'::('e'::('d'::(' '::('g'::('l'::('o'::('b'::('a'::('l'::('s'::[]))))))))))))))
                          transform_program))
                      (print
                        (print_RTL (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))
                    (time
                      ('R'::('e'::('g'::('i'::('s'::('t'::('e'::('r'::(' '::('a'::('l'::('l'::('o'::('c'::('a'::('t'::('i'::('o'::('n'::[])))))))))))))))))))
                      Allocation.transf_program)) (print print_LTL))
                (time
                  ('B'::('r'::('a'::('n'::('c'::('h'::(' '::('t'::('u'::('n'::('n'::('e'::('l'::('i'::('n'::('g'::[]))))))))))))))))
                  tunnel_program))
              (time
                ('C'::('F'::('G'::(' '::('l'::('i'::('n'::('e'::('a'::('r'::('i'::('z'::('a'::('t'::('i'::('o'::('n'::[])))))))))))))))))
                Linearize.transf_program))
            (time
              ('L'::('a'::('b'::('e'::('l'::(' '::('c'::('l'::('e'::('a'::('n'::('u'::('p'::[])))))))))))))
              CleanupLabels.transf_program))
          (partial_if debug
            (time
              ('D'::('e'::('b'::('u'::('g'::('g'::('i'::('n'::('g'::(' '::('i'::('n'::('f'::('o'::(' '::('f'::('o'::('r'::(' '::('l'::('o'::('c'::('a'::('l'::(' '::('v'::('a'::('r'::('i'::('a'::('b'::('l'::('e'::('s'::[]))))))))))))))))))))))))))))))))))
              Debugvar.transf_program)))
        (time
          ('M'::('a'::('c'::('h'::(' '::('g'::('e'::('n'::('e'::('r'::('a'::('t'::('i'::('o'::('n'::[])))))))))))))))
          Stacking.transf_program)) (print print_Mach))
    (time
      ('A'::('s'::('m'::(' '::('g'::('e'::('n'::('e'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))
      Asmgen.transf_program)

(** val transf_cminor_program : Cminor.program -> Asm.program Errors.res **)

let transf_cminor_program p =
  apply_partial
    (apply_partial
      (apply_partial (apply_total (Errors.OK p) (print print_Cminor))
        (time
          ('I'::('n'::('s'::('t'::('r'::('u'::('c'::('t'::('i'::('o'::('n'::(' '::('s'::('e'::('l'::('e'::('c'::('t'::('i'::('o'::('n'::[])))))))))))))))))))))
          sel_program))
      (time
        ('R'::('T'::('L'::(' '::('g'::('e'::('n'::('e'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))
        RTLgen.transl_program)) transf_rtl_program

(** val transf_clight_program : Clight.program -> Asm.program Errors.res **)

let transf_clight_program p =
  apply_partial
    (apply_partial
      (apply_partial
        (apply_partial (apply_total (Errors.OK p) (print print_Clight))
          (time
            ('S'::('i'::('m'::('p'::('l'::('i'::('f'::('i'::('c'::('a'::('t'::('i'::('o'::('n'::(' '::('o'::('f'::(' '::('l'::('o'::('c'::('a'::('l'::('s'::[]))))))))))))))))))))))))
            SimplLocals.transf_program))
        (time
          ('C'::('#'::('m'::('i'::('n'::('o'::('r'::(' '::('g'::('e'::('n'::('e'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))
          Cshmgen.transl_program))
      (time
        ('C'::('m'::('i'::('n'::('o'::('r'::(' '::('g'::('e'::('n'::('e'::('r'::('a'::('t'::('i'::('o'::('n'::[])))))))))))))))))
        Cminorgen.transl_program)) transf_cminor_program

(** val transf_c_program : Csyntax.program -> Asm.program Errors.res **)

let transf_c_program p =
  apply_partial
    (apply_partial (Errors.OK p)
      (time
        ('C'::('l'::('i'::('g'::('h'::('t'::(' '::('g'::('e'::('n'::('e'::('r'::('a'::('t'::('i'::('o'::('n'::[])))))))))))))))))
        transl_program)) transf_clight_program
