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

val print_Clight : Clight.program -> unit

val print_Cminor : Cminor.program -> unit

val print_RTL : coq_Z -> program -> unit

val print_LTL : LTL.program -> unit

val print_Mach : Mach.program -> unit

val apply_total : 'a1 Errors.res -> ('a1 -> 'a2) -> 'a2 Errors.res

val apply_partial :
  'a1 Errors.res -> ('a1 -> 'a2 Errors.res) -> 'a2 Errors.res

val print : ('a1 -> unit) -> 'a1 -> 'a1

val time : char list -> ('a1 -> 'a2) -> 'a1 -> 'a2

val total_if : (unit -> bool) -> ('a1 -> 'a1) -> 'a1 -> 'a1

val partial_if :
  (unit -> bool) -> ('a1 -> 'a1 Errors.res) -> 'a1 -> 'a1 Errors.res

val transf_rtl_program : program -> Asm.program Errors.res

val transf_cminor_program : Cminor.program -> Asm.program Errors.res

val transf_clight_program : Clight.program -> Asm.program Errors.res

val transf_c_program : Csyntax.program -> Asm.program Errors.res
