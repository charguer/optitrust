open AST
open BinInt
open BinNums
open Conventions1
open List0
open Locations

val max_outgoing_1 : coq_Z -> loc -> coq_Z

val max_outgoing_2 : coq_Z -> loc rpair -> coq_Z

val size_arguments : signature -> coq_Z

val parameter_of_argument : loc -> loc

val loc_parameters : signature -> loc rpair list

val tailcall_is_possible : signature -> bool
