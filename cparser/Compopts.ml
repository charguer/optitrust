
(** val optim_for_size : unit -> bool **)

let optim_for_size = fun _ -> !Clflags.option_Osize

(** val propagate_float_constants : unit -> bool **)

let propagate_float_constants = fun _ -> !Clflags.option_ffloatconstprop >= 1

(** val generate_float_constants : unit -> bool **)

let generate_float_constants = fun _ -> !Clflags.option_ffloatconstprop >= 2

(** val va_strict : unit -> bool **)

let va_strict = fun _ -> false

(** val optim_tailcalls : unit -> bool **)

let optim_tailcalls = fun _ -> !Clflags.option_ftailcalls

(** val optim_constprop : unit -> bool **)

let optim_constprop = fun _ -> !Clflags.option_fconstprop

(** val optim_CSE : unit -> bool **)

let optim_CSE = fun _ -> !Clflags.option_fcse

(** val optim_redundancy : unit -> bool **)

let optim_redundancy = fun _ -> !Clflags.option_fredundancy

(** val debug : unit -> bool **)

let debug = fun _ -> !Clflags.option_g
