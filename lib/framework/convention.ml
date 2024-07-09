(** [naming_policy]: naming policy used for string concatentation *)
type naming_policy = Naming_capitalize | Naming_underscore

(** [default_naming_policy]: by default the second string is capitalized *)
let default_naming_policy = ref Naming_capitalize


(** [name_app ~policy s1 s2]: concatenates string [s1] and [s2] using [policy] *)
let name_app ?(policy:naming_policy option) (s1 : string) (s2 : string) : string =
  let policy =
    match policy with
    | None -> !default_naming_policy
    | Some p -> p
    in
  match policy with
  | Naming_underscore -> s1 ^ "_" ^ s2
  | Naming_capitalize -> s1 ^ String.capitalize_ascii s2
