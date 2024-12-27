open Ast
open Typ
open Trm
open Path
open Target
open Apac_miscellaneous

(** {0:preprocessing Pre-processing}
    
    Prior to translating the input sequential program into task candidate graphs
    (see [!section:tcd]), it undergoes a series of preliminary analysis and
    transformation passes.

    {1:exploding_let_mult Exploding multiple variable definitions}

    The first pass transforms all the multiple variable definitions in the
    source code into equivalent simple variable definitions. It simplifies
    further analyses and transformations but before all, it circumvents the
    problem causing incorrect value when computing the number of levels of
    indirection of variables in multiple variable declarations. *)

(** [explode_let_mult tg]: expects target [tg] to point at a multiple variable
    declaration. Then, it replaces the multiple variable declaration by a
    sequence of simple variable declarations.

    For example:

    {[
    int a = 1, b = a;
    ]}

    becomes:

    {[
    int a = 1;
    int b = a;
    ]}

    Note that this pass requires reparsing to work properly. *)
let explode_let_mult (tg : target) : unit =
  Target.reparse_after (fun tg ->
      Nobrace_transfo.remove_after (fun () ->
          Target.apply_at_target_paths (fun t ->
              (** Deconstruct the target multiple variable declaration term [t]
                  into a variable kind [vk], a list of typed variables [tvs]
                  being declared and a list of their initialization terms
                  [tis]. *)
              let error = "Apac_preprocessing.explode_let_mult: expected a \
                           target to a multiple variable declaration." in
              let (vk, tvs, tis) = trm_inv ~error trm_let_mult_inv t in
              (** Transform the multiple variable declaration into a sequence of
                  simple variable declarations. *)
              let s = List.map2 (fun tv ti -> trm_let vk tv ti) tvs tis in
              (** Return a new sequence (without braces) containing the simple
                  variable declarations. *)
              Syntax.trm_seq_no_brace s
            ) tg)
    ) tg

(** {1:building_records Building records}

    For these passes to work smoothly, we begin by constituting records about
    all the function and global variable definitions in the latter. *)

(** [record_functions tg]: expects the target [tg] to point at a function
    definition and builds a function record for it (see [!type:f]). *)
let record_functions (tg : target) : unit =
  (** [record_functions.writes t]: looks for write operations to global
      variables (see [!Apac_records.globals]) in the term [t] and returns the
      target global variables in a set. *)
  let writes (t : trm) : Var_set.t =
    (** [record_functions.writes.aux t w]: same as [!writes], but hides the
        intermediate result argument [w]. *)
    let aux (w : Var_set.t) (t : trm) : Var_set.t =
      (** [record_functions.writes.aux.one t]: try to resolve the variable
          behind the lvalue term [t] and if it's a global variable, i.e. a
          variable from [!Apac_records.globals], add it to [w] and return the
          resulting set. Otherwise, return [w] as is. *)
      let one (t : trm) : Var_set.t =
        let ll = trm_find_memlocs t in
        List.fold_left (fun w l ->
            if (Var_map.mem l.variable !Apac_records.globals) then
              Var_set.add l.variable w
            else w
          ) w ll
      in
      match t.desc with
      (** If [t] is an assignment or a compound assignment to an [lval]ue or *)
      | Trm_apps (_, [lval; _]) when is_set_operation t -> one lval
      (** if [t] is an increment or decrement unary operation on an [lval]ue,
          process it with [!one]. *)
      | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _}, [lval]) when
             (is_prefix_unary op) || (is_postfix_unary op) -> one lval
      (** In any other case, [t] is not an assignment, so do nothing. *)
      | _ -> w
    in
    (** Launch the discovery of write operations to global variables. *)
    trm_fold aux Var_set.empty t
  in
  Target.iter_at_target_paths (fun t ->
      (** Explode the function definition term into the function name variable
          [f], its return type [ret_ty] and the list of its arguments [args]. *)
      let error = "Apac_preprocessing.record_functions: expected a target to a \
                   function definition!" in
      let (fn, ret_ty, args, body) = trm_inv ~error trm_let_fun_inv t in
      (** If [fn] is a class member method, its first argument in [args] is the
          [this] variable referring to the parent class. Ignore it. *)
      let args =
        (** This is necessary only if [fn] does not refer to the [main] function
            and if it has at least one argument. *)
        if fn.name <> "main" && (List.length args) > 0 then
          (** In this case, extract the first argument of the function and if it
              is [this], discard it. *)
          let (first, _) = List.hd args in
          if first.name = "this" then List.tl args else args
        else args in
      (** Retrieve the global variables from [!Apac_records.globals] as a list
          of typed variables (see type [!type:typed_var]) so as to include them
          into the local scope of the function in the function record (see the
          call to [!Apac_records.FunctionRecord.create] below). *)
      let globs =
        Var_map.fold (fun v (ty, _) acc ->
            (v, ty) :: acc
          ) !Apac_records.globals [] in
      (** Build the function record of [fn] while looking and recording write
          operations to global variables and *)
      let r =
        Apac_records.FunctionRecord.create args globs (writes body) t in
      (** add it to [!Apac_records.functions] if it is not present in the hash
          table already, e.g. in the case of a pre-declaration. *)
      if not (Var_Hashtbl.mem Apac_records.functions fn) then
        Var_Hashtbl.add Apac_records.functions fn r;
      (** Dump the records on the screen, if requested. *)
      if !Apac_flags.verbose then
        Printf.printf "%s = %s\n"
          (var_to_string fn) (Apac_records.FunctionRecord.to_string r)
    ) tg

(** [record_globals]: expect the target [tg] to point at a top-level variable
    definition. To keep track of global variables, this pass stores the target
    variables in the set of global variables [!Apac_records.globals]. *)
let record_globals (tg : target) : unit =
  Target.iter_at_target_paths (fun t ->
      (** Extract the variable [v] from the variable definition term [t]. *)
      let error = "Apac_preprocessing.record_globals: expected a target to a \
                   variable definition!" in
      let (_, v, ty, _) = trm_inv ~error trm_let_inv t in
      (** Add [v] to the record of global variables [!Apac_records.globals]. *)
      Apac_records.globals :=
        Var_map.add v (ty, false) !Apac_records.globals
    ) tg

(** {1:candidate_preselection Candidate pre-selection}

  Nevertheless, we do not necessarily transform the entire input program. We
  focus only on the functions likely to features eligible taskification
  candidates. Before proceeding with any other analysis or transformation pass,
  we mark these functions with [!Apac_macros.candidate_mark]. *)

(** [select_candidates tg]: expects the target [tg] to point at a function
    definition. If the corresponding function meets the requirements of a
    taskification candidate, the pass marks it with
    [!Apac_macros.candidate_mark]. To consider a function a taskification
    candidate, it must feature at least two calls to a function having a record
    in [!Apac_records.functions] or one call to such a function and one loop or
    two loop nests. *)
let select_candidates (tg : target) : unit =
  (** Iterate over each target function definition term [t] and mark each
      function meeting the taskification candidate requirements with
      [!Apac_macros.candidate_mark]. *)
  Target.apply_at_target_paths (fun t ->
      (** Deconstruct the definition term [t] of the function [f]. *)
      let error = "Apac_preprocessing.select_candidates: expected a target to \
                   a function definition!" in
      let (f, _, _, body) = trm_inv ~error trm_let_fun_inv t in
      (** If [t] is in the [!Apac_flags.skip] set, we must not consider it as a
          taskification candidate even if it fulfills the conditions. *)
      if not (Tools.String_set.mem f.name !Apac_flags.skip) then
        (** Count [calls] to functions having a record in
            [!Apac_records.functions] and [loops] in the [body] of [f]. *)
        let (calls, loops) =
          trm_fold (fun (c, l) t ->
              match t.desc with
              (** [t] is a call to a function in [!Apac_records.functions]. *)
              | Trm_apps ({ desc = Trm_var (_, f)}, _) when
                     Var_Hashtbl.mem Apac_records.functions f -> (c + 1, l)
              (** [t] is a loop. *)
              | Trm_for _
                | Trm_for_c _
                | Trm_while _ 
                | Trm_do_while _ -> (c, l + 1) 
              (** Otherwise, there is nothing to count. *)
              | _ -> (c, l)
            ) (0, 0) body in
        (** Check whether it contains at least two calls to function having a
            record in [!Apac_records.functions] or one call to such a function
            and at least one loop. *)
        let candidate =
          if (calls > 1) || (calls > 0 && loops > 0) then
            (** If this condition verifies, we can consider [f] a taskification
                candidate. *)
            true
          else if (loops > 1) then
            (** If it is not the case, but if there are at least two loops in
                the [body] of [f], check the latter for the presence of at least
                two loop nests. If this condition verifies, we can consider [f]
                a taskification candidate. *)
            trm_fold (fun n t ->
                match t.desc with
                (** [t] is a sequence of statements [s]. Check whether the
                    latter features at least two loop nests. *)
                | Trm_seq s ->
                   let l = Mlist.fold_left (fun acc e ->
                               match e.desc with
                               | Trm_for _
                                 | Trm_for_c _
                                 | Trm_while _
                                 | Trm_do_while _ -> acc + 1
                               | _ -> acc
                             ) 0 s in
                   if l > 1 then n || true else n
                (** In any other case, there is nothing to check. *)
                | _ -> n
              ) false body
          else
            (** Otherwise, [f] does not meet the requirements of a taskification
                candidate. *)
            false
        in
        (** If [f] meets the requirements of a taskification candidate, mark its
            definition term [t] with [!Apac_macros.candidate_mark]. Otherwise,
            return [t] as-is. *)
        if candidate then Mark.trm_add_mark Apac_macros.candidate_mark t
        else t
      else t
    ) tg

(** Operating on its abstract syntax tree representation, the passes below
    intend to make the input program ready for the automatic parallelization
    process.

    {1:function_call_extraction Function call extraction}

    Function calls represent our main parallelization target. The goal of the
    [!unfold_function_calls] and [!detach_function_calls] transformation passes
    is to make their identification as task candidates easier. Indeed, we may
    find function calls within variable declaration statements which we do not
    consider as suitable task candidates. The goal of [!detach_function_calls]
    is to detach the initialization, provided it is or it contains a function
    call, from the variable declaration and replace the original statement by
    two new statements, i.e. a variable declaration and a value assignment. If
    the initial declaration qualifies the variable as [const], this property
    must be removed. Moreover, a single statement may feature a nested call
    involving multiple functions. This situation prevents us from considering
    each of the function calls as an individual task candidate. Therefore, the
    pass [!unfold_function_calls] unfolds nested function calls while assigning
    the intermediate return values into temporary variables. *)

(** [unfold_function_calls tg]: expects target [tg] to point at a function call.
    If the function call resides within another function call, the pass then
    separates the target inner function call and the parent function call by
    passing the result from the former to the latter through an interdmediate
    variable.

    For example

    {[
    a = f(g(2));
    ]}
    
    becomes

    {[
    int __apac_var1;
    __apac_var1 = g(2);
    a = __apac_var1;
    ]} *)
let unfold_function_calls (tg : target) : unit =
  Target.iter (fun _ p ->
      (** Check whether the target function call resides within another function
          call. *)
      if (Apac_miscellaneous.has_trm (Path.parent p) (fun t ->
              match t.desc with
              | Trm_apps ({ desc = Trm_var (_ , _); _ }, _) -> true
              | _ -> false
         )) then
        (** If so, define a new intermediate variable, *)
        let var = fresh_var_name ~prefix:Apac_macros.intermediate_variable () in
        (** save the result from the target function call into the intermediate
            variable and pass the latter to the parent function call. *)
        Variable_basic.bind var (target_of_path p)
    ) tg

(** [detach_function_calls tg]: expects target [tg] to point at a variable
    definition. If the latter has an initialization term featuring a function
    call, the transformation pass detaches the initialization of the variable
    from its declaration.

    For example

    {[
    int a = f(2);
    ]}
    
    becomes

    {[
    int a;
    a = f(2);
    ]}
    
    Note that

    {[
    const int * a = foo();
    ]}
    
    becomes
    
    {[
    int * a;
    a = foo();
    ]}

    as we must remove the [const] property from [a]. *)
let detach_function_calls (tg : target) : unit =
  Target.iter (fun t p ->
      (** Extract the variable [v] and the initialization term [init] from the
          target variable definition term. *)
      let error = "Apac_preprocessing.detach_function_calls: expected a target \
                   to a variable definition!" in
      let (_, v, _, init) =
        trm_inv ~error trm_let_inv (Path.get_trm_at_path p t) in
      (** If [init] features a function call, *)
      if (trm_fold (fun acc t ->
              match t.desc with
              | Trm_apps ({ desc = Trm_var (_ , _); _ }, _) -> acc || true
              | _ -> acc || false
            ) false init) then
        begin
          (** detach the declaration and the initialization of [v] while
              removing the [const] property of [v] if present.*)
          let tg = Target.target_of_path p in
          Variable_basic.to_nonconst tg;
          Variable_basic.init_detach tg
        end
    ) tg

(** {1:statement_body_normalization Statement body normalization}

    Iteration and if-statements involve substatements, i.e. a loop body as well
    as a then and optionally an else branch, respectively. For the sake of
    consistency, when building a task candidate graph, we expect such a
    substatement to be a compund statement. However, in the input source code,
    it is not always the case. Therefore, we rely on the
    [!normalize_statement_bodies] transformation pass to ensure that all the
    loop bodies and if-conditional branches consist of compound statements. *)

(** [normalize_statement_bodies tg]: expects target [tg] to point at an
    iteration statement or an if-conditional. If the body, or the branches, of
    the latter do not consist of a compound statement, the transformation places
    the body of the iteration statement or the branches of the if-conditional
    into compound statements.

    For example

    {[
    for (int jj = kk + 1; jj < bots_arg_size; jj++)
      if (BENCH[kk * bots_arg_size + jj] != NULL)
      {
        fwd(BENCH[kk*bots_arg_size+kk], BENCH[kk*bots_arg_size+jj]);
      }
    ]}
    
    becomes

    {[
    for (int jj = kk + 1; jj < bots_arg_size; jj++) {
      if (BENCH[kk * bots_arg_size + jj] != NULL)
      {
        fwd(BENCH[kk*bots_arg_size+kk], BENCH[kk*bots_arg_size+jj]);
      }
    }
    ]}. *)
let normalize_statement_bodies (tg : target) : unit =
  Target.apply_at_target_paths (fun t ->
      match t.desc with
      | Trm_for (range, body, _) when not (is_trm_seq body) ->
         trm_for ~annot:t.annot range (trm_seq (Mlist.of_list [body]))
      | Trm_for_c (init, cond, inc, body, _) when not (is_trm_seq body) ->
         trm_for_c
           ~annot:t.annot init cond inc (trm_seq (Mlist.of_list [body]))
      | Trm_while (cond, body) when not (is_trm_seq body) ->
         trm_while ~annot:t.annot cond (trm_seq (Mlist.of_list [body]))
      | Trm_do_while (body, cond) when not (is_trm_seq body) ->
         trm_do_while ~annot:t.annot (trm_seq (Mlist.of_list [body])) cond
      | Trm_if (cond, yes, no) when
             not (is_trm_seq yes) &&
               not (is_trm_unit no) && not (is_trm_seq no) ->
         trm_if
           ~annot:t.annot cond
           (trm_seq (Mlist.of_list [yes])) (trm_seq (Mlist.of_list [no]))
      | Trm_if (cond, yes, no) when
             not (is_trm_seq yes) &&
               ((is_trm_unit no) || (is_trm_seq no)) ->
         trm_if ~annot:t.annot cond (trm_seq (Mlist.of_list [yes])) no
      | Trm_if (cond, yes, no) when
             (is_trm_seq yes) &&
               not (is_trm_unit no) && not (is_trm_seq no) ->
         trm_if ~annot:t.annot cond yes (trm_seq (Mlist.of_list [no]))
      | _ -> t
    ) tg

(** {1:constification Constification}

    The presence of the [const] qualifier in a function prototype helps us to
    determine whether the function modifies a given argument by side-effect,
    i.e. through a pointer or a reference, and classify the argument as
    read-write or read-only during the data dependency discovery (see
    [!Apac_taskify.trm_discover_dependencies] and [!Apac_taskify.taskify_on]).
    In C++ specifically, functions representing class member methods may carry
    the [const] qualifier too. In this case, it indicates that the method does
    not modify any of the class member variables.

    However, the programmers do not always make use of the [const] qualifier
    which makes the data dependency discovery in function calls harder. The goal
    of the constification is to add the [const] qualifier to arguments in
    function prototypes wherever it is possible and useful.

    For each function definition in the input abstract syntax tree, we build a
    function constification record of type [!type:Constification.f] featuring a
    separate argument constification record of type [!type:Constification.a] for
    each argument in the function prototype. We use these records throughout the
    constification process to determine to which arguments and functions, in the
    case of class member methods, we can add the [const] qualifier at the end.
    Note that we store the function constification records in a hash table of
    type [!type:Constification.r].

    {2:principle Working principle}

    At the beginning of the constification, we assume every argument in every
    function constifiable (see [!Apac_records.build_records]). We then perform
    an analysis of dependencies between function arguments and write operations
    involving the latter within the bodies of corresponding functions (see
    [!analyze]). When the analysis concludes an argument is written to, it
    modifies its constification record so as to mark the argument as
    unconsitifiable, i.e. passes the [const] element of the corresponding record
    of type [!type:Constification.a] to [false]. The same happens for class
    member methods modifying sibling class member variables (see the [const]
    element of function constification records of type
    [!type:Constification.f]). However, we do not modify the constification
    records during the analysis. Instead, we use a stack of type
    [!type:Constification.u] keeping trace of arguments and functions to
    unconstify at the end of the data dependency analysis on arguments and class
    memebers. When we pass an object as argument to a function, we must not
    constify it if it calls a non-[const] class member method. However, we do
    not have this information before processing the elements in the
    [!type:Constification.u] stack (see [!unconstify]). This is why we need a
    second phase to process the elements of a [!type:Constification.o] stack
    (see [!Constification.unconstify]).

    {3:aliases Aliasing}

    Pointers and references within a function definition might alias the same
    data as the arguments of the function. Therefore, when we constify an
    argument, we must constify its aliase(s) too. To keep track of argument
    aliases, we use a hash table of type [!type:Constification.l] (see
    [!Constification.find_unconstifiable]).

    {3:multi Multiple variable declarations}

    When we constify an argument, we must propagate the constification to the
    aliases of the argument as well (see type [!type:Constification.l] and
    [!Constification.constify_arguments]). This process is straightforward in
    the case of simple variable declarations. However, if an argument alias
    appears in a multiple variable declaration, we have two different situations
    to consider:

    1. the data type of the multiple variable declaration is already constant,
   
    {[ // i is a constant integer
       // j is a mutable pointer to a constant integer
       int const i, * j; ]}

    2. the data type of the multiple variable declaration is not constant.

    {[ // i is a mutable integer
       // j is a mutable pointer to a mutable integer
       int i, * j; ]}

    If we want to constify [j] in the first situation, it is possible without
    breaking the multiple variable declaration:

    {[int const i, * const j;}]

    In the second case, we cannot fully constify [j], i.e. [int const * const
    j], without constifying [i], which is not desirable. In this case, we have
    to split the multiple variable declaration into a sequence of simple
    variable declarations and then constify the declaration of [j] only.
    However, we cannot apply this transformation directly applied within
    [!Constification.constify_aliases]. Indeed, the introduction of a new
    sequence of instructions changes the scope of the declared variables. We
    cannot prevent this in a term to term transformation function such as
    [!Constification.constify_aliases], but in a transformation function
    modyfing the abstract syntax tree by side-effect, i.e. a target to unit
    transformation, in which we can call [!Nobrace_transfo.remove_after] to
    effectively remove the braces from the sequence of simple variable
    declarations so as to preserve their initial scope. Therefore,
    [!Constification.constify_aliases] should only mark those multiple variable
    declarations which need to be split into simple variable declarations and
    determine those to constify. To keep track of this information, so we can
    actually perform the transformations (using
    [!Constification.nfold_let_mult]), we use a hash table of type
    [!type:Constification.m]. *)

(** {2:module Module}

    Constification is a complex analysis and transformation pass consisting of
    many data types and function. We thus dedicate it an entire module. *)

module Constification : sig
  type r
  val constify_prototypes : ?crs:(r option) -> target -> unit
  val constify_aliases : ?crs:(r option) -> target -> unit
  val constify : ?frs:(Apac_records.FunctionRecord.t Var_Hashtbl.t option) ->
                 ?trans:bool -> target -> unit
end = struct
  (** [k]: variable kinds (simple variable, reference or pointer). *)
  type k =
    (** The variable is a simple variable. *)
    | Variable
    (** The variable is a pointer, e.g. [int * a]. *)
    | Pointer
    (** The variable is a reference, e.g. [int & a]. *)
    | Reference
    (** The variable is an array, e.g. [int a\[N\]]. *)
    | Array

  (** [a]: an argument constification record. *)
  type a = {
      (** [self]: gives the kind of the argument (see [!type:k]). *)
      self : k;
      (** [const]: decides on the constification of the argument. *)
      mutable const : bool;
      (** [propagate]: a map of function variables of type [!type:var] (keys are
          0-based argument positions of type [!type:int]).
          
          If the argument is a reference or a pointer, i.e. when [!kind] is
          either a [Pointer] or a [Reference], and if the function modifies the
          value in memory it refers or points to, we must not constify the
          argument. Let us explain [propagate] on an example. Let [g] be a
          function we define as follows:
          
          {[
          void g(int &v) {
            v += 4;
          }
          ]}
          
          and [f] a function we define as follows:
          
          {[
          int f(int a, int b) {
            g(b);
            return a + b;
          }
          ]}
          
          In [g], [v] is a reference and the function modifies the target value.
          Therefore, we must not constify the argument [v]. However, [f] calls
          [g] and passes its argument [b] by reference to [g]. This way, [g]
          shall modify the value behind [b]. We already know we must not
          constify [v] in [g]. Due to this dependency, we must propagate this
          decision also to [b] in [f]. To achieve this and keep track of the
          dependency, we add [(f, 1)] to the [propagate] map of the argument
          constification record of [v] in [g]. *)
      mutable propagate : int Var_map.t;
    }
  
  (** [f]: a function constification record. *)
  type f = {
      (** [args]: a map of argument constification records (see [!type:a], keys
          are 0-based argument positions of type [!type:int]). *)
      args : a Tools.Int_map.t;
      (** [const]: decides on the constification of the function (relevant only
          for class member methods). *)
      mutable const : bool;
      (** [kind]: determines the return value kind (see [!type:k]). *)
      return : k;
      (** [member]: tells whether the function is a class member method. *)
      mutable member : bool
    }
  
  (** [r]: a type of hash table for function constification records (see
      [!section:intro]). *)
  type r = f Var_Hashtbl.t
  
  (** [l]: a type of hash table of argument aliases (see [!section:aliases]).
      
      Keys are the [!type:lvar] of the alias and values are pairs of two
      [!type:int] elements. The first corresponds to the 0-based position of the
      function argument being aliased and the second gives alias' number of
      levels of indirection, e.g. 2 in the case of [int ** tab] and 0 in the
      case of [int a]. *)
  type l = (int * int) LVar_Hashtbl.t
  
  (** [u]: type of stack of arguments, except for objects, we must not constify
      (see [!section:principle]).
      
      Elements are ([!type:var], [!type:int]) pairs where the [!type:var]
      element identifies the target function and the [!type:int] element
      identifies the position of the argument to unconstify. If the latter is
      set to -1, it means we have to unconstify the function itself. *)
  type u = (var * int) Stack.t
  
  (** [o]: type of stack of arguments, representing objects, we must not
      constify (see [!section:principle]).
      
      Elements are ([!type:var], [!type:int], [!type:var]) triplets where the
      first element identifies the target function, the second element gives the
      0-based position of the argument to unconstify and the third element
      represents the class member method called on that argument. Then, during
      unconstification, if we unconstify the method behind the third element, we
      do so for the argument (the second element) of the function behind the
      first element too. *)
  type o = (var * int * var) Stack.t

  (** [k_to_string kind]: returns a string representation of the [kind]. *)
  let k_to_string (kind : k) : string =
    match kind with
    | Variable -> "Variable"
    | Pointer -> "Pointer"
    | Reference -> "Reference"
    | Array -> "Array"

  (** [a_to_string ?indent record]: returns a string representation of the
      argument constification [record]. The [indent] flag allows for turning on
      additional indentation in the output string. This is useful when calling
      the function from within [!f_to_string]. *)
  let a_to_string ?(indent : bool = false) (record : a) : string =
    let kind = k_to_string record.self in
    let propagate =
      Var_map.fold (fun func i acc ->
          acc ^ (if indent then "\t\t\t\t" else "") ^
            "(" ^ (var_to_string func) ^ ", " ^ (string_of_int i) ^ "),\n"
        ) record.propagate "\n" in
    let propagate =
      let l = String.length propagate in
      if l > 2 then String.sub propagate 0 (l - 2) else "empty" in
    "{\n" ^ (if indent then "\t\t" else "") ^ "\tself: " ^ kind ^ ",\n" ^
      (if indent then "\t\t" else "") ^ "\tconst: " ^
        (if record.const then "yes" else "no") ^ ",\n" ^
          (if indent then "\t\t" else "") ^ "\tpropagate: " ^
            propagate ^ "\n" ^ (if indent then "\t\t" else "") ^ "}"
  
  (** [f_to_string record]: returns a string representation of the function
      constification [record]. *)
  let f_to_string (record : f) : string =
    let args =
      Tools.Int_map.fold (fun i arg acc ->
          acc ^ "\t\t" ^ (string_of_int i) ^ ": " ^
            (a_to_string ~indent:true arg) ^ ",\n"
        ) record.args "\n" in
    let args =
      let l = String.length args in
      if l > 2 then String.sub args 0 (l - 2) else "empty" in
    "{\n\targs: " ^ args ^ "\n\tconst: " ^
      (if record.const then "yes" else "no") ^ ",\n\tmember: " ^
        (if record.member then "yes" else "no") ^ "\n}"

  (** [typ_constify ty]: augment the type [ty] with the [const] qualifier so as
      to prevent a function with an argument of type [ty] to alter it by
      side-effect. Basically, if [ty] is a pointer or a reference type, i.e. we
      do not simply pass the argument by value, [typ_constify] ensures [ty]
      refers to [const] data, e.g. from [int * arg] it makes [const int * arg],
      but in the case of [int arg], it leaves [ty] unchanged. *)
  let typ_constify (ty : typ) : typ =
    (** Alias values we refer a lot. *)
    let annot = ty.typ_annot in
    let attributes = ty.typ_attributes in
    (** [typ_constify.aux ty]: auxiliary function to recursively constify [ty],
        e.g. [int ** arg] becomes [const int * const * arg]. *)
    let rec aux (ty : typ) : typ =
      match ty.typ_desc with
      (** When [ty] is a pointer, add the [const] qualifier to it. *)
      | Typ_ptr { ptr_kind = Ptr_kind_mut; inner_typ = ty } ->
         typ_const (typ_ptr ~annot ~attributes Ptr_kind_mut (aux ty))
      (** When [ty] is a constant pointer, add the [const] qualifier to the
          inner type. *)
      | Typ_const { typ_desc =
                      Typ_ptr { ptr_kind = Ptr_kind_mut; inner_typ = ty };
                    typ_annot = annot;
                    typ_attributes = attributes } ->
         typ_const (typ_ptr ~annot ~attributes Ptr_kind_mut (aux ty))
      (** When [ty] is a user-defined constructed type and *)
      | Typ_constr (_, id, _) ->
         begin match Context.typid_to_typedef id with
         (** if it is a [typedef] declaration *)
         | Some td ->
            begin match td.typdef_body with
            (** aliasing an another type, try to constify the latter. *) 
            | Typdef_alias ty -> aux ty
            (** Otherwise, constify the constructed type as is. *)
            | _ -> typ_const ty
            end
         | None -> typ_const ty
         end
      (** When [ty] is already of constant type. There is nothing to do. *)
      | Typ_const _ -> ty
      (** In any other case, add the [const] qualifier to [ty]. *)
      | _ -> typ_const ty
    in
    (** [typ_constify.first ty]: auxiliary function to skip the constification
        of the 0-th level of indirection of [ty] at the beginning of the
        process, e.g. we want [int ** arg] to become [const int * const * arg]
        and not [const int * const * const arg] so as [int arg] to stay [int
        arg]. A function always receives this value as a copy. *)
    let first (ty : typ) : typ =
      match ty.typ_desc with
      (** When [ty] is already of constant type, there is nothing to skip. We
          just have to ensure that the inner types of [ty] are [const] too. *)
      | Typ_const _ -> aux ty
      (** When [ty] is a reference, a pointer or an array, launch the
          constification on the inner types. *)
      | Typ_ptr { ptr_kind = Ptr_kind_ref; inner_typ = ty } ->
         typ_ref ~annot ~attributes (aux ty)
      | Typ_ptr { ptr_kind = Ptr_kind_mut; inner_typ = ty; } ->
         typ_ptr ~annot ~attributes Ptr_kind_mut (aux ty)
      | Typ_array (ty, s) -> typ_array ~annot ~attributes (aux ty) s
      (** Otherwise, we do not constify anything and leavy [ty] as is. *)
      | _ -> ty
    in
    (** If [ty] is a user-defined constructed type, *)
    match ty.typ_desc with
    | Typ_constr (_, id, _) ->
       begin match Context.typid_to_typedef id with
       | Some td ->
          begin match td.typdef_body with
          (** try to identify which type it is aliasing before launching the
              constification. *)
          | Typdef_alias ty -> first ty
          | _ -> ty
          end
       (** If [ty] is not an alias, simply constify [ty] as is. *)
       | None -> typ_const ty
       end
    (** Otherwise, launch the constification. *)
    | _ -> first ty

  (** [arg_explode v ty]: if the type [ty] of the argument variable [v]
      represents a structure or a class, this function returns a list of
      labelled variables (see type [!type:lvar]) corresponding to each member of
      the structure or the class. If a member of the latter is itself another
      structure or class, the function explores it recursively.

      For example, let us consider two structure types [Point] and [Polygone] we
      define as follows:

      {[
      typedef struct {
      int x;
      int y;
      } Point;

      typedef struct {
      Point* pts;
      long unsigned int angles;
      } Polygone;
      ]}

      For an argument variable [Polygone * poly], this function returns the list
      [(poly, poly#pts, poly#pts#x, poly#pts#y, poly#angles)]. *)
  let arg_explode (v : var) (ty : typ) : (lvar * typ) list =
    (** [arg_explode.inner ty]: an auxiliary function returning the inner type
        of [ty] when [ty] is a pointer type, const type or an array type. *)
    let rec inner (ty : typ) : typ =
      match ty.typ_desc with
      | Typ_const ty -> inner ty
      | Typ_ptr {inner_typ = ty; _} -> inner ty
      | Typ_array (ty, _) -> inner ty
      | _ -> ty
    in
    (** [arg_explode.core v l ty lvs]: an auxiliary function allowing us to
        recursively explore the type [ty] of the argument variable [v] and to
        build a list of L-variables (see type [!type:lvar]) and their types
        [lvs], with [v] as the variable component and [l] as the label
        component, corresponding to each member of a structure or a class within
        [ty]. *)
    let rec core (v : var) (l : label) (ty : typ)
              (lvs : (lvar * typ) list) : (lvar * typ) list =
      match ty.typ_desc with
      (** When [ty] is a user-defined record type *)
      | Typ_constr (_, id, _) ->
         begin match Context.typid_to_typedef id with
         | Some td ->
            begin match td.typdef_body with
            | Typdef_record fs ->
               (** recursively explore its members in order to represent them as
                   labelled variables we add to [lvs]. *)
               List.fold_left (fun acc (rf, _) ->
                   match rf with
                   | Record_field_member (l', ty') ->
                      let l'' = if l <> "" then l ^ "#" ^ l' else l' in
                      let lv : lvar = { v = v; l = l''} in
                      acc @ ((lv, ty') :: (core v l'' ty' lvs))
                   | _ -> acc
                 ) lvs fs
            (** Other types cannot have members, so there is nothing to explore,
                we can return [lvs]. *)
            | _ -> lvs
            end
         (** The same goes for user-defined types simply aliasing atomic
             types. *)
         | None -> lvs
         end
      (** When [ty] is a class, a structure or a union, explore the underlying
          record type. *)
      | Typ_record (_, ty) -> core v l ty lvs
      (** In any other case, there are no members to explore. *)
      | _ -> lvs
    in
    (** Get the inner type of [ty], i.e. strip pointer, reference, constant and
        array types from [ty]. *)
    let ty = inner ty in
    (** Build the top-level labelled variable from [v]. *)
    let lv : lvar = { v = v; l = "" } in
    (** When [ty] is a user-defined record type, a class, a structure or a
        union, explore its members. *)
    match ty.typ_desc with
    | Typ_constr _
      | Typ_record _ -> (lv, ty) :: (core v "" ty [])
    (** Otherwise, simply return the toplevel labelled variable [lv]. *)
    | _ -> [(lv, ty)]

  (** [aliasing a ll]: taking into account the hash table of aliases [a] (see
      [!type:a]), the function filters the list of memory locations [ll] (see
      [!type:memloc]) so as to keep only memory locations representing an
      argument or an alias to an argument. It then transforms the resulting list
      of memory locations into the list of pairs of L-variables and the
      corresponding 0-based positions of the arguments they alias. *)
  let rec aliasing (a : l) (ll : memloc list) : (lvar * int) list =
    match ll with
    | hd :: tl ->
       let lv : lvar = { v = hd.variable; l = hd.label } in
       let alias = LVar_Hashtbl.find_opt a lv in
       if Option.is_some alias then
         let tg, _ = Option.get alias in
         (lv, tg) :: (aliasing a tl)
       else aliasing a tl
    | [] -> []

  (** [targeting a v l nli]: checks in the hash table of aliases [a] (see
      [!type:l]) whether an L-variable consisting of the variable [v] and the
      label [l], subject to [-n] dereferencements, is an argument or an alias to
      an argument and if it represents a potential aliasing target. If so, it
      returns the L-variable as well as the corresponding 0-based position of
      the argument it aliases. *)
  let targeting (a : l) (v : var) (vk : varkind) (l : label) (n : int) :
        (lvar * int) option =
    let lv : lvar = { v = v; l = l } in
    match LVar_Hashtbl.find_opt a lv with
    | Some (tg, nli) when nli <> (- n) -> Some (lv, tg)
    | _ -> None

  (** [find_parent_typedef_record p]: goes back up the path [p] and returns the
      first term corresponding to a class or a structure definition, if any. We
      use this function to determine the parent class of a structure or a
      function in order to access to its members. *)
  let find_parent_typedef_record (p : path) : trm option =
    (** We shall go back on our steps in the path, i.e. in the direction of the
        root of the abstract syntax tree, so we need to reverse [p]. *)
    let r = List.tl (List.rev p) in
    (** [find_parent_typedef_record.aux r]: an auxiliary function hiding the
        path reversal. Here, the [r] parameter refers to the reversed path. *)
    let rec aux (r : path) : trm option =
      (** Recursively inspect [r] and *)
      match r with
      | e :: f -> 
         begin
           (** if we detect a class or a structure definition, return it. *)
           let tg = target_of_path (List.rev r) in
           let t = get_trm_at_exn tg in
           match t.desc with
           | Trm_typedef { typdef_body = Typdef_record _; _ } -> Some (t)
           | _ -> aux f
         end
      | [] -> None
    in
    (** Call the auxiliary function. *)
    aux r

  (** [missing_record f a]: generates an error message telling that there is no
      constification record for the argument at a position [i] in the case of a
      function [f] and fails. [l] represents the name of the calling function to
      include in the message. If [i] is [-1], the resulting message states that
      no constification record exist for the function [f] all together. *)
  let missing_record (l : string) (f : var) (i : int) : string =
    if i < 0 then
      Printf.sprintf
        "Apac_preprocessing.Constification.%s: there is no constification \
         record of '%s'."
        l (var_to_string f)
    else 
      Printf.sprintf
        "Apac_preprocessing.Constification.%s: the constification record of \
         '%s' has no argument constification record for the argument on \
         position '%d'." l (var_to_string f) i

  (** [build_records_on ]: see [!build_records]. *)
  let build_records_on (crs : r) (t : trm) : unit =
    (** Deconstruct the function definition term [t]. *)
    let error = "Apac_preprocessing.Constification.build_records_on: expected \
                 a target to a function definition!" in
    let (f, ret_ty, args, _) = trm_inv ~error trm_let_fun_inv t in
    (** If the function [f] is a class member method, its first argument is the
        [this] variable referring to the parent class. Ignore it. *)
    let args =
      (** This is necessary only if [fn] does not refer to the [main] function
          and if it has at least one argument. *)
      if f.name <> "main" && (List.length args) > 0 then
        (** In this case, extract the first argument of the function and if it
            is [this], discard it. *)
        let (first, _) = List.hd args in
        if first.name = "this" then List.tl args else args
      else args in
    (** Create constification records for the function's arguments. *)
    let args = List.mapi (fun i (arg, ty) ->
                   (i, {
                      self = if (is_typ_ptr ty) then Pointer
                             else if (is_typ_ref ty) then Reference
                             else if (is_typ_array ty) then Array
                             else Variable;
                      const = true;
                      propagate = Var_map.empty
                 })) args in
    let args = Tools.Int_map.of_seq (List.to_seq args) in
    (** Create the constification record for the function itself *)
    let fcr : f = {
        args = args;
        const = true;
        return = if (is_typ_ptr ret_ty) then Pointer
                 else if (is_typ_ref ret_ty) then Reference
                 else if (is_typ_array ret_ty) then Array
                 else Variable;
        member = false
      } in
    (** and add it to the hash table of function constification records [crs] if
        it is not present there already, e.g. in the case of a
        pre-declaration. *)
    if not (Var_Hashtbl.mem crs f) then
      Var_Hashtbl.add crs f fcr;
    (** Dump the record on the screen, if requested. *)
    if !Apac_flags.verbose then
      Printf.printf "%s = %s\n"
        (var_to_string f) (f_to_string fcr)
  
  (** [build_records crs tg]: expects the target [tg] to point at a function
      definition, builds a function constification record for it and stores the
      record in the hash table of function constification records [crs] (see
      type [!type:r]). *)
  let build_records (crs : r) (tg : target) : unit =
    Target.iter_at_target_paths (build_records_on crs) tg

  (** [analyze_on us ous crs p t]: see [!analyze]. *)
  let analyze_on (us : u) (ous : o) (crs : r) (p : path) (t : trm) : unit =
    (** [analyze_on.aux aliases f t]: auxiliary function to recursively visit
        the abstract syntax tree term [t] within a function [f] in order to
        resolve dependencies between arguments and aliases. When first calling
        this function, [t] should be the statement sequence representing [f]'s
        body. *)
    let rec aux (aliases : l) (f : var) (t : trm) : unit =
      match t.desc with
      (** When [t] is a compound, an interation or a selection statement
          involving substatements, loop over the latter. *)
      | Trm_seq _
        | Trm_for _
        | Trm_for_c _
        | Trm_if _
        | Trm_switch _
        | Trm_while _ -> trm_iter (aux aliases f) t
      (** When [t] is a call to a function [f'], we may have to update the
          [propagate] elements in the constification records of its [args]. *)
      | Trm_apps ({ desc = Trm_var (_ , f'); _ }, args) ->
         (** If we have a function constification record for [f'], *)
         if Var_Hashtbl.mem crs f' then
           begin
             (** gather it. *)
             let fcr = Var_Hashtbl.find crs f' in
             (** In the case of a call to a class member method, the first
                 argument is the [this] variable referring to the parent class
                 instance, e.g. in [this->p(i, j)] the first argument is [this]
                 and in [a.f(i, j)] the first argument is [a]. In other terms,
                 there is an [extra] argument at the beginning of [args]. *)
             let extra =
               (List.length args) > (Tools.Int_map.cardinal fcr.args) in
             if extra then
               begin
                 (** If we are calling a class member method and if the parent
                     is not [this], we may have to unconstify the argument (see
                     [!unconstify]). For now, we do not take into account class
                     member variables. *)
                 let ll = trm_find_memlocs (List.hd args) in
                 let lva = aliasing aliases ll in
                 List.iter (fun (lv, tg) ->
                     if lv.v.name <> "this" then
                       Stack.push (f, tg, f') ous
                   ) lva
               end;
             (** For each argument of the function call (except for [this]), *)
             List.iteri (fun i arg ->
                 (** we gather the constification record of the [i']-th argument
                     of [f']. *)
                 if (Tools.Int_map.mem i fcr.args) then
                   begin
                     let acr = Tools.Int_map.find i fcr.args in
                     (** Then, for each L-variable [lv] we find in [arg], we
                         have to check whether it aliases an argument [tg] of
                         [f]. If so, we may have to unconstify the [tg]-th
                         argument of [f] the variable [lv] behind the [arg] it
                         is aliasing. *)
                     let ll = trm_find_memlocs arg in
                     let lva = aliasing aliases ll in
                     List.iter (fun (_, tg) ->
                         acr.propagate <- Var_map.add f tg acr.propagate
                       ) lva
                   end
                 else
                   (** If it's not possible, fail. This is not normal! *)
                   fail t.loc (missing_record "analyze_on.aux" f i)
               ) (if extra then (List.tl args) else args);
           end
         (** When we do not have a constification record for [f'], it means that
             we do not know the definition of [f'] and thus, we are not able to
             perform the constification as usual. In this case, we consider that
             [f'] may alter any of its arguments. *)
         else
           begin
             (** Let us warn the user about that. *)
             Printf.printf
               "[APAC] [Warning] Missing definition of `%s', considering the \
                function may alter any of its arguments.\n" f'.name;
             (** Then, for each argument [arg] of the function call and *)
             List.iter (fun arg ->                 
                 (** for each L-variable [lv] we find in [arg], we have to check
                     whether it aliases an argument [tg] of [f]. If so, we may
                     have to unconstify the [tg]-th argument of [f] the variable
                     [lv] behind the [arg] it is aliasing. *)
                 let ll = trm_find_memlocs arg in
                 let lva = aliasing aliases ll in
                 List.iter (fun (_, tg) -> Stack.push (f, tg) us) lva
               ) args
           end;
         (** Continue the analysis on substatements, if any. *)
         trm_iter (aux aliases f) t
      (** When [t] is a declararion of a variable [v] of type [ty] with an
          initialization term [ti], *)
      | Trm_let (_, (v, ty), { desc = Trm_apps (_, [ti]); _ }, _) ->
         (** we try to identify each memory location in [ti] representing an
             argument or an alias to an argument [tg] of [f] and we update the
             hash table of [aliases] accordingly, if necessary. *)
         let ll = trm_find_memlocs ti in
         let lva = aliasing aliases ll in
         List.iter (fun (_, tg) ->
             let alias : lvar = { v = v; l = String.empty } in
             LVar_Hashtbl.add
               aliases alias (tg, Apac_miscellaneous.typ_get_nli ty)
           ) lva;
         (** We then continue the analysis on substatements, if any. *)
         trm_iter (aux aliases f) t
      (** When [t] is an assignment or a compound assignment of an [rval] to an
          [lval] term, we may have to update the unconstification stack [us]. *)
      | Trm_apps (_, [lval; rval]) when is_set_operation t ->
         (** We are modifying [lval] by assignment. Find the underlying memory
             locations [ll]. *)
         let ll = trm_find_memlocs lval in
         (** Then, for each memory location [l] in [ll] we do the follwing. *)
         List.iter (fun l ->
             (** We build the L-variable corresponding to [l]. *)
             let lv : lvar = { v = l.variable; l = l.label } in
             (** If [lv] is [this], it means that [f] is modifying a member
                 variable of the parent class. Therefore, we will have to
                 unconstify [f]. *)
             if lv.v.name = "this" then Stack.push (f, -1) us;
             (** If [lv] is an argument or an alias to an argument, *)
             if LVar_Hashtbl.mem aliases lv then
               begin
                 (** determine the position [tg] of the argument [lv] stands for
                     or is aliasing together with the number of levels of
                     indirection [nli] of the latter. *)
                 let tg, nli = LVar_Hashtbl.find aliases lv in
                 if tg < 0 then
                   Stack.push (f, -1) us;
                 (** Check whether [lv] simply becomes an [alias] to another
                     target. *)
                 let alias =
                   match l.kind with
                   | Var_mutable when l.dereferencements + 1 < nli ->
                      true
                   | Var_immutable when lv.l = "" && l.dereferencements < 0 ->
                      true
                   | Var_immutable when
                          lv.l <> "" && l.dereferencements < nli ->
                      true 
                   | _ -> false
                 in
                 if alias then
                   begin
                     (** If so, for each memory location representing an
                         argument or an alias to an argument [tg] we find in
                         [rval], we add a new entry into [aliases]. This
                         happens, for example, on L3 in the example below. *)
                     let rval = trm_find_memlocs rval in
                     let rval = aliasing aliases rval in
                     List.iter (fun (_, tg) ->
                         if !Apac_flags.verbose then
                           Printf.printf "Constification of `%s': retargeting \
                                          alias %s (%d dereferencements, %d \
                                          levels of indirection)\n"
                             f.name (LVar.to_string lv) l.dereferencements nli;
                         LVar_Hashtbl.add aliases lv (tg, nli)
                       ) rval
                   end
                 else
                   (** Otherwise, it means that we are modifying a function
                       argument by side-effect through an alias. Therefore, we
                       have to unconstify the target argument. Note that an
                       alias of the same name may have been used multiple times
                       to alias different memory locations.
                       
                       For example, in:
                       
                       [{
                       L1: void f(int * a, int * b, int * c) {
                       L2:   int * d = a;
                       L3:   d = c; 
                       L4:   *d = 1;
                       L5: }
                       ]}
                       
                       [d] is declared as an alias to [a] on L2. The function
                       does not modify The data pointed to by [a]. On L3, [d]
                       becomes an alias for [c]. Then, on L4, the function
                       modifies the data pointed to by [c] through its alias
                       [d]. Therefore, the analysis concludes that nor [c] nor
                       [d] should be constified. However, for [a] it concludes
                       that we can constify the argument as the function never
                       modifies the data it is pointing to. In the end, this
                       produces a compilation error as the function assignes
                       [a], which became [const], to the non-[const] variable
                       [d] on L2.
                       
                       In order to avoid this situation, we must propagate the
                       unconstification to previous aliases too. As [aliases]
                       stores all the values that were ever assigned to a given
                       key, we simply have to [find_all] of them and push them
                       to the unconstification stack. *)
                   let all = LVar_Hashtbl.find_all aliases lv in
                   if !Apac_flags.verbose then
                     Printf.printf "Constification of `%s': modifying alias %s \
                                    (%d dereferencements, %d levels of \
                                    indirection)\n"
                       f.name (LVar.to_string lv) l.dereferencements nli;
                   List.iter (fun (tg, _) ->
                       (** Again, we do not consider parent class members
                           because the constification process does not
                           analyze entire classes for now. *)
                       if tg > -1 then Stack.push (f, tg) us
                     ) all
               end
             else
               (** When [lv] is not an argument and it is not an alias yet, we
                   look for any memory location [rv] in [rval] and check if it
                   is an argument or an alias to an argument [tg]. If so, we
                   have to add a new entry into [aliases]. As we do not know the
                   number of levels of indirections of [lval], we use the number
                   of levels of indirection [nli] of [rv]. *)
               let rval = trm_find_memlocs rval in
               let rval = aliasing aliases rval in
               List.iter (fun (rv, tg) ->
                   let _, nli = LVar_Hashtbl.find aliases rv in
                   LVar_Hashtbl.add aliases lv (tg, nli)
                 ) rval
           ) ll;
         (** Continue the analysis on substatements, if any. *)
         trm_iter (aux aliases f) t
      (** When [t] is an increment or decrement unary operation on an operand
          [o], we may have to update the unconstification stack [us]. *)
      | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _}, [o]) when
             (is_prefix_unary op) || (is_postfix_unary op) ->
         (** We are modifying [o] by assignment. For each memory location [lv]
             in [o], check whether it is an argument or an alias to an argument
             [tg]. If so, we must unconstify it and propagate this decision to
             all previous aliases. Again, we do not consider parent class
             members because the constification process does not analyze entire
             classes for now. *)
         let o = trm_find_memlocs o in
         let o = aliasing aliases o in
         List.iter (fun (lv, tg) ->
             let all = LVar_Hashtbl.find_all aliases lv in
             List.iter (fun (tg, _) ->
                 if tg > -1 then Stack.push (f, tg) us
               ) all
           ) o;
         (** Continue the analysis on substatements, if any. *)
         trm_iter (aux aliases f) t
      (** When [t] is a return statement returning [rt], we have to update the
          unconstification stack [us] if the return type of [f] is a pointer, a
          reference or an array access, i.e. it is not a simple variable, *)
      | Trm_abort (Ret (Some rt)) ->
         (** Acquire the constification record of [f]. If it does not exist,
             fail. This is not normal! *)
         if not (Var_Hashtbl.mem crs f) then
           fail t.loc (missing_record "analyze_on.aux" f (-1));
         let fcr = Var_Hashtbl.find crs f in
         (** If the return type of [f] is a pointer or a reference, *)
         if fcr.return = Reference || fcr.return = Pointer then
           begin
             (** we check for each memory location in [rt] whether it is an
                 argument or an alias to an argument [tg]. If so, we must
                 unconstify it. Again, we do not consider parent class members
                 because the constification process does not analyze entire
                 classes for now. *)
             let rt = trm_find_memlocs rt in
             let rt = aliasing aliases rt in
             List.iter (fun (_, tg) ->
                 if tg > -1 then Stack.push (f, tg) us
               ) rt
           end;
         (** Continue the analysis on substatements, if any. *)
         trm_iter (aux aliases f) t
      (** When [t] is none of the above, try continuing the analysis on
          substatements, if any. *)
      | _ -> trm_iter (aux aliases f) t
    in
    (** Deconstruct the definition term [t] of the function [f]. *)
    let error = "Apac_preprocessing.Constification.analyze_on: expected target \
                 to a function definition." in
    let (f, ret_ty, args, body) = trm_inv ~error trm_let_fun_inv t in
    (** Find the constification record [fcr] of [f] in [crs]. If it does not
        exist, fail. This is not normal! *)
    if not (Var_Hashtbl.mem crs f) then
      fail t.loc (missing_record "analyze_on.aux" f (-1));
    let fcr = Var_Hashtbl.find crs f in
    (** Create a hash table of arguments and aliases to arguments. *)
    let aliases : l = LVar_Hashtbl.create 10 in
    (** Try to find the parent class of the function. If any, get all the member
        variables of that class in [siblings] and *)
    let siblings = match (find_parent_typedef_record p) with
      | Some (td) -> typedef_get_members td
      | None -> [] in
    (** add them to [fcr]. *)
    if (List.length siblings) > 0 then
      begin
        (** In the case of class member methods, the first argument of the
            function is the variable referring to the parent class instance,
            i.e. [this]. Get it. *)
        let (this, _) = List.hd args in
        (** Record that [f] is a class member method. *)
        fcr.member <- true;
        (** For each member variable of the parent class, *)
        List.iteri (fun i (label, ty) ->
            (** build the corresponding labelled variable using [this] and
                [label] and *)
            let lv : lvar = { v = this; l = label } in
            (** add it to [aliases] while determining its number of levels of
                indirection. *)
            LVar_Hashtbl.add aliases lv (-1, Apac_miscellaneous.typ_get_nli ty)
          ) siblings
      end;
    (** If the function is not a class member method, we must not constify it.
        It wouldn't be taken into account by the compiler, anyways. *)
    if not fcr.member then
      fcr.const <- false;
    (** Then, of course, we have to add the arguments of the function itself to
        [fcr] so as to be able to identify possible aliases to these variables
        within the body of the function during the analysis using the above
        local auxiliary function. For class member methods, we do not add the
        first argument which is [this], a variable referring to the parent class
        instance. Indeed, we have already added sibling class members [fcr]. *)
    let args = if fcr.member then List.tl args else args in
    List.iteri (fun i (v, ty) ->
        if (Tools.Int_map.mem i fcr.args) then
          begin
            let acr = Tools.Int_map.find i fcr.args in
            if acr.self <> Variable then
              (** If an argument is a structure or a class, we have to
                  recursively include its members (see [!arg_explode]). *)
              let lvs = arg_explode v ty in
              (** For now, we do not perform the below tests for structure and
                  class members. *)
              List.iter (fun (lv, ty) ->
                  let nli = Apac_miscellaneous.typ_get_nli ty in
                  LVar_Hashtbl.add aliases lv (i, nli)
                ) lvs
          end
        else
          failwith (missing_record "analyze_on" f i)
      ) args;
    (** Analyze the body of [f] for data dependencies on arguments and their
        aliases. *)
    aux aliases f body

  (** [analyze us ous crs tg]: expects the target [tg] to point at a function
      definition. It scans the body of the target function definition for data
      dependencies on arguments and their aliases while filling up the
      unconstification stacks [us] and [ous] and updating the hash table of
      function constification records [crs] (see [!section:principle]). *)
  let analyze (us : u) (ous : o) (crs : r) (tg : target) : unit =
    Target.iter (fun t p -> analyze_on us ous crs p (get_trm_at_path p t)) tg

  (** [unconstify us ous]: unconstify (see [!section:principle]) arguments and
      functions, i.e. pass the [const] element of their constification records
      to [false], depending on the items in the unconstification stack [us], the
      object unconstification stack [ous] and the hash table of function
      constification records [crs]. *)
  let unconstify (us : u) (ous : o) (crs : r) : unit =
    (** [unconstify.non_objects]: unconstifies functions and arguments, except
        for those representing objects. *)
    let rec non_objects () : unit =
      (** Pop out an element from [us]. *)
      match Stack.pop_opt us with
      | Some (f, i) ->
         (** Find the corresponding function constification record in [crs]. *)
         let cr = Var_Hashtbl.find crs f in
         (** If the argument's position is set to [-1], *)
         if i = -1 then
           (** it means that the function is a class member method and a class
               sibling is being modified within its body. Therefore, we must not
               constify the function. *)
           begin if cr.member then cr.const <- false end
         else
           begin
             (** Otherwise, try to get the corresponding argument constification
                 record. *)
             if (Tools.Int_map.mem i cr.args) then
               begin
                 let a = Tools.Int_map.find i cr.args in
                 (** Then, if needed, *)
                 if a.const then
                   begin
                     (** unconstify the argument *)
                     a.const <- false;
                     (** and push to [us] all of the arguments we should
                         unconstify by propagation. In other terms, follow the
                         dependencies. *)
                     Var_map.iter (
                         fun k e -> Stack.push (k, e) us
                       ) a.propagate;
                   end
               end
             else
               (** If it is not possible, fail. This is not normal! *)
               let error =
                 Printf.sprintf
                   "Apac_preprocessing.Constification.unconstify.non_objects: \
                    the constification record of `%s' has no argument \
                    constification record for the argument on position `%d'."
                   (var_to_string f) i
               in
               failwith error
           end;
         (** Continue the unconstification process until *)
         non_objects ()
      (** there are no more elements in the unconstification stack [us]. *)
      | None -> ()
    in
    (** [unconstify.objects]: unconstifies function arguments representing
        objects. *)
    let rec objects () : unit =
      (** Pop out an element from [ous]. *)
      match Stack.pop_opt ous with
      | Some (tg, i, f) ->
         (** Find the constification record of the callee, i.e. [f]. *)
         let crf = Var_Hashtbl.find crs f in
         (** If it is a non-[const] class member method, *)
         if crf.member && (not crf.const) then
           begin
             (** find the constification record of the caller of [f], i.e. [tg],
                 and *)
             let crtg = Var_Hashtbl.find crs tg in
             (** try to gather the corresponding argument constification
                 record. *)
             if (Tools.Int_map.mem i crtg.args) then
               begin
                 let a = Tools.Int_map.find i crtg.args in
                 (** Then, if needed, unconstify the argument. *)
                 if a.const then a.const <- false
               end
             else
               (** If it is not possible, fail. This is not normal! *)
               let error =
                 Printf.sprintf
                   "Apac_preprocessing.Constification.unconstify.objects: the \
                    constification record of `%s' has no argument \
                    constification record for the argument on position `%d'."
                   (var_to_string tg) i
               in
               failwith error
           end;
         (** Continue the unconstification process until *)
         objects ()
      (** there are no more elements in the unconstification stack [ous]. *)
      | None -> ()
    in
    non_objects ();
    objects ()

  (** [constify_prototypes_on ?crs t]: see [!constify_prototypes]. *)
  let constify_prototypes_on ?(crs : r option = None)
        (p : path) (t : trm) : trm =
    (** Deconstruct the definition term [t] of the function [f]. *)
    let error = "Apac_preprocessing.Constification.constify_prototypes_on: \
                 expected a target to a function definition." in
    let (f, ret_typ, args, body) = trm_inv ~error trm_let_fun_inv t in
    (** Proceed only if [f] is other than [!Apac_flags.main]. *)
    if f.name <> !Apac_flags.main then
      match crs with
      (** If there are no constification records, *)
      | None ->
         (** constify all of the arguments as well as *)
         let args = List.map (fun (v, ty) -> (v, (typ_constify ty))) args in
         let t = trm_let_fun ~annot:t.annot f ret_typ args body in
         (** the function itself, if it is a class member method. *)
         if Option.is_some (find_parent_typedef_record p) then
           trm_add_cstyle Const_method t
         else t
      (** Otherwise, consult the constification record of [f] to find out which
          of its arguments we should constify, if any, and whether we should
          constify the function itself. *)
      | Some crs when (Var_Hashtbl.mem crs f) ->
         let crf = Var_Hashtbl.find crs f in
         (** If the function is a class member method, its first argument is the
             [this] variable referring to the parent class. Ignore it. *)
         let args' = if crf.member then List.tl args else args in
         (** Loop over the list of arguments and *)
         let args' = List.mapi (fun i (v, ty) ->
                         (** after checking whether there is an argument
                             constification record for each argument of [f], *)
                         if (Tools.Int_map.mem i crf.args) then
                           let cra = Tools.Int_map.find i crf.args in
                           if cra.const then
                             (** constify it if the records says so *)
                             (v, (typ_constify ty))
                           else
                             (** or leave it as is. *)
                             (v, ty)
                         else
                           (** If an argument constification record is missing,
                               fail. This is not normal! *)
                           fail t.loc
                             (missing_record "constify_prototypes_on.aux" f i)
                       ) args' in
         (** Rebuild the definition term [t] of [f] using the updated list of
             arguments [args']. However, we have to bring back [this] to the
             list of arguments. If [f] is a class member method, [this] is a
             non-empty list. *)
         let args = if crf.member then (List.hd args) :: args' else args' in
         let t = trm_let_fun ~annot:t.annot f ret_typ args body in
         (** Constify the function too if its constification record says so. *)
         if crf.const then trm_add_cstyle Const_method t
         else t
      (** If there is no constification record for [f], fail. This is not
          normal! *)
      | _ -> fail t.loc (missing_record "constify_prototypes_on.aux" f (-1))
  else t

  (** [constify_prototypes ?crs tg]: expects the target [tg] to point at a
      function definition. Then, based on the constification record of the
      function in the hash table of function constification records [crs], it
      contifies selected arguments of the function and if the constification
      record says so, it constifies the function itself too.

      One can ignore, e.g. for testing purposes, the constification records and
      constify all of the function's arguments as well as the function itself.
      For this, skip the [crs] argument or set it to [None]. *)
  let constify_prototypes ?(crs : r option = None) (tg : target) : unit =
    Target.apply (fun t p ->
        Path.apply_on_path (constify_prototypes_on ~crs p) t p
      ) tg

  (** [constify_aliases_on ?crs t]: see [!constify_aliases]. *)
  let constify_aliases_on ?(crs : r option = None) (t : trm) : trm =
    (** [constify_aliases_on.aux aliases t]: auxiliary function to recursively
        constify the variable declarations of all the [aliases] in an abstract
        syntax tree term [t]. *)
    let rec aux (aliases : l) (t : trm) : trm =
      match t.desc with
      (** When [t] is a compound, an interation or a selection statement
          involving substatements, loop over the latter. *)
      | Trm_seq _
        | Trm_for _
        | Trm_for_c _
        | Trm_if _
        | Trm_switch _
        | Trm_while _ -> trm_map (aux aliases) t
      (** When [t] is a declaration of a variable [v] of type [ty] with an
          initialization term [ti], we have to update the list of [aliases] and
          constify [ty] if it is an alias to a constant variable. *)
      | Trm_let (_, (v, ty), { desc = Trm_apps (_, [ti]); _ }, _) ->
         (** At first, we determine what [kind] of variable [v] is. *)
         let kind : k =
           if trm_has_cstyle Reference t then Reference
           else
             if is_typ_ptr (get_inner_const_type (get_inner_ptr_type ty)) then
               Pointer
             else
               if is_typ_array
                    (get_inner_const_type (get_inner_ptr_type ty)) then
                 Array
               else Variable in
         (** Then, check whether we are declaring an alias to an argument. *)
         let ll = trm_find_memlocs ti in
         let ll = aliasing aliases ll in
         (** If so, i.e. if [ll] is not an empty list, *)
         List.iter (fun (_, argument) ->
             (** we have to update [aliases] for each [alias] to an
                 [argument]. *)
             let alias : lvar = { v = v; l = String.empty } in
             LVar_Hashtbl.add
               aliases alias (argument, Apac_miscellaneous.typ_get_nli ty)                    
           ) ll;
         (** Also, we have to constify the current variable declaration. *)
         if (List.length ll) > 0 then
           match kind with
           | Reference ->
              let ty = typ_ref (typ_constify (get_inner_ptr_type ty)) in
              trm_let_mut (v, ty) ti
           | Pointer ->
              let ty = typ_constify (get_inner_ptr_type ty) in
              trm_let_mut (v, get_inner_const_type ty) ti
           | Array ->
              let ty = typ_constify (get_inner_array_type ty) in
              trm_let_mut (v, get_inner_const_type ty) ti
           (** Note that simple variables do not lead to aliases. *)
           | Variable -> t
         else t
      (** When [t] is none of the above, loop over its child elements. *)
      | _ -> trm_map (aux aliases) t
    in
    (** Deconstruct the definition term [t] of the function [f]. *)
    let error = "Apac_preprocessing.Constification.constify_aliases_on: \
                 expected a target to a function definition." in
    let (f, ret_ty, args, body) = trm_inv ~error trm_let_fun_inv t in
    (** Proceed only if [f] is other than [!Apac_flags.main]. *)
    if f.name <> !Apac_flags.main then
      (** Create a hash table of arguments and aliases to arguments. *)
      let aliases : l = LVar_Hashtbl.create 10 in
      (** If the hash table of function constification records [crs] is present,
          consult the constification record [crf] of [f] to find out which of
          its arguments became constant, if any. Then, add them to [aliases] so
          the auxiliary function constifies all of their aliases within the body
          of [f].

          The principle we adopt here is the opposite of [!analyze]. In the
          latter, we use a hash table of arguments and aliases to arguments (see
          type [!type:l]) to keep track of arguments and aliases to arguments we
          must not constify whereas, here, we use them to keep track of those
          arguments that have been constified and their aliases to which we have
          to propagate the constification process. *)
      begin
        match crs with
        | Some crs ->
           (** Find the constification record [fcr] of [f] in [crs]. If it does
               not exist, fail. This is not normal! *)
           if not (Var_Hashtbl.mem crs f) then
             fail t.loc (missing_record "constify_aliases_on.aux" f (-1));
           let fcr = Var_Hashtbl.find crs f in
           (** If the function is a class member method, its first argument is
               the [this] variable referring to the parent class. Ignore it. *)
           let args' = if fcr.member then List.tl args else args in
           (** Otherwise,  *)
           (** Loop over the list of arguments and *)
           List.iteri (fun i (v, ty) ->
               (** after checking whether there is an argument constification
                   record for each argument of [f], *)
               if (Tools.Int_map.mem i fcr.args) then
                 begin
                   (** gather the argument constification record [acr] of the
                       [i]-th argument of [f]. *)
                   let acr = Tools.Int_map.find i fcr.args in
                   (** If it has been constified and if it does not represent a
                       simple variable, *)
                   if acr.const && acr.self <> Variable then
                     (** add it to [aliases]. *)
                     let lv : lvar = { v = v; l = String.empty } in
                     LVar_Hashtbl.add
                       aliases lv (i, Apac_miscellaneous.typ_get_nli ty)
                 end
               else
                 (** If an argument constification record is missing, fail. This
                     is not normal! *)
                 fail t.loc (missing_record "constify_aliases_on.aux" f i)
             ) args'
        | _ ->
           (** If [crs] is not present, consider all the arguments of [f] as
               [const]. Add them to [aliases], except for [this] in the case of
               class member methods, so the auxiliary function constifies all of
               their aliases within the body of [f]. *)
           List.iteri (fun i (v, ty) ->
               if v.name <> "this" then
                 let lv : lvar = { v = v; l = String.empty } in
                 LVar_Hashtbl.add aliases
                   lv (i, Apac_miscellaneous.typ_get_nli ty)
             ) args
      end;
      (** Constify the declarations of aliases in the body of [f]. *)
      let body = aux aliases body in
      (** Rebuild and return the definition term [t] of [f]. *)
      trm_let_fun ~annot:t.annot f ret_ty args body
    else t

  (** [constify_aliases ?crs tg]: expects target the target [tg] to
      point at a function definition. Then, based on the constification record
      of the function in the hash table of function constification records
      [crs], it constifies the aliases to arguments that became constant.

      One can ignore, e.g. for testing purposes, the constification records and
      consider that all the function's arguments as well as the function itself
      have been constified and then constify all of the relevant aliases. For
      this, omit the [crs] argument or set it to [None]. *)
  let constify_aliases ?(crs : r option = None) (tg : target) : unit =
    Target.apply_at_target_paths (constify_aliases_on ~crs) tg

  (** [constify ?frs ?trans tg]: expects target [tg] to point at all the
      function definitions in the input source code. It constifies the
      function's arguments and the function itself whenever it is possible and
      useful. If a hash table of function records is present in the argument
      [frs], update the records in the latter with the results of the
      constification pass. If the [trans] option, by default set to [true], is
      set to [false], the pass updates [frs] but preserves the input source code
      and does not insert any [const] qualifier in it. If [frs] is not present,
      the results of the constification pass are lost.

      For example, let us consider the short C program below defining a function
      [add].

      {[
      void add(int * a, int * b, int c) {
        a[0] = a[0] + b[0] + c;
      }
      ]}

      The arguments [a] and [b] of [add] are pointers to non-[const] data. The
      function overwrites the content at the memory location in [a], but it does
      not modify the contents behind [b] and [c] is not a pointer. Therefore,
      the transformation concludes that it is safe to constify [b]:

      {[
      void add(int * a, const int * b, int c) {
        a[0] = a[0] + b[0] + c;
      }
      ]}

      Let us now analyze a more complex situation:

      {[
      int * g(int * out, int * in, int n) {
        for(int i = 0; i < n; i++)
          out[i] *= in[i];
        return out;
      }
      int * f(int * data, int * result) {
        result = g(result, data);
        return result;
      }
      ]}

      The above short C program defines two functions [g] and [f]. [g] has three
      formal arguments [out], [in] and [n]. [out] and [in] are pointers to
      non-[const] data and [n] is an integer. The function multiplies the
      elements of [out] with the elements of [in] while storing the result in
      [out]. [f] has two formal arguments [data] and [result] pointing to
      non-[const] data. It calls [g] while passing it [result] and [data] as
      arguments. At first, in the case of [g], the constification pass concludes
      that the argument [in] can be safely constified, but not [out], which is
      overwritten by the function. However, [f] calls [g] and passes [result]
      and [data] as actual arguments to [g]. We already know that the the
      corresponding formal arguments [out] and [in] of [g] are pointers to
      non-[const] and [const] data, respectively. Therefore, in the case of [f],
      the pass concludes that [data] can be safely constified, but not [result],
      pointing to data modified in [g]:

      {[
      int * g(int * out, const int * in, int n) {
        for(int i = 0; i < n; i++)
          out[i] *= in[i];
        return out;
      }
      int * f(const int * data, int * result) {
        result = g(result, data);
        return result;
      }
      ]} *)
  let constify
        ?(frs : Apac_records.FunctionRecord.t Var_Hashtbl.t option = None)
        ?(trans : bool = true) (tg : target) : unit =
    (** Create a hash table of function constification records with an initial
        size of 10 entries. *)
    let crs : r = Var_Hashtbl.create 10 in
    (** Build constification records for all the function definitions in the
        input source code. *)
    build_records crs tg;
    (** Create unconstification stacks. *)
    let us : u = Stack.create () in
    let ous : o = Stack.create () in
    (** Analyze the functions definitions and determine which arguments and
        function, in the case of class member methods, we can constify. *)
    analyze us ous crs tg;
    (** After initially considering all arguments and functions as constifiable,
        unconstify those we cannot constify based on data accesses. *)
    unconstify us ous crs;
    (** Dump the records on the screen, if requested. *)
    if !Apac_flags.verbose then
      begin
        Printf.printf "Constification records:\n";
        Var_Hashtbl.iter (fun f fcr ->
            Printf.printf "%s = %s\n" (var_to_string f) (f_to_string fcr)) crs
      end;
    if trans then
      begin
        (** Add the [const] keyword to the function prototypes based on the
            above analysis and unconstification passes. *)
        constify_prototypes ~crs:(Some crs) tg;
        (** Constify aliases to arguments or to previously declared aliases. *)
        constify_aliases ~crs:(Some crs) tg
      end;
    (** Propagate the results of the constification to the function records in
        [frs], if any. *)
    match frs with
    | Some frs ->
       (** For each constification record [cr] of a function [f], *)
       Var_Hashtbl.iter (fun f cr ->
           (** determine which arguments of [f] we have constified and retrieve
               the information in the form of a list of booleans [cl] with
               [true] for each [const] argument and [false] for each non-[const]
               argument, *)
           let cl =
             Tools.Int_map.fold (fun _ (acr : a) acc ->
                 acc @ [acr.const]
               ) cr.args [] in
           (** find the function record [fr] of [f] in [frs] and *)
           if (Var_Hashtbl.mem frs f) then
             let fr = Var_Hashtbl.find frs f in
             (** update the access classification of [f]'s arguments [fr]. *)
             fr.args <- Apac_records.FunctionRecord.constify cl fr.args;
           else
             (** If [fr] does not exist, fail. *)
             let error = Printf.sprintf
                           "Apac_preprocessing.Constification.constify: \
                            function `%s' has no function record."
                           (var_to_string f) in
             failwith error           
         ) crs
    (** Otherwise, do nothing. *)
    | None -> ()
end

(** {1:return_unification Return unification}

    Handling [return] statements when building a task candidate graph requires a
    preliminary transformation, referred to as return unification, of the
    corresponding function definition. Indeed, in order to guarantee a correct
    return value, a function should wait for the completion of all the tasks it
    has generated before returning. Therefore, we cannot simply include [return]
    statements in task candidates. This pass ensures a single [return] statement
    at the end of each function definition by translating all the other
    [return]s into [goto]s jumping to a placeholder label (see
    [!Apac_macros.goto_label]) preceding that final [return]. This
    transformation then allows us, in the parallel code generation stage, to
    introduce synchronization barriers in such a way as to wait for all the
    tasks of the function to complete prior to returning. If the return type of
    the function is other than [void], the pass introduces a unique variable
    (see [!Apac_macros.result_variable]) to contain the value to return, updates
    it before each [goto] statement and returns it in the final [return]
    statement. *)

(** [unify_returns tg]: expects the target [tg] to point at a function
    definition. It replaces [return] statements within the body of the latter by
    a single [return] statement through the usage of [goto] jumps.

    First of all, the transformation wraps the body of the target function into
    a sequence and marks it with [!Apac_macros.candidate_body_mark], as well as
    [!Apac_macros.candidate_main_mark] in the case of the [!Apac_flags.main]
    function. Then, if the function returns [void], it appends the
    [!Apac_macros.goto_label] to the sequence and replaces each [return]
    statement inside the sequence with a [goto] jumping to that label.

    For example, the following function definition

    {[
    void returns_nothing(int a, int b) {
      if(a > b) {
        // ... some work with a ...
        return;
      }
      // ... some work with b ...
    }
    ]}

    becomes

    {[
    void returns_nothing(int a, int b) {
      {
        if(a > b) {
          // ... some work with a ...
          goto __apac_exit;
        }
        // ... some work with b ...
    __apac_exit: ;
      }
    }
    ]}

    If the function returns a value, the transformation preprends the sequence
    with the declaration of [!Apac_macros.result_variable], appends the
    [!Apac_macros.goto_label] to the sequence and replaces each [return]
    statement inside the sequence with an assignment of the value to return to
    the [!Apac_macros.result_variable] and a [goto] jumping to
    [!Apac_macros.goto_label].

    For example, the following function definition

    {[
    void returns_something(int a, int b) {
      if(a > b) {
        // ... some work with a ...
        return a;
      }
      // ... some work with b ...
      return b;
    }
    ]}

    becomes

    {[
    void returns_something(int a, int b) {
      {
        if(a > b) {
          // ... some work with a ...
          __apac_ret = a;
          goto __apac_exit;
        }
        // ... some work with b ...
        __apac_ret = b;
        goto __apac_exit;
    __apac_exit: ;
      }
    }
    ]} *)
let unify_returns (tg : target) : unit =
  Nobrace_transfo.remove_after (fun _ ->
      Target.apply_at_target_paths (fun t ->
          (** Deconstruct the definition term [t] of the function [f]. *)
          let error = "Apac_preprocessing.unify_returns: expected a target to \
                       a function definition." in
          let (f, ty, args, body) = trm_inv ~error trm_let_fun_inv t in
          (** Unify the [return] statements within the [body] of [f]. The result
              is a new body sequence. *)
          let ret = new_var Apac_macros.result_variable in
          let (body, _) =
            Internal.replace_return_with_assign
              ~check_terminal:false
              ~exit_label:Apac_macros.goto_label ret body in
          (** Add [ret] into the function record, if it exists. This may not be
              the case when testing the transformation separately. *)
          if (Var_Hashtbl.mem Apac_records.functions f) then
            begin
              let r = Var_Hashtbl.find Apac_records.functions f in
              Var_Hashtbl.add r.scope ret (1, Var_mutable)
            end;
          (** Append [!Apac_macros.goto_label] to the new sequence. *)
          let body = trm_seq_add_last
                       (trm_add_label
                          Apac_macros.goto_label (trm_unit())) body in
          (** Mark the new sequence with [!Apac_macros.candidate_body_mark]. *)
          let body = Mark.trm_add_mark Apac_macros.candidate_body_mark body in
          (** and with [!Apac_macros.candidate_main_mark] if [f] is the
              [!Apac_flags.main] function. *)
          let body =
            if f.name = !Apac_flags.main then
              Mark.trm_add_mark Apac_macros.candidate_main_mark body
            else
              body
          in
          (** If [f] returns something else than [void], we need to declare
              [!Apac_macros.result_variable] at the beginning of the new
              sequence and return its value at the end of the sequence. *)
          let body = if (is_type_unit ty) then
                       trm_seq_nomarks [body]
                     else
                       trm_seq_nomarks [
                           (trm_let_mut (ret, ty) (trm_uninitialized ()));
                           body;
                           trm_ret (Some (trm_var_get ret))
                         ] in
          (** Rebuild and return the function definition term. *)
          trm_let_fun ~annot:t.annot f ty args body
        ) tg)

