open Ast
open Typ

(** [cwd]: returns the path to the current working directory. *)
let cwd () : string =
  Flags.process_program_name ();
  Filename.dirname (!Flags.program_name)

(** [gwd]: returns the path to a directory within the current working directory
    to store task candidate graphs in. If this directory does not exists, this
    function creates it. *)
let gwd () : string =
  (** Build the full path to the destination directory. *)
  let path = (cwd ()) ^ "/" ^ !Apac_macros.keep_graphs_in in
  if (Sys.file_exists path) then
    if (Sys.is_directory path) then
      (** If the path points to an existing directory, just return it. *)
      path
    else
      (** If the path points to an existing file which is not a directory,
          fail. *)
      let error = Printf.sprintf "Apac_miscellaneous.gwd: `%s' exists, but it \
                                  is not a directory." path in
      failwith error
  else
    (** Otherwise, create the destination directory with ususal permission set
        and return the path to it. *)
    begin
      Sys.mkdir path 0o755;
      path
    end

(** [gf]: returns the full path (see [!gwd]) to a file of type [extension]
    ([dot] or [pdf], which is the default) containing the task candidate graph
    of a function [f]. The name of the file may carry an optional [suffix]. *)
let gf ?(suffix : string = "") ?(extension : string = "pdf")
      (f : var) : string =
  let dir = gwd () in
  let name = f.name ^ "-" ^ (string_of_int f.id) ^
               (if suffix <> "" then "-" ^ suffix else "") ^ "." ^ extension in
  dir ^ "/" ^ name

(** [hcbpm]: returns the full paths (see [!cwd]) to the profiling [h]eader, the
    source [c]ode with profiling instructions, the corresponding [b]inary
    executable, the resulting [p]rofile and performance [m]odel files. *)
let hcbpm () : string * string * string * string * string =
  let here = cwd () ^ "/" in
  let this = Filename.remove_extension
               (Filename.basename !Flags.program_name) in
  let _ = Printf.printf "this is %s\n" this in
  let header = here ^ Apac_macros.profiler_header in
  let code = here ^ this ^ "_profiling" in
  let binary = here ^ this ^ "_profiling.bin" in
  let profile = here ^ this ^ ".profile" in
  let model = profile ^ ".model" in
  (header, code, binary, profile, model)

(** [excerpt ?max ast]: returns an excerpt of a string representation of an
    [ast] term at most [max] characters long. *)
let excerpt ?(max : int = 20) (ast : trm) =
  let instr = AstC_to_c.ast_to_string ast in
  let instr = String.split_on_char '\n' instr in
  let instr = List.hd instr in
  let instr = String.split_on_char '"' instr in
  let instr = List.hd instr in
  let instr = String.trim instr in
  let limit = String.length instr in
  let limit = if limit > max then max else limit in
  String.sub instr 0 limit

(* [typ_is_alias ty]: checks if [ty] is a user-defined alias to a basic type. *)
let typ_is_alias (ty : typ) : bool =
  match ty.typ_desc with
  | Typ_constr (_, id, _) ->
    begin match Context.typid_to_typedef id with
    | Some (td) ->
      begin match td.typdef_body with
      | Typdef_alias _ -> true
      | _  -> false
      end
    | None -> false
    end
  | _ -> false

(* [typ_get_alias ty]: if [ty] is a user-defined alias to a basic type, it
   returns the latter. *)
let typ_get_alias (ty : typ) : typ option =
  match ty.typ_desc with
  | Typ_constr (_, id, _) ->
    begin match Context.typid_to_typedef id with
    | Some (td) ->
      begin match td.typdef_body with
      | Typdef_alias ty -> Some (ty)
      | _  -> None
      end
    | None -> None
    end
  | _ -> None

(** [typ_get_nli ty]: returns the number of levels of indirection of the type
    [ty], e.g. [2] for [int ** a]. *)
let typ_get_nli (ty : typ) : int =
  (** [typ_get_nli.aux nli ty]: auxiliary function hiding the counter [nli] of
      levels of indirection of [ty]. *)
  let rec aux (nli : int) (ty : typ) : int =
    match ty.typ_desc with
    (** When [ty] is a constant type or a reference, [nli] does not change,
        continue the computation on the inner type. *)
    | Typ_const ty -> aux nli ty
    | Typ_ptr { ptr_kind = Ptr_kind_ref; inner_typ = ty } -> aux nli ty
    (** When [ty] is a pointer or an array, increase [nli] and continue the
        computation on the inner type. *)
    | Typ_ptr { ptr_kind = Ptr_kind_mut; inner_typ = ty } -> aux (nli + 1) ty
    | Typ_array (ty, _) -> aux (nli + 1) ty
    (** When [ty] is a constructed user-defined type and it is an alias to a
        basic type, resolve the latter and continue computation. *)
    | Typ_constr _ when typ_is_alias ty ->
      begin match typ_get_alias ty with
      | Some (ty) -> aux nli ty
      | None -> failwith "Apac_miscellaneous.typ_get_nli: unable to determine \
                          the basic type behind an alias."
      end
    (** When [ty] is a basic type or a constructed user-defined type which is {b
        not} an alias to a basic type, we have finished computing, return the
        final number of levels fo indirection of [ty]. *)
    | _ -> nli
  in
  (** Start counting the levels of indirection of [ty] at level [0]. *)
  aux 0 ty

(* [trm_strip_accesses_and_references_and_get_lvar t]: strips [*t, &t, ...]
   recursively and if [t] is a variable, it returns the associated labelled
   variable. *)
let trm_strip_and_get_lvar (t : trm) : lvar option =
  (* Internal auxiliary recursive function allowing us to hide the [l] parameter
     to the outside world. *)
  let rec aux (l : label) (t : trm) : lvar option =
    match t.desc with
    (* [t] is a unary operation *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _ }, [t]) ->
       begin
         match op with
         (* Whenever we stumble upon a structure access or get operation, we
            extract the label of the structure field involved in the
            operation. *)
         | Unop_struct_access field -> aux field t
         | Unop_struct_get field -> aux field t
         (* In case of another unary operations, we simply recurse on the
            internal term. *)
         | _ -> aux l t
       end
    (* [t] is a binary operation corresponding to an array access *)
    | Trm_apps ({
            desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)));
            _ }, [t; _]) -> aux l t
    (* [t] is a binary operation of another type: strip and recurse on both left
       and right-hand sides. *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_binop _ )); _ }, [lhs; rhs]) ->
       (* We continue to recurse on both the left and the right internal
          terms. *)
       begin
         match (aux l lhs, aux l rhs) with
         | Some (res), None -> Some (res)
         | None, Some (res) -> Some (res)
         | None, None
           (* In practice, binary operations between two pointers supported in
              C/C++ can not lead to a valid alias of one of them. *)
           | Some (_), Some (_) -> None
       end
    (* [t] actually leads to a variable *)
    | Trm_var (_, var) ->
       (* Use [var] and the label [l] to build the associated labelled
          variable and return it. *)
       let lv : lvar = { v = var; l = l } in Some lv
    | _ -> None
  in
  aux "" t

(* [trm_can_resolve_pointer t]: tries to resolve operation [t] to unique
   variable and returns [true] on success and [false] otherwise. *)
let rec trm_can_resolve_pointer (t : trm) : bool =
    match t.desc with
    (* [t] is unary operation: strip and recurse. *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _ }, [t]) ->
       begin match op with
       | Unop_get
         | Unop_address
         | Unop_cast _ -> trm_can_resolve_pointer t
       | _ -> false
       end
    (* [t] is a binary operation corresponding to an array access: strip and
       recurse. *)
    | Trm_apps ({
            desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)));
            _ }, [t; _]) -> trm_can_resolve_pointer t
    (* [t] is a binary operation of another type: strip and recurse on both left
       and right-hand sides. *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_binop _ )); _ }, [lhs; rhs]) ->
       (trm_can_resolve_pointer lhs) || (trm_can_resolve_pointer rhs)
    (* [t] actually leads to a variable: success. Return [true]. *)
    | Trm_var _ -> true
    | _ -> false

(* [trm_can_resolve_pointer t]: tries to resolve operation [t] to unique
   variable. It then returns the latter on success and [None] otherwise. *)
let trm_resolve_pointer_and_degree (t : trm) : (var * int) option =
  let rec aux (degree : int) (t : trm) : (var * int) option =
    match t.desc with
    (* [t] is unary operation: strip and recurse. *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _ }, [t']) ->
       begin match op with
       | Unop_get -> aux (degree - 1) t'
       | Unop_address -> aux (degree + 1) t'
       | Unop_cast ty -> aux (degree + typ_get_nli ty) t'
       | Unop_struct_access _
         | Unop_struct_get _ -> aux degree t'
       | _ -> None
       end
    (* [t] is a binary operation corresponding to an array access: strip and
       recurse. *)
    | Trm_apps ({
            desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)));
            _ }, [t'; _]) -> aux (degree - 1) t'
    (* [t] is a binary operation of another type: strip and recurse on both left
       and right-hand sides. *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_binop _ )); _ }, [lhs; rhs]) ->
       (* We continue to recurse on both the left and the right internal
          terms. *)
       begin
         match (aux degree lhs, aux degree rhs) with
         | Some (res), None -> Some (res)
         | None, Some (res) -> Some (res)
         | None, None
           (* In practice, binary operations between two pointers supported in
              C/C++ can not lead to a valid alias of one of them. *)
           | Some (_), Some (_) -> None
       end
    (* [t] actually leads to a variable. Return it. *)
    | Trm_var (vk, v) -> Some (v, degree)
    (* In all the other cases, return [None]. *)
    | _ -> None
  in
  aux 0 t

(** [trm_is_array_or_direct_access t]: checks whether the term [t] represents an
    array or a direct variable access. *)
let trm_is_array_or_direct_access (t : trm) : bool =
  match t.desc with
    | Trm_apps ({desc = Trm_val
                          (Val_prim (Prim_binop Binop_array_access)); _}, _) ->
       true
    | Trm_var _ -> true
    | _ -> false

(** [trm_resolve_dereferenced_with_degree t]: if the term [t] represents a stack
    of dereference operations, it returns the underlying array or direct
    variable access term and the number of dereferencings, i.e. the degree. *)
let trm_resolve_dereferenced_with_degree (t : trm) : (trm * int) option =
  let rec aux (degree : int) (t : trm) : (trm * int) option =
    match t.desc with
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _ }, [t']) ->
       begin match op with
       | Unop_get -> aux (degree + 1) t'
       | _ -> None
       end
    | Trm_apps ({desc = Trm_val
                          (Val_prim (Prim_binop Binop_array_access)); _}, _)
      | Trm_var _ -> Some (t, degree)
    | _ -> None
  in
  aux 0 t

(* [trm_resolve_binop_lval_and_get_with_deref] tries to resolve the variable
   behind an lvalue and check whether it has been dereferenced, i.e. following
   an array access or the use of [*]. Upon success, it returns the corresponding
   labelled variable. See [LVar] for more details on labelled variables. *)
let trm_resolve_binop_lval_and_get_with_deref ?(plus : bool = false)
      (t : trm) : (lvar * bool) option =
  let rec aux (d : int) (l : label) (t : trm) : (lvar * bool) option =
    match t.desc with
    (* We have found the variable, build and return the resulting labelled
       variable. *)
    | Trm_var (vk, var) ->
       let lv : lvar = { v = var; l = l } in
       let d' = if vk = Var_immutable && plus then d + 1 else d in
       let _ = Printf.printf "%s has finally %d derefs\n" var.name d' in
       Some (lv, d' > 0)
    (* [t] is an array access, which means that the operand was dereferenced.
       Continue resolution on the latter. *)
    | Trm_apps ({
            desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)));
            _ }, [t; _]) -> aux (d + 1) l t
    (* [t] is a unary operation. *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop (op))); _ }, [t]) ->
       begin
         match op with
         (* A get operation, e.g. [*operand], as well as a structure access,
            e.g. [operand.field], both imply that the operand was dereferenced.
            Continue resolution on the latter. *)
         | Unop_get -> aux (d + 1) l t
         | Unop_address -> aux (d - 1) l t
         | Unop_cast ty -> aux (d + typ_get_nli ty) l t
         | Unop_struct_access field -> aux (d + 1) field t
         (* A structure access through pointer, e.g. [operand->field], means
            that the operand was not dereferenced. To finish finished resolving,
            iterate once more on [t]. *)
         | Unop_struct_get field -> aux d field t
         (* In case of another binary operation, do nothing and continue
            resolution on the operand. *)
         | _ -> aux d l t
       end
    | _ -> None
  in
  aux 0 "" t

(** [profiler_hpp profile output]: generates the contents of the profiler header
    while considering [profile] as the name of the profiling output file and
    saves it to an [output] file. *)
let profiler_hpp (profile : string) (output : string) : unit =
  let output = open_out output in
  output_string output
    ("\
#ifndef __APAC_PROFILER_HPP
#define __APAC_PROFILER_HPP

#include <iostream>
#include <cstdlib>
#include <fstream>
#include <string>
#include <chrono>
#include <filesystem>

class apac_t {
private:
  std::chrono::high_resolution_clock::time_point begin;
  std::chrono::high_resolution_clock::time_point end;

public:
  apac_t() { start(); }
  void start() { begin = std::chrono::high_resolution_clock::now(); }
  void stop() { end = std::chrono::high_resolution_clock::now(); }
  double elapsed() const {
    return
      std::chrono::duration_cast<std::chrono::nanoseconds>
      (end - begin).count() / 1e9;
  }
};

class apac_s {
private:
  int params;
  std::string prefix;
  std::string current;
  apac_t timer;
  const std::string profile;

void ensure_profile() const {
  static bool first = true;
  if (first) {
    if (std::filesystem::exists(this->profile)) {
      std::remove(this->profile.c_str());
    }

    std::ofstream profile(this->profile, std::ios_base::app);
    profile.close();
    first = false;
  }
}

public:
  apac_s() : params(0), profile(\"" ^ profile ^ "\") { }

  void initialize(const std::string id) {
    prefix.append(\"{ \");
    prefix.append(id);
    prefix.append(\", \");
  }

  template <class T> void add(const T & parameter) {
    current.append(\"R=\");
    current.append(std::to_string(parameter));
    current.append(\", \");
    params++;
  }

  void before() {
    prefix.append(std::to_string(params));
    prefix.append(\", \");
    timer.start();
  }

  void after() {
    timer.stop();
    ensure_profile();

    std::ofstream profile(this->profile, std::ios_base::app);
    if(!profile.is_open())
      throw std::runtime_error(\"Error opening profile!\");

    current.append(std::to_string(timer.elapsed()));
    current.append(\" }\\n\");
    profile << prefix << current;
    current.clear();
  }
};

#endif // __APAC_PROFILER_HPP\
    ");
  close_out output
