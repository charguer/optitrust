open Ast

(** {0:macros Macros}

    This module provides the definitions of internal parameters for all the
    other modules. See [section:flags] for the declarations and default values
    of parameters the user can alter using command-line options or environment
    variables. *)

(** {1 Miscellaneous and pre-sets} *)

(** [cwd]: returns the path to the current working directory. *)
let cwd () : string = Filename.dirname (!Apac_flags.input)

(** [skip fs]: adds the function names from the list [fs] into the set
    [!Apac_flags.skip]. *)
let skip (fs : string list) : unit =
  List.iter (fun f ->
      Apac_flags.skip := Tools.String_set.add f !Apac_flags.skip
    ) fs

(** {1 Pre-processing stage} *)

(** [intermediate_variable]: prefix for the names of the intermediate variables
    we may introduce during the function call extraction (see
    [!Apac_preprocessing.extract_function_calls]). *)
let intermediate_variable : string = "__apac_var"

(** [candidate_mark]: string for marking taskification candidate functions (see
    [!Apac_preprocessing.select_candidates]). *)
let candidate_mark : mark = "__apac_candidate"

(** [candidate_main_mark]: string for marking the [!Apac_flags.main] function as
    taskification candidate (see [!Apac_prologue.select_candidates]). *)
let candidate_main_mark : mark = "__apac_candidate_main"

(** [candidate_body_mark]: string for marking bodies of taskification candidate
    functions (see [!Apac_preprocessing.unify_returns]). *)
let candidate_body_mark : mark = "__apac_candidate_body"

(** [goto_label]: label for replacing [return] statements by [goto] jumps within
    the pre-processing stage (see [Apac_preprocessing.unify_returns]). *)
let goto_label : label = "__apac_exit"

(** [result_variable]: name of the variable for collecting return values when
    replacing [return] statements by [goto] jumps within the pre-processing
    stage (see [Apac_preprocessing.unify_returns]). *)
let result_variable : string = "__apac_result"

(** {1 Task candidate discovery and optimization stage} *)

(** [gwd ()]: returns the path to a directory to store task candidate graphs in.
    If it doesn't exist, create it. *)
let gwd () : string =
  (** Build the full path to the destination directory. *)
  let path = (cwd ()) ^ "/" ^ !Apac_flags.keep_graphs_in in
  if (Sys.file_exists path) then
    if (Sys.is_directory path) then
      (** If the path points to an existing directory, just return it. *)
      path
    else
      (** If the path points to another type of file of the same name, fail. *)
      let error = Printf.sprintf "Apac_macros.gwd: `%s' exists, but it is not \
                                  a directory." path in
      failwith error
  else
    (** Otherwise, create the destination directory with ususal permission set
        and return the path to it. *)
    begin
      Sys.mkdir path 0o755;
      path
    end

(** [gf ()]: returns the path (see [!gwd]) to a file of type [extension] ([dot]
    or [pdf], which is the default) containing the task candidate graph of a
    function [f]. The name of the file may carry an optional [suffix]. *)
let gf ?(suffix : string = "") ?(extension : string = "pdf")
      (f : var) : string =
  let dir = gwd () in
  let name = f.name ^ "-" ^ (string_of_int f.id) ^
               (if suffix <> "" then "-" ^ suffix else "") ^ "." ^ extension in
  dir ^ "/" ^ name

(** {1 Instrumented code generation and run-time analysis} *)

(** {2 Profiling} *)

(** [profile_section_type]: data type of profiling sections. *)
let profile_section_type : string = "apac_s"

(** [profile_section_prefix]: variable name prefix for profiling sections. *)
let profile_section_prefix : string = "__apac_section"

(** [profile_section_init]: method for initializing profiling sections. *)
let profile_section_init : string = "initialize"

(** [profile_section_add]: method for adding profiling parameters. *)
let profile_section_add : string = "add"

(** [profile_section_before]: method for starting execution timer. *)
let profile_section_before : string = "before"

(** [profile_section_after]: method for stopping execution timer. *)
let profile_section_after : string = "after"

(** [profile_header]: profiling header file name. *)
let profile_header : string = "apac_profiling.hpp"

(** [profile_include]: directive to include [!profile_header] in the output
    source code. *)
let profile_include : string = "#include \"" ^ profile_header ^ "\""

(** [profile_hpp profile output]: generates the contents of [!profile_header]
    while considering [profile] as the name of the output profile and saves it
    to an [output] file. *)
let profile_hpp (profile : string) (output : string) : unit =
  let output = open_out output in
  output_string output
    ("\
#ifndef __APAC_PROFILE_HPP
#define __APAC_PROFILE_HPP

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

class " ^ profile_section_type ^ " {
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

void clear() {
  params = 0;
  prefix.clear();
  current.clear();
}

public:
  " ^ profile_section_type ^ "() : params(0), profile(\"" ^ profile ^ "\") { }

  void " ^ profile_section_init ^ "(const std::string id) {
    prefix.append(\"{ \");
    prefix.append(id);
    prefix.append(\", \");
  }

  template <class T> void " ^ profile_section_add ^ "(const T & parameter) {
    current.append(\"R=\");
    current.append(std::to_string(parameter));
    current.append(\", \");
    params++;
  }

  void " ^ profile_section_before ^ "() {
    prefix.append(std::to_string(params));
    prefix.append(\", \");
    timer.start();
  }

  void " ^ profile_section_after ^ "() {
    timer.stop();
    ensure_profile();

    std::ofstream profile(this->profile, std::ios_base::app);
    if(!profile.is_open())
      throw std::runtime_error(\"Error opening profile!\");

    current.append(std::to_string(timer.elapsed()));
    current.append(\" }\\n\");
    profile << prefix << current;
    clear();
  }
};

#endif // __APAC_PROFILE_HPP\
    ");
  close_out output

(** [compile_cmdline c o]: returns the command line for building the source code
    with profiling instructions in [c] into an executable [o] while considering
    the compiler and the compilation options in [!Apac_flags.compile_with]. *)
let compile_cmdline (c : string) (o : string) : string =
  let cc, options = !Apac_flags.compile_with in
  match cc with
  | Gnu ->
     let options = if options = "" then "" else " " ^ options in
     "g++ -o " ^ o ^ " " ^ c ^ " " ^ options
  | Clang ->
     let options = if options = "" then "" else " " ^ options in
     "clang++ -o " ^ o ^ " " ^ c ^ " " ^ options
  | Custom -> options

(** [profile_cmdline p]: returns the command line for running the executable
    with profiling instructions [p] while considering additional paramaters in
    [!Apac_flags.profile_with], if any. *)
let profile_cmdline (p : string) : string =
  if !Apac_flags.profile_with_custom <> "" then
    !Apac_flags.profile_with_custom
  else
    let args =
      if !Apac_flags.profile_with = "" then ""
      else " " ^ !Apac_flags.profile_with in
    p ^ args

(** {2:modeling Modeling} *)

(** [model_pow]: name of the power function in polynome expressions. *)
let model_pow : string = "apac_fpow"

(** [model_parameter]: prefix of generic parameter names in a model file, e.g.
    [X1] where [X] is the prefix. *)
let model_parameter : string = "X"

(** [model_pow_re]: regular expression matching calls, to [!model_pow] in a
    model file, e.g. [apac_fpow<2>(X0)]. *)
let model_pow_re : string = model_pow ^ "<\\([0-9]+\\)>(" ^
                                  model_parameter ^ "\\([0-9]+\\))"

(** [model_na]: placeholder for empty execution time formula. *)
let model_na : string = "None"

(** [model_na]: regular expression matching a single line in a model file. *)
let model_re : string = "\\([0-9]+\\)(nbparams=[0-9]+)=\\(" ^ model_na ^
                          "\\|[^=\n]+\\)"

(** [model_cmdline p m]: returns the command line for running the execution time
    modeler on the profile [p] while considering additional paramaters in
    [!Apac_flags.model_with], if any, and saving the resulting model in [m]. *)
let model_cmdline (p : string) (m : string) : string =
  let args = if !Apac_flags.model_with = "" then ""
             else " " ^ !Apac_flags.model_with in
  "python3 -m apac_modelizer --profile " ^ p ^ " --model " ^ m ^ args

(** {2 Common} *)

(** [runtime_analysis_files ()]: returns the paths (see [!cwd]) to the profiling
    header, the source code with profiling instructions, the corresponding
    binary executable, the resulting profile and model files. *)
let runtime_analysis_files () : string * string * string * string * string =
  let here = cwd () ^ "/" in
  let this = Filename.remove_extension (Filename.basename !Apac_flags.input) in
  let header = here ^ profile_header in
  let here = here ^ this in
  let code = here ^ "_profiling" in
  let binary = here ^ "_profiling.exe" in
  let profile = here ^ ".profile" in
  let model =  here ^ ".model" in
  (header, code, binary, profile, model)

(** [runtime_analysis_logs ()]: returns the paths (see [!cwd]) to the build, run
    and modeling logs. *)
let runtime_analysis_logs () : string * string * string =
  let here = cwd () ^ "/" in
  let this = Filename.remove_extension (Filename.basename !Apac_flags.input) in
  let here = here ^ this in
  let build = here ^ "_build.log" in
  let run = here ^ "_run.log" in
  let modeling = here ^ "_modeling.log" in
  (build, run, modeling)

(** {1 Parallel code generation} *)

(** [apac_variable]: enumeration of instrumentation variables that might appear
    in the resulting source code. *)
type apac_variable =
  (** Gives the current task count. *)
  | ApacCount
  (** Gives the current task depth. *)
  | ApacDepth
  (** Task-private copy of [ApacDepth]. *)
  | ApacDepthLocal
  (** True if the task count limit was not reached yet. *)
  | ApacCountOk
  (** True if the task depth limit was not reached yet. *)
  | ApacDepthOk
  (** True when the task count is not limited. *)
  | ApacCountInfinite
  (** True when the task depth is not limited. *)
  | ApacDepthInfinite
  (** Gives the maximum task count. *)
  | ApacCountMax
  (** Gives the maximum task depth. *)
  | ApacDepthMax
  (** Task submission cut-off value. *)
  | ApacCutOff

(** [get_apac_variable]: generates a string representation of the
    instrumentation variable [v]. *)
let get_apac_variable (v : apac_variable) : string =
  match v with
  | ApacCount -> "__apac_count"
  | ApacDepth -> "__apac_depth"
  | ApacDepthLocal -> "__apac_depth_local"
  | ApacCountOk -> "__apac_count_ok"
  | ApacDepthOk -> "__apac_depth_ok"
  | ApacCountInfinite -> "__apac_count_infinite"
  | ApacDepthInfinite -> "__apac_depth_infinite"
  | ApacCountMax -> "__apac_count_max"
  | ApacDepthMax -> "__apac_depth_max"
  | ApacCutOff -> "__apac_cutoff"

(** [heapify_mark]: string for marking sequences of statements for heapification
    (see [Apac_epilogue.heapify]). *)
let heapify_mark : mark = "__apac_heapify"

(** [heapify_breakable_mark]: string for marking sequences of statements,
    possibly containing [break] or [continue] statements, for heapification (see
    [Apac_epilogue.heapify]). *)
let heapify_breakable_mark : mark = "__apac_heapify_breakable"

(** [count_infinite]: name of the optional environment variable allowing the
    end-user to disable the task creation cut-off based on the number of
    submitted tasks. *)
let count_infinite : string = "APAC_TASK_COUNT_INFINITE"

(** [depth_infinite]: name of the optional environment variable allowing the
    end-user to disable the task creation cut-off based on the task depth. *)
let depth_infinite : string = "APAC_TASK_DEPTH_INFINITE"

(** [count_max]: name of the optional environment variable allowing the end-user
    to set the maximum count of submitted tasks. *)
let count_max : string = "APAC_TASK_COUNT_MAX"

(** [depth_max]: name of the optional environment variable allowing the end-user
    to set the maximum task depth. *)
let depth_max : string = "APAC_TASK_DEPTH_MAX"

(** [depth_sequential]: prefix for the names of sequential implementations of
    parallel functions the output program switches to when the task depth
    reaches [depth_max]. *)
let depth_sequential : string = "__apac_sequential_"

(** [depth_placeholder]: placeholder string for the pattern matching the names
    of existing sequential implementations of parallel functions (see
    [!Apac_flags.sequential]). *)
let depth_placeholder : string = "@f"

(** [execution_time_cutoff]: name of the optional environment variable allowing
    the end-user to set the cut-off value limiting the insertion of tasks
    according to their estimated execution time. *)
let execution_time_cutoff : string = "APAC_EXECUTION_TIME_CUTOFF"

(** [execution_time_min]: the estimation of the minimal execution time for a
    task to be worth of spawning. *)
let execution_time_min : string = "2.22100e-6"

(** [pow]: an implementation of [!model_pow] to include in the parallel
    source code when using execution time modeling (see [section:modeling]). *)
let pow : string = "
template <class T> T " ^ model_pow ^ "(int exp, const T & base) {
  T result = T(1);
  T pow = base;
  int i = exp;
  while(i){
    if(i & 1){
      result *= pow;
    }
    pow *= pow;
    i /= 2;
  }
  return result;
}
"
