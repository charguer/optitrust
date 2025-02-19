open Optitrust

type options =
  | Help
  | Version
  | Output
  | Verbose
  | WrapAt
  | Include
  | Constify
  | ConstifyQuietly
  | Main
  | Omit
  | Skip
  | KeepGraphs
  | KeepGraphsIn
  | Traversal
  | Profile
  | CompileWith
  | RunWith
  | RunWithCustom
  | ModelWith
  | CutOffCount
  | CutOffDepth
  | CutOffCountFactor
  | CutOffDepthMax
  | CutOffDepthSequential

let pf = Printf.printf

let pe = Printf.eprintf

let pl = print_endline
  
let purpose (option : options) : string =
  let keep_graphs_in = !Apac_flags.keep_graphs_in in
  let compiler, options = !Apac_flags.compile_with in
  match option with
  | Help -> "Show this help message and exit."
  | Version -> "Show version information and exit."
  | Output -> "Place the resulting parallel source code into FILE."
  | Verbose -> "Enable verbose output."
  | WrapAt -> "In the resulting source code, wrap lines at COLUMN. By default, \
               the lines end at the column " ^
                (string_of_int !Flags.code_print_width) ^
                  "."
  | Include -> "When parsing the input source code file, look for include \
                files also in DIRECTORY. By default, the parser considers only \
                the directory where the input source code file resides."
  | Constify -> "Analyze function definitions in the input source code, \
                 automatically determine which arguments are read-write and \
                 which are read-only so as to allow for a potentially more \
                 accurate dependency analysis and augment function prototypes \
                 with the `const' keyword, i.e. constify them, whenever it is \
                 possible."
  | ConstifyQuietly -> "Perform only the analysis phase of the constification \
                        without augmenting function prototypes with the \
                        `const' keyword at the end. The compiler will still \
                        take advantage of the constification, but it will not \
                        affect the output source code."
  | Main -> "Instead of the `main' function, consider FUNCTION as the starting \
             point of the parallel execution, i.e. place the master task group \
             in FUNCTION."
  | Omit -> "Omit functions the name of which matches REGEX during the \
             compilation process."
  | Skip -> "Exclude FUNCTION from the selection of taskification candidates."
  | KeepGraphs -> "Dump the initial task candidate graph representation of the \
                   input source code and then after each task candidate graph \
                   transformation pass into separate files in Portable \
                   Document Format (PDF). By default, the compiler stores the \
                   files in a folder named`" ^
                    keep_graphs_in ^
                      "' it creates, if it does not exist, in the current \
                       working directory."
  | KeepGraphsIn -> "Place the task candidate graph files into a folder at \
                     PATH."
  | Traversal -> "Follow the algorithmic VARIANT when traversing task \
                  candidate graphs. Possible values are `strict', `priority', \
                  and `relaxed'. The `strict' traversal ensure the respect of \
                  the initial lexicographic order of statements in the input \
                  source code. The `priority' traversal is a variant of the \
                  breadth-first traversal. However, after visiting the first \
                  task candidate vertex in a series of inter-dependent task \
                  candidates, it prefers to finish visiting the entire series \
                  before moving to a sibling task candidate vertex. Finally, \
                  the `relaxed' variant represents the vanilla breadth-first \
                  traversal. By default, the compiler follows the `strict' \
                  traversal variant."
  | Profile -> "Enable execution time profiling and modeling for a finer task \
                granularity control in the resulting parallel source code. To \
                this end, the compiler annotates the input source code with a \
                series of profiling instructions, then builds a profiling \
                executable thanks to a C compiler and runs it so as to record \
                the execution time of different task candidates."
  | CompileWith -> "Run the C language COMPILER with additional OPTIONS to \
                    build the profiling executable. Possible values for \
                    COMPILER are `gnu', `clang' and `custom'. In the case of \
                    the latter, the OPTIONS string acts as the custom build \
                    command. The default setting is`" ^
                     (match compiler with
                        Gnu -> "gnu" | Clang -> "clang" | Custom -> "custom") ^
                       "'. `" ^ options ^ "' are the built-in options."
  | RunWith -> "Run the profiling executable with additional OPTIONS."
  | RunWithCustom -> "Run the profiling executable using the custom \
                      COMMAND-LINE."
  | ModelWith -> "The compiler resorts to an execution time modeler. This \
                  option allows for passing additional arguments to the latter \
                  when calling it."
  | CutOffCount -> "Annotate the resulting parallel source code so as to \
                    enable task granularity control according to global task \
                    submission counter."
  | CutOffDepth -> "Annotate the resulting parallel source code so as to \
                    enable task granularity control according to global task \
                    parallelism depth counter."
  | CutOffCountFactor -> "Submit at most FACTOR times the maximum thread count \
                          parallelizable tasks. By default, FACTOR is " ^
                           (string_of_int !Apac_flags.count_max_thread_factor)
                           ^ ". Note that it is also possible to set FACTOR \
                              using the `" ^
                             Apac_macros.count_max ^
                               "' environment variable or allow the submission \
                                of an unlimited number of parallelizable tasks \
                                using the `" ^
                                 Apac_macros.count_infinite ^
                                   "' environment variable when running the \
                                    resulting task-based parallel application."
  | CutOffDepthMax -> "Nest at most MAX parallelizable tasks. By default, MAX \
                       is " ^
                        (string_of_int !Apac_flags.depth_max_default) ^
                          ". Note that it is also possible to set MAX using \
                           the `" ^
                            Apac_macros.count_max ^
                              "' environment variable or allow the nesting of \
                               an unlimited number of parallelizable tasks \
                               using the `" ^
                                Apac_macros.count_infinite ^
                                  "' environment variable when running the \
                                   resulting task-based parallel application."
  | CutOffDepthSequential -> "In the resulting parallel source code, the task \
                              granularity control mechanism relying on a \
                              per-thread parallelism depth counter switches to \
                              the sequential implementation of a given \
                              parallel function when the counter reaches " ^
                               (string_of_int !Apac_flags.depth_max_default) ^
                                 " (adjustable using the `--cutoff-depth-max' \
                                  option). To this end, the compiler takes \
                                  care of preserving a sequential \
                                  implementation of each function it \
                                  parallelizes in the output source code. \
                                  However, if the input source code already \
                                  contains a copy of each function to \
                                  parallelize the user excludes from the \
                                  compilation process using the `--omit' \
                                  option, it is possible to use these copies \
                                  instead of generating new ones. In order to \
                                  allow the compiler to identify an existing \
                                  sequential implementation of a function, the \
                                  user should provide an adequate sequential \
                                  function name REGEX using the \
                                  `--cutoff-depth-sequential` in which `%f' \
                                  represents the function name, e.g. \
                                  `\"%f_seq$\"' to tell the compiler that the \
                                  names of the sequential implementations end \
                                  with `_seq'."

let usage () : string =
  let executable = Sys.argv.(0) in
  "Usage: " ^ executable ^ " [OPTIONS] FILE\n" ^
    "Automatically translate a sequential C source FILE into a task-based \
     parallel C source file."

let info () : unit =
  pl "Written by Marek Felšöci.";
  pl "Report bugs to: <marek.felsoci@lip6.fr>";
  pl "Source code available at: \
      <https://github.com/charguer/optitrust/tree/apac/main>"

let help () : unit =
  pl (usage ());
  pf "\n";
  pl "Possible options are:\n";
  pl "  Help and information";
  pf "    -h, --help       %s\n" (purpose Help);
  pf "    -V, --version    %s\n\n" (purpose Version);
  pl "  General configuration";
  pf "    -I, --includes=DIRECTORY[,DIRECTORY]    %s\n\n" (purpose Include);
  pf "    -o, --output=FILE                       %s\n" (purpose Output);
  pf "    -v, --verbose                           %s\n" (purpose Verbose);
  pf "    -w, --wrap-at=COLUMN                    %s\n\n" (purpose WrapAt);
  pl "  Pre-processing";
  pf "    -c, --constify                %s\n" (purpose Constify);
  pf "    --constify-quietly            %s\n" (purpose ConstifyQuietly);
  pf "    -m, --main=FUNCTION           %s\n\n" (purpose Main);
  pf "    --omit=REGEX                  %s\n\n" (purpose Omit);
  pf "    --skip=FUNCTION[,FUNCTION]    %s\n\n" (purpose Skip);
  pl "  Task candidate graph";
  pf "    -k, --keep-graphs          %s\n" (purpose KeepGraphs);
  pf "    --keep-graphs-in=PATH      %s\n" (purpose KeepGraphsIn);
  pf "    -t, --traversal=VARIANT    %s\n\n" (purpose Traversal);
  pl "  Execution time profiling and modeling";
  pf "    -p, --profile                        %s\n" (purpose Profile);
  pf "    --compile-with=COMPILER[,OPTIONS]    %s\n" (purpose CompileWith);
  pf "    --run-with=OPTIONS                   %s\n" (purpose RunWith);
  pf "    --run-with-custom=COMMAND-LINE       %s\n" (purpose RunWithCustom);
  pf "    --model-with=OPTIONS                 %s\n\n" (purpose ModelWith);
  pl "  Parallel code generation";
  pf "    --cutoff-count                     %s\n" (purpose CutOffCount);
  pf "    --cutoff-depth                     %s\n" (purpose CutOffDepth);
  pf "    --cutoff-count-factor=FACTOR       %s\n" (purpose CutOffCountFactor);
  pf "    --cutoff-depth-max=MAX             %s\n\n" (purpose CutOffDepthMax);
  pf "    --cutoff-depth-sequential=REGEX    %s\n\n"
    (purpose CutOffDepthSequential);
  info ()

let version () : unit =
  pf "Automatic PArallelizer for C version %s\n" (Version.current);
  info ()

let parse_arguments () =
  let compile_with : string ref = ref "clang," in
  let includes : string ref = ref "" in
  let skip : string ref = ref "" in
  let arguments = [
      ("-h", Arg.Unit (fun () -> help (); exit 0), purpose Help);
      ("-V", Arg.Unit (fun () -> version (); exit 0), purpose Version);
      ("--version", Arg.Unit (fun () -> version (); exit 0), purpose Version);
      ("-o", Arg.Set_string Apac_flags.output, purpose Output);
      ("--output", Arg.Set_string Apac_flags.output, purpose Output);
      ("-v", Arg.Set Apac_flags.verbose, purpose Verbose);
      ("--verbose", Arg.Set Apac_flags.verbose, purpose Verbose);
      ("-w",
       Arg.Int (fun n -> Flags.code_print_width := n),
       purpose WrapAt);
      ("--wrap-at",
       Arg.Int (fun n -> Flags.code_print_width := n),
       purpose WrapAt);
      ("-I", Arg.Set_string includes, purpose Include);
      ("--includes", Arg.Set_string includes, purpose Include);
      ("-c", Arg.Set Apac_flags.constify, purpose Constify);
      ("--constify", Arg.Set Apac_flags.constify, purpose Constify);
      ("--constify-quietly",
       Arg.Set Apac_flags.constify_quietly,
       purpose ConstifyQuietly);
      ("-m", Arg.Set_string Apac_flags.main, purpose Main);
      ("--main", Arg.Set_string Apac_flags.main, purpose Main);
      ("--omit", Arg.Set_string Apac_flags.omit, purpose Omit);
      ("--skip", Arg.Set_string skip, purpose Skip);
      ("-k", Arg.Set Apac_flags.keep_graphs, purpose KeepGraphs);
      ("--keep-graphs", Arg.Set Apac_flags.keep_graphs, purpose KeepGraphs);
      ("--keep-graphs-in",
       Arg.Set_string Apac_flags.keep_graphs_in,
       purpose KeepGraphsIn);
      ("-t", Arg.Set_string Apac_flags.traversal, purpose Traversal);
      ("--traversal", Arg.Set_string Apac_flags.traversal, purpose Traversal);
      ("-p", Arg.Set Apac_flags.profile, purpose Profile);
      ("--profile", Arg.Set Apac_flags.profile, purpose Profile);
      ("--compile-with", Arg.Set_string compile_with, purpose CompileWith);
      ("--run-with", Arg.Set_string Apac_flags.profile_with, purpose RunWith);
      ("--run-with-custom",
       Arg.Set_string Apac_flags.profile_with_custom,
       purpose RunWithCustom);
      ("--model-with", Arg.Set_string Apac_flags.model_with, purpose ModelWith);
      ("--cutoff-count", Arg.Set Apac_flags.cutoff_count, purpose CutOffCount);
      ("--cutoff-depth", Arg.Set Apac_flags.cutoff_depth, purpose CutOffDepth);
      ("--cutoff-count-factor",
       Arg.Int (fun n -> Apac_flags.count_max_thread_factor := n),
       purpose CutOffCountFactor);
      ("--cutoff-depth-max",
       Arg.Int (fun n -> Apac_flags.depth_max_default := n),
       purpose CutOffDepthMax);
      ("--cutoff-depth-sequential",
       Arg.Set_string Apac_flags.sequential,
       purpose CutOffDepthSequential)
    ] in
  begin
    try
      Arg.parse_argv Sys.argv arguments (fun a ->
          if !Apac_flags.input <> "" then
            prerr_endline ("apac: redefining input file, taking `" ^ a ^ "'");
          Apac_flags.input := a
        ) (usage ());
    with
    | Arg.Bad error ->
       let error = String.split_on_char '\n' error in
       let error = if (List.length error) > 0 then List.nth error 0 else "" in
       let error = if error <> "" then String.split_on_char ':' error else [] in
       let error =
         if (List.length error) > 1 then String.trim (List.nth error 1)
         else "something went wrong" in
       pe "Error: %s\n%s\n" error (usage ());
       exit 2
    | Arg.Help _ -> help (); exit 0
  end;
  let compile_with =
    if String.contains !compile_with ',' then
      String.split_on_char ',' !compile_with
    else [!compile_with; ""] in
  let _, options = !Apac_flags.compile_with in
  Apac_flags.compile_with :=
    if (List.nth compile_with 0) = "gnu" then
      (Gnu, options ^ " " ^ (List.nth compile_with 1))
    else if (List.nth compile_with 0) = "clang" then
      (Clang, options ^ " " ^ (List.nth compile_with 1))
    else if (List.nth compile_with 0) = "custom" then
      (Custom, List.nth compile_with 1)
    else !Apac_flags.compile_with;
  let includes = String.split_on_char ',' !includes in
  if (List.length includes) > 0 then
    Flags.c_parser_includes := includes;
  let skip = String.split_on_char ',' !skip in
  if (List.length skip) > 0 then
    Apac_macros.skip skip;
  if !Apac_flags.input = "" then
    failwith "apac: no input file\nNothing to compile!";
  if !Apac_flags.output = "" then
    let base = Filename.chop_extension !Apac_flags.input in
    let extension = Filename.extension !Apac_flags.input in
    Apac_flags.output := base ^ "_parallel" ^ extension

let () =
  parse_arguments ();
  Run.apac !Apac_flags.input !Apac_flags.output
