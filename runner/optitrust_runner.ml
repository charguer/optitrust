(* This is an dynamic loader for OCaml, it takes an OCaml plugin as its
 * first argument and execute it as if it was an OCaml program. *)

let () =
  if Array.length Sys.argv >= !Arg.current + 2 then (
    Arg.current := !Arg.current + 1;
    let plugin_name = Sys.argv.(!Arg.current) in
    try
      Dynlink.loadfile plugin_name
    with
      Dynlink.Error err -> (
        (*let sbt = Printexc.get_backtrace() in
        Printf.eprintf "Error loading %s: %s\n%s" plugin_name (Dynlink.error_message err) sbt; *)
        Printf.printf "OptiTrust runner did not find the compiled script file: %s\n" plugin_name;
        exit 1
      )
  ) else
    Printf.eprintf "Error: Expected a transformation script plugin as first argument\n";;
    exit 1
