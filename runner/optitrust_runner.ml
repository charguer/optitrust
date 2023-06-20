(* This is an dynamic loader for OCaml, it takes an OCaml plugin as its
 * first argument and execute it as if it was an OCaml program. *)

let () =
  if Array.length Sys.argv >= !Arg.current + 2 then (
    Arg.current := !Arg.current + 1;
    let plugin_name = Sys.argv.(!Arg.current) in
    if not (Sys.file_exists plugin_name) then begin
      Printf.printf "OptiTrust runner did not find the compiled script file: %s\n" plugin_name;
      exit 1
    end;
    try
      Dynlink.loadfile plugin_name
    with
    | Dynlink.Error (Library's_module_initializers_failed err) -> (
        (* Try not to pollute the error backtrace with runner details *)
        Printf.eprintf "Uncaught exception in %s: %s\n" plugin_name (Printexc.to_string err);
        begin match Printexc.(backtrace_slots (get_raw_backtrace ())) with
          | Some bt ->
            let exception Stop in
            begin try
              Array.iteri (fun nth btslot ->
                  begin match Printexc.Slot.name btslot with
                    | Some n when String.starts_with ~prefix:"Dynlink." n -> raise Stop
                    | _ -> ()
                  end;
                  begin match Printexc.Slot.format nth btslot with
                    | Some s -> Printf.eprintf "%s\n" s
                    | _ -> ()
                  end
                ) bt
            with Stop -> ()
            end
          | None -> Printf.eprintf "The backtrace was not recorded\n"
        end;
        exit 1
      )
    | Dynlink.Error err -> (
        Printf.eprintf "OptiTrust runner dynlink error %s: %s" plugin_name (Dynlink.error_message err);
        exit 1
      )
  ) else (
    Printf.eprintf "Error: Expected a transformation script plugin as first argument\n";
    exit 1
  )
