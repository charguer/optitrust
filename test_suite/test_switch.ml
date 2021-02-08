open ScriptTools

let _ =
  run
    (fun () ->
      set_init_source "test.cpp";
      switch
        [
          (fun () -> add_label "label1" [cVarDef ~name:"v" ()]);
          (fun () -> add_label "label1" [cVarDef ~name:"w" ()])
        ];
      switch
        [
          (fun () -> add_label "label2" [cVarDef ~name:"n" ()]);
          (fun () -> add_label "label2" [cVarDef ~name:"m" ()])
        ];
      dump ();
      reset ();
      set_init_source "test_accesses.cpp";
      switch
        [
          (fun () -> add_label "label1" [cVarDef ~name:"p" ()]);
          (fun () ->
            add_label "label1" [cVarDef ~name:"q" ()];
            switch
              [
                (fun () -> add_label "label2" [cVarDef ~name:"r" ()]);
                (fun () -> add_label "label2" [cVarDef ~name:"s" ()]);
              ]
          )
        ];
      dump ()
    )
