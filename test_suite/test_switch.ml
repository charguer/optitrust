open Optitrust

let _ =
  run
    (fun () ->
      set_init_source "test.cpp";
      switch
        [
          (fun () -> add_label "label1" [cVarDef "v" ]);
          (fun () -> add_label "label1" [cVarDef "w" ])
        ];
      switch
        [
          (fun () -> add_label "label2" [cVarDef "n" ]);
          (fun () -> add_label "label2" [cVarDef "m" ])
        ];
      dump ();
      reset ();
      set_init_source "test_accesses.cpp";
      switch
        [
          (fun () -> add_label "label1" [cVarDef "p" ]);
          (fun () ->
            add_label "label1" [cVarDef "q" ];
            switch
              [
                (fun () -> add_label "label2" [cVarDef "r" ]);
                (fun () -> add_label "label2" [cVarDef "s" ]);
              ]
          )
        ];
      dump ()
    )
