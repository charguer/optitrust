open Optitrust

let _ =
  run
    (fun () ->
      set_init_source "test.cpp";
      switch
        [
          (fun () -> Label.add "label1" [cVarDef "v" ]);
          (fun () -> Label.add "label1" [cVarDef "w" ])
        ];
      switch
        [
          (fun () -> Label.add "label2" [cVarDef "n" ]);
          (fun () -> Label.add "label2" [cVarDef "m" ])
        ];
      dump ();
      reset ();
      set_init_source "test_accesses.cpp";
      switch
        [
          (fun () -> Label.add "label1" [cVarDef "p" ]);
          (fun () ->
            Label.add "label1" [cVarDef "q" ];
            switch
              [
                (fun () -> Label.add "label2" [cVarDef "r" ]);
                (fun () -> Label.add "label2" [cVarDef "s" ]);
              ]
          )
        ];
      dump ()
    )
