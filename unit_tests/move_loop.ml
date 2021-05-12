open Optitrust

let _ = 
    run(
        fun () ->
        set_init_source "move_loop.cpp";
       (* move_loop ~path:() ~move_after:string ~move_before:string 
          
          if path is pointing to the inner loop, then you need to work
          out the path to the outer loop with corresponding name

          let outer_loop_path = [cFor ~index:move_before ~body:(path)] in
          check that outer_loop_path resolves to exactly one solution
          let inner_loop_trm = (resolve_path path) in
          let name = get_loop_index inner_loop_trm

          call your function move_loop_before outer_loop_path name


          todo: test that you get an error in this case, tell the user to first rename the loop:
            for a
                for b
                    for a
                        for c


                        move c before a
       *)


        move_loop "c" ~move_after:"d";
        move_loop "c" ~move_before:"d";
        (*
        move_loop ~path:[cFun ~name:"f"; cFor "c"] ~move_after:"d";
        move_loop ~move_before:[cFun ~name:"f"; cFor "c"] ~move_after:"d";
        move_loop ~name:"c" ~move_after:"d" ();
        move_loop "c" ~move_before:"d" ();
        
       
        *)

        
        dump ()
    )

    (* nohup meld file1 file2 >/dev/null 2>&1 
       killall meld || echo "nothing todo" > /dev/null *)