open Optitrust
open Target
open Ast

let _ = Flags.pretty_matrix_notation := true
(* let _ = Flags.analyse_stats := true *)

module Function = struct
  include Function

  (* TODO:
    - cFuns = cOr ...
    Function.inline on a definition, finding all calls
    *)
  let inline_all (funs : vars) : unit =
    funs |> List.iter (fun f ->
      Function.inline ~delete:true [nbMulti; cFun f])
end

module Variable = struct
  include Variable

  (* TODO:
     - cVarDefs = cOr ...
     - cOrMap cVarDef vars ?
    *)
  let inline_all (vars : vars) : unit =
    vars |> List.iter (fun v -> Variable.inline [cVarDef v])
end

module Matrix = struct
  include Matrix

  let elim_accesses (name : string) : unit =
    (* FIXME:
    show [cArrayRead "weights_sobelX"];
    show [sExpr "weights_sobelX["];
    *)
    Matrix.elim_mops [nbMulti; sExpr (name ^ "[")];
    Arrays.elim_accesses [nbMulti; cVarDef name]
end

(* keep local *)
module Image = struct
  let fusion_array_writes ?(nest_of = 1) ~(index : var) (arrays : var list) : unit =
    let writes = cOr (List.map (fun a -> [cArrayWrite a]) arrays) in
    Loop.fusion_targets ~nest_of [nbMulti; cFor ~body:[writes] index]

  let align_stop_extend_start (index : var) ~(start : trm) ~(stop : trm) : unit =
    Loop.shift (StopAt stop) [nbMulti; cFor index];
    Loop.extend_range ~start:(ExtendTo start) [nbMulti; cFor index];
    (* TODO: transfo to remove useless if inside a for *)
end

let _ = Run.script_cpp (fun () ->
  bigstep "inline operators";
  !! Function.inline_all ["conv2D"];
  !! Loop.unroll ~nest_of:2 [nbMulti; cFor ~body:[cPlusEqVar "acc"] "i"];
  !! Matrix.elim_accesses "weights";
  !! Function.inline_all ["grayscale"; "sobelX"; "sobelY"; "sum3x3"; "mul"; "coarsity"];
  (* cFunBody "harris"; cConstDef ""; *)
  !! Variable.inline_all ["h1"; "w1"; "h2"; "w2"];
  !! Arith.(simpl gather) [nbMulti; cFor ""; dForStop];

  bigstep "fuse operators";
  let array_accs_to_rename = ["ix"; "iy"; "sxx"; "sxy"; "syy"] in
  !! array_accs_to_rename |> List.iter (fun array ->
    Variable.rename ~into:("acc_" ^ array) [cFor ~body:[cArrayWrite array] ""; cVarDef "acc"]
  );
  let fuse = Image.fusion_array_writes ~index:"y" in
  !! fuse ~nest_of:2 ["ix"; "iy"];
  !! fuse ~nest_of:2 ["ixx"; "ixy"; "iyy"];
  !! fuse ~nest_of:2 ["sxx"; "sxy"; "syy"; "out"];
  !! Image.align_stop_extend_start "y" ~start:(trm_int 0) ~stop:(trm_var "h");
  !! fuse ["gray"; "ix"; "ixx"; "out"];

  bigstep "circular buffers";
  let circular_buffer v = Matrix.storage_folding ~dim:0 ~size:(trm_int 3) ~var:v [cFunBody "harris"] in
  !! List.iter circular_buffer ["gray"; "ix"; "iy"];

  !!! Instr.read_last_write [nbMulti; cArrayRead "sxx"];
  !! ["sxx"; "sxy"; "syy"] |> List.iter (fun to_elim ->
    Instr.read_last_write [nbMulti; cArrayRead to_elim];
    (* FIXME: dangerous transformation? *)
    (* Instr.delete [cArrayWrite to_elim];
    Matrix.delete ~var:to_elim [cFunBody "harris"]; *)
    (* TODO: Matrix.delete_not_read *)
  );
  (* TODO: not possible like this
     Matrix.read_last_write that can
  !! ["ixx"; "ixy"; "iyy"] |> List.iter (fun to_elim ->
    Instr.read_last_write [nbMulti; cArrayRead to_elim];
    (* FIXME: dangerous transformation? *)
    Instr.delete [cArrayWrite to_elim];
    Matrix.delete ~var:to_elim [cFunBody "harris"];
  ); *)
  (* TODO: do before fuse + Variable.bind_common cArrayRead ["ix"; "iy"] *)

  bigstep "parallelism";
  !! Omp.header ();

  bigstep "code details";
  !! Matrix.elim_mops [];
)