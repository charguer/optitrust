include Instr_basic

let inline_last_write ~write:(write : Target.target) ?(delete : bool = true) (tg : Target.target) : unit =
  Instr_basic.read_last_write ~write tg;
  if delete then Instr_basic.delete write 

