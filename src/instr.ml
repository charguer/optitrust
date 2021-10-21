include Instr_basic

let inline_last_write ~write:(write : Target.target) (tg : Target.target) : unit =
  Instr_basic.read_last_write ~write tg;
  Instr_basic.delete write 
