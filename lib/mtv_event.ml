let printf = Printf.printf

type thread = int

type read_outcome = Read_resolved | Read_sleeping

type op = 
  | Creates of thread * thread * string
  | Reads of thread * thread * read_outcome
  | Resolves of thread * thread * string option
  | Becomes of thread * thread
  | Label of thread * string
  | Switch of thread
  | Gc of float
  | Increases of thread * string * int
  | Signals of thread * thread

type t = {
  time : float;
  op : op;
}

let fmt ch tid = Printf.fprintf ch "%d" (tid : thread :> int)
