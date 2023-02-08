let printf = Printf.printf

type thread = int

type read_outcome = Read_resolved | Read_resolved_later | Read_sleeping

type gc_kind = Minor | Major | Unknown

type op =
  | Creates of thread * thread * string
  | Reads of thread * thread * read_outcome
  | Resolves of thread * thread * string option
  | Becomes of thread * thread
  | Label of thread * string
  | Switch of thread
  | Gc of float * gc_kind
  | Increases of thread * string * int    (* Deprecated; use Counter_value instead *)
  | Counter_value of thread * string * int
  | Signals_and_switches of thread * thread
  | Signals of thread * thread

type t = {
  time : float;
  op : op;
}

let fmt ch tid = Printf.fprintf ch "%d" (tid : thread :> int)
