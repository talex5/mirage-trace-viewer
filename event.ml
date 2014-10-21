open Sexplib.Std

let printf = Printf.printf

type thread = int with sexp

type op = 
  | Creates of thread * thread * string
  | Reads of thread * thread
  | Resolves of thread * thread * string option
  | Becomes of thread * thread
  | Label of thread * string
  | Switch of thread
  | Gc of float
  | Increases of thread * string * int
  with sexp

type t = {
  time : float;
  op : op;
} with sexp

let fmt ch tid = Printf.fprintf ch "%d" (tid : thread :> int)
