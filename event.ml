open Sexplib.Std

let printf = Printf.printf

type thread = int with sexp

type op = 
  | Creates of thread * thread
  | Reads of thread * thread
  | Resolves of thread * thread * string option
  | Becomes of thread * thread
  | Label of thread * string
  | Switch of thread
  with sexp

type t = {
  time : float;
  op : op;
} with sexp

let fmt ch tid = Printf.fprintf ch "%d" (tid : thread :> int)
