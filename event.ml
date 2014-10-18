open Sexplib.Std

let printf = Printf.printf

type thread = int with sexp

type thread_type =
  | Wait
  | Task
  | Bind
  | Try
  | Choose
  | Pick
  | Join
  | Map
  | Preexisting
  with sexp

let string_of_thread_type = function
  | Wait -> "wait"
  | Task -> "task"
  | Bind -> "bind"
  | Try -> "try"
  | Choose -> "choose"
  | Pick -> "pick"
  | Join -> "join"
  | Map -> "map"
  | Preexisting -> "preexisting"

type op = 
  | Creates of thread * thread * thread_type
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
