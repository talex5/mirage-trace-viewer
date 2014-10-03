let printf = Printf.printf

type thread = Lwt.thread_id

type op = 
  | Creates of thread * thread
  | Reads of thread * thread
  | Resolves of thread * thread
  | Becomes of thread * thread
  | Label of thread * string

type 'a t = {
  time : float;
  op : 'a;
}

let fmt ch tid = Printf.fprintf ch "%d" (tid : thread :> int)

let print_event t =
  match t.op with
  | Creates (parent, child) -> printf "[%.1f] %a creates %a\n" t.time fmt parent fmt child
  | Reads (a, b) -> printf "[%.1f] %a reads %a\n" t.time fmt a fmt b
  | Resolves (a, b) -> printf "[%.1f] %a resolves %a\n" t.time fmt a fmt b
  | Becomes (a, b) -> printf "[%.1f] %a becomes %a\n" t.time fmt a fmt b
  | Label (a, b) -> printf "[%.1f] %a: %s\n" t.time fmt a b

let event_log : op t list ref = ref []

let record event =
  event_log := event :: !event_log;
  print_event event
