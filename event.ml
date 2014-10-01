let printf = Printf.printf

type thread = Lwt.thread_id

type op = 
  [ `creates of thread * thread
  | `reads of thread * thread
  | `resolves of thread * thread
  | `becomes of thread * thread ]

type 'a t = {
  time : float;
  op : 'a;
}

let fmt ch tid = Printf.fprintf ch "%d" (tid : thread :> int)

let print_event t =
  match t.op with
  | `creates (parent, child) -> printf "[%.1f] %a creates %a\n" t.time fmt parent fmt child
  | `reads (a, b) -> printf "[%.1f] %a reads %a\n" t.time fmt a fmt b
  | `resolves (a, b) -> printf "[%.1f] %a resolves %a\n" t.time fmt a fmt b
  | `becomes (a, b) -> printf "[%.1f] %a becomes %a\n" t.time fmt a fmt b

let events : op t list ref = ref []

let record event =
  events := event :: !events;
  print_event event
