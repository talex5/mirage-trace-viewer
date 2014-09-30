let printf = Printf.printf

type thread = int

type op = 
  [ `creates of thread * thread
  | `notifies of thread * thread
  | `becomes of thread * thread ]

type 'a t = {
  time : float;
  op : 'a;
}

let print_event t =
  match t.op with
  | `creates (parent, child) -> printf "[%.1f] %d creates %d\n" t.time parent child
  | `notifies (sender, recv) -> printf "[%.1f] %d notifies %d\n" t.time sender recv
  | `becomes (old, replacement) -> printf "[%.1f] %d becomes %d\n" t.time old replacement

let events : op t list ref = ref []

let record event =
  events := event :: !events;
  print_event event
