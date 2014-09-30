let printf = Printf.printf

open Lwt

(* representation of a node -- must be hashable *)
module Node = struct
   type t = int
   let compare = Pervasives.compare
   let hash = Hashtbl.hash
   let equal = (=)
end

let labels = Hashtbl.create 10

(* representation of an edge -- must be comparable *)
module Edge = struct
   type t = string
   let compare = Pervasives.compare
   let equal = (=)
   let default = ""
end

module G = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled(Node)(Edge)

let graph = G.create ()

let note_create parent child : unit =
  printf "%d creates %d\n" parent child;
  G.add_edge graph parent child

let note_input main input : unit =
  printf "%d takes input from %d\n" main input;
  G.add_edge graph input main

let note_merge main input : unit =
  printf "%d merges into %d\n" main input;
  G.add_edge graph input main

let () =
  Lwt.tracer := { Lwt.
    note_create;
    note_input;
    note_merge;
  }

module Time_map = Map.Make(struct type t = int let compare = compare end)

let events = ref Time_map.empty
let now = ref 0
let event = ref 0

let block msg duration fn =
  let t, w = wait () in
  printf "%d is %s\n" (Lwt.id_of_thread t) msg;
  Hashtbl.add labels (Lwt.id_of_thread t) msg;
  events := !events |> Time_map.add (!now + duration) (fun () ->
    Lwt.wakeup w (fn ());
  );
  t

let sleep x =
  block "sleep" x (fun () -> ())

let read_block n =
  block "read_block" 2 (fun () -> "block" ^ string_of_int n)

let send_tcp d =
  block "send_tcp" 2 (fun () -> printf "Transmit: %s\n" d; ())

let main =
  sleep 1 >>= fun () ->
  read_block 1 >>= fun r ->
  send_tcp r

(* module for creating dot-files *)
module Dot = Graph.Graphviz.Dot(struct
   include G (* use the graph module from above *)
   let edge_attributes (_a, e, _b) = [`Label e; `Color 4711]
   let default_edge_attributes _ = []
   let get_subgraph _ = None
   let vertex_attributes _ = [`Shape `Box]
   let vertex_name v =
     try Hashtbl.find labels v
     with Not_found -> string_of_int v
   let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

let () =
  while not (Time_map.is_empty !events) do
    let time, fn = Time_map.min_binding !events in
    printf "main: event %d at t=%d\n" !event time;
    now := time;
    events := !events |> Time_map.remove time;
    fn ()
  done;

  let ch = open_out "graph.dot" in
  Dot.output_graph ch graph;
  close_out ch
