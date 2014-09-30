let printf = Printf.printf

open Lwt

let labels = Hashtbl.create 10
let now = ref 0

let () =
  let open Event in

  let record op =
    Event.record {time = float_of_int !now; op} in

  let note_create parent child =
    `creates (parent, child) |> record in

  let note_input main input =
    `notifies (input, main) |> record in

  let note_merge main input =
    `becomes (input, main) |> record in

  Lwt.tracer := { Lwt.
    note_create;
    note_input;
    note_merge;
  }

module Time_map = Map.Make(struct type t = int let compare = compare end)

let events = ref Time_map.empty
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

let () =
  while not (Time_map.is_empty !events) do
    let time, fn = Time_map.min_binding !events in
    printf "main: event %d at t=%d\n" !event time;
    now := time;
    events := !events |> Time_map.remove time;
    fn ()
  done;

  Render.render (Simplify.simplify !Event.events) "graph.png"
