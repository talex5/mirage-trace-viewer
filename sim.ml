let printf = Printf.printf

open Lwt

let labels = Hashtbl.create 10
let now = ref 0.0
let toplevel = Lwt.current_id ()

let () =
  let open Event in

  let last_was_creates = ref false in

  let record op =
    last_was_creates := begin match op with
    | Creates _ -> true | _ -> false end;
    let time = !now in
    Event.record {time; op} in

  let current_id = Lwt.current_id in

  let note_created child =
(*     if !last_was_creates then now := !now +. 0.1; *)
    Creates (current_id (), child) |> record in

  let note_read input =
    Reads (current_id (), input) |> record;
    now := !now +. 0.1 in

  let note_resolved p =
    Resolves (current_id (), p) |> record in

  let note_becomes input main =
    if main <> input then
      Becomes (input, main) |> record in

  Lwt.tracer := { Lwt.
    note_created;
    note_read;
    note_resolved;
    note_becomes;
  }

module Time_map = Map.Make(struct type t = float let compare = compare end)

let events = ref Time_map.empty

let block msg duration fn =
  let t, w = wait () in
  printf "%a is %s\n" Event.fmt (Lwt.id_of_thread t) msg;
  Event.(record {time = !now; op = Label (Lwt.id_of_thread t, msg)});
  Hashtbl.add labels (Lwt.id_of_thread t) msg;
  events := !events |> Time_map.add (!now +. duration) (fun () ->
    Lwt.wakeup w (fn ());
  );
  t

let sleep x =
  block "sleep" x (fun () -> ())

let read_block n =
  block "read_block" 2.0 (fun () -> "block" ^ string_of_int n)

let send_tcp d =
  block "send_tcp" 2.0 (fun () -> printf "Transmit: %s\n" d; ())

let main =
  let a =
    sleep 1.0 >>= fun () ->
    read_block 1 >>= fun r ->
    send_tcp r in
  let b = sleep 3.4 in
  Lwt.join [a; b]

let () =
  while state main = Sleep do
    let time, fn = Time_map.min_binding !events in
    printf "main: event at t=%.1f\n" time;
    now := time;
    events := !events |> Time_map.remove time;
    fn ()
  done;
  Event.(record {time = !now; op = Reads (toplevel, id_of_thread main)});
  Event.(record {time = !now; op = Resolves (toplevel, toplevel)});

  let ch = open_out "log.sexp" in
  List.rev !Event.event_log |> List.iter (fun ev ->
    Sexplib.Sexp.output_mach ch (Event.sexp_of_t ev : Sexplib.Sexp.t)
  );
  close_out ch
