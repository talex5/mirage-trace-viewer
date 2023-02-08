
open Mirage_trace_viewer

module Ctf = Eio.Private.Ctf

let current_thread = ref (-1)

let to_time timestamp =
  let t= Runtime_events.Timestamp.to_int64 timestamp in
  Int64.to_float t /. 1_000_000_000.

let id_event_callback ~add _domain_id timestamp user_event ((id : Ctf.id), value) =
  match Runtime_events.User.tag user_event, value with
  | Ctf.Created, e ->
    add timestamp (Mtv_event.Creates (!current_thread, (id :> int), Ctf.event_to_string e))
  | _ -> ()

let id_callback ~add _domain_id timestamp user_event id =
  match Runtime_events.User.tag user_event with
  | Ctf.Switch ->
    current_thread := (id :> int);
    add timestamp (Mtv_event.Switch (id :> int) )
  | Ctf.Resolved ->
    add timestamp (Mtv_event.Resolves (!current_thread, (id :> int), None) )
  | _ -> ()

let unit_callback ~add _domain_id timestamp user_event () =
  match Runtime_events.User.tag user_event with
  | Ctf.Suspend ->
    current_thread := -1;
    add timestamp (Mtv_event.Switch (-1) )
  | _ -> ()

let id_label_callback ~add _domain_id timestamp user_event ((id : Ctf.id), label) =
  match Runtime_events.User.tag user_event with
  | Ctf.Label ->
    add timestamp (Mtv_event.Label ((id :> int), label) )
  | Ctf.Failed ->
    add timestamp (Mtv_event.Resolves (!current_thread, (id :> int), Some label) )
  | Ctf.Increase ->
    add timestamp (Mtv_event.Increases (!current_thread, label, (id :> int)) )
  | Ctf.Value ->
    add timestamp (Mtv_event.Counter_value (!current_thread, label, (id :> int)) )
  | _ -> ()

let two_ids_callback ~add _domain_id timestamp user_event ((id1 : Ctf.id), (id2 : Ctf.id)) =
  match Runtime_events.User.tag user_event with
  | Ctf.Signal ->
    add timestamp (Mtv_event.Signals (((id1 :> int), (id2 :> int))))
  | _ -> ()

let gc_begin = ref 0L

let runtime_begin _ t phase =
  match phase with
  | Runtime_events.EV_MINOR | Runtime_events.EV_MAJOR ->
    gc_begin := (Runtime_events.Timestamp.to_int64 t)
  | _ -> ()

let runtime_end ~add _ t phase =
  match phase with
  | Runtime_events.EV_MINOR | Runtime_events.EV_MAJOR ->
    let ev = if phase = EV_MINOR then Mtv_event.Minor else Major in
    let duration = Int64.sub (Runtime_events.Timestamp.to_int64 t) !gc_begin in
    add t (Mtv_event.Gc (Int64.to_float duration /. 1_000_000_000., ev))
  | _ -> ()

let load_runtime_events path =
  let dir = Filename.dirname path in
  let file = Filename.basename path in
  let pid = String.split_on_char '.' file |> List.hd |> int_of_string in
  let cursor = Runtime_events.create_cursor (Some (dir, pid)) in

  let events = ref [] in
  let add timestamp op =
    events := { Mtv_event.time = to_time timestamp; op} :: !events
  in
  let callback =
    let open Runtime_events.Callbacks in
    create
      ~runtime_begin
      ~runtime_end:(runtime_end ~add) ()
    |> add_user_event Ctf.created_type (id_event_callback ~add)
    |> add_user_event Runtime_events.Type.int (id_callback ~add)
    |> add_user_event Runtime_events.Type.unit (unit_callback ~add)
    |> add_user_event Ctf.labelled_type (id_label_callback ~add)
    |> add_user_event Ctf.two_ids_type (two_ids_callback ~add)
  in
  while Runtime_events.read_poll cursor callback (None) > 0 do
    ()
  done;
  List.rev !events
