(* Copyright (C) 2014, Thomas Leonard *)

type interaction = Resolve | Read

type time = float

type t = {
  thread_type : Event.thread_type;
  tid : int;
  start_time : time;
  mutable end_time : time;
  mutable creates : t list;
  mutable becomes : t option;
  mutable labels : (time * string) list;
  mutable interactions : (time * interaction * t) list;
  mutable activations : (time * time) list;
  mutable failure : string option;
  mutable y : float;
}

let make_thread ~tid ~start_time ~thread_type = {
  thread_type;
  tid;
  start_time;
  end_time = infinity;
  creates = [];
  becomes = None;
  labels = [];
  interactions = [];
  activations = [];
  failure = None;
  y = -.infinity;
}

let rec iter fn thread =
  fn thread;
  thread.creates |> List.iter (iter fn)

let of_sexp events =
  let start_time =
    match events with
    | [] -> failwith "No events!"
    | hd :: _ -> Event.((t_of_sexp hd).time) in
  let top_thread = make_thread ~start_time ~tid:(-1) ~thread_type:Event.Preexisting in
  top_thread.end_time <- 0.0;

  let rec replacement thread =
    match thread.becomes with
    | None -> thread
    | Some t2 -> replacement t2 in

  let threads = Hashtbl.create 100 in
  Hashtbl.add threads (-1) top_thread;
  let get_thread tid =
    try Hashtbl.find threads tid |> replacement
    with Not_found ->
      let t = make_thread ~tid ~start_time:0.0 ~thread_type:Event.Preexisting in
      Hashtbl.add threads tid t;
      top_thread.creates <- t :: top_thread.creates;
      t in

  let running_thread = ref None in
  let switch time next =
    match !running_thread, next with
    | Some (_, prev), Some next when prev.tid = next.tid -> ()
    | prev, next ->
        begin match prev with
        | Some (start_time, prev) ->
            let end_time = min time (prev.end_time) in
            prev.activations <- (start_time, end_time) :: prev.activations
        | None -> () end;
        match next with
        | Some next -> running_thread := Some (time, next)
        | None -> running_thread := None in

  events |> List.iter (fun sexp ->
    let open Event in
    let ev = t_of_sexp sexp in
    if ev.time > top_thread.end_time then top_thread.end_time <- ev.time;

    match ev.op with
    | Creates (a, b, thread_type) ->
        let a = get_thread a in
        assert (not (Hashtbl.mem threads b));
        let child = make_thread ~start_time:ev.time ~tid:b ~thread_type in
        Hashtbl.add threads b child;
        a.creates <- child :: a.creates
    | Resolves (a, b, failure) ->
        let a = get_thread a in
        let b = get_thread b in
        a.interactions <- (ev.time, Resolve, b) :: a.interactions;
        b.failure <- failure;
        b.end_time <- ev.time
    | Becomes (a, b) ->
        let a = get_thread a in
        a.end_time <- ev.time;
        assert (a.becomes = None);
        let b = Some (get_thread b) in
        a.becomes <- b;
        begin match !running_thread with
        | Some (_t, current_thread) when current_thread.tid = a.tid -> switch ev.time b
        | _ -> () end
    | Reads (a, b) ->
        let a = get_thread a in
        let b = get_thread b in
        switch ev.time (Some a);
        a.interactions <- (ev.time, Read, b) :: a.interactions;
    | Label (a, msg) ->
        if a <> -1 then (
          let a = get_thread a in
          a.labels <- (ev.time, msg) :: a.labels
        )
    | Switch a ->
        switch ev.time (Some (get_thread a))
  );
  switch top_thread.end_time None;
  top_thread |> iter (fun t ->
    t.labels <- List.rev t.labels
  );
  top_thread

let thread_type t = t.thread_type
let start_time t = t.start_time
let end_time t = t.end_time
let creates t = t.creates
let becomes t = t.becomes
let labels t = t.labels
let interactions t = t.interactions
let activations t = t.activations
let failure t = t.failure
let y t = t.y
let id t = t.tid

let set_y t y = t.y <- y

(** Sorts by y first, then by thread ID *)
let compare a b =
  match compare a.y b.y with
  | 0 -> compare a.tid b.tid
  | r -> r

let from_channel ch =
  Sexplib.Sexp.input_sexps ch |> of_sexp
