(* Copyright (C) 2014, Thomas Leonard *)

type interaction = Resolve | Read

type time = float

type t = {
  thread_type : string;
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

type mutable_counter = {
  mutable mc_values : (time * int) list;
}

type vat = {
  top_thread : t;
  mutable gc : (time * time) list;
  mutable counters : Counter.t list;
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

let counter_value c =
  match c.mc_values with
  | [] -> 0
  | (_, v) :: _ -> v

let of_sexp events =
  let trace_start_time =
    match events with
    | [] -> failwith "No events!"
    | hd :: _ -> Event.((t_of_sexp hd).time) in
  let top_thread = make_thread ~start_time:0.0 ~tid:(-1) ~thread_type:"Preexisting" in
  top_thread.end_time <- 0.0;

  let vat = {top_thread; gc = []; counters = []} in

  let counters = Hashtbl.create 2 in
  let get_counter name =
    try Hashtbl.find counters name
    with Not_found ->
      let c = { mc_values = [] } in
      Hashtbl.add counters name c;
      c in

  let rec replacement thread =
    match thread.becomes with
    | None -> thread
    | Some t2 -> replacement t2 in

  let threads = Hashtbl.create 100 in
  Hashtbl.add threads (-1) top_thread;
  let get_thread tid =
    try Hashtbl.find threads tid |> replacement
    with Not_found ->
      let t = make_thread ~tid ~start_time:0.0 ~thread_type:"Preexisting" in
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
    let time = ev.time -. trace_start_time in
    if time > top_thread.end_time then top_thread.end_time <- time;

    match ev.op with
    | Creates (a, b, thread_type) ->
        let a = get_thread a in
        assert (not (Hashtbl.mem threads b));
        let child = make_thread ~start_time:time ~tid:b ~thread_type in
        Hashtbl.add threads b child;
        a.creates <- child :: a.creates
    | Resolves (a, b, failure) ->
        let a = get_thread a in
        let b = get_thread b in
        a.interactions <- (time, Resolve, b) :: a.interactions;
        b.failure <- failure;
        b.end_time <- time
    | Becomes (a, b) ->
        let a = get_thread a in
        a.end_time <- time;
        assert (a.becomes = None);
        let b = Some (get_thread b) in
        a.becomes <- b;
        begin match !running_thread with
        | Some (_t, current_thread) when current_thread.tid = a.tid -> switch time b
        | _ -> () end
    | Reads (a, b) ->
        let a = get_thread a in
        let b = get_thread b in
        switch time (Some a);
        a.interactions <- (time, Read, b) :: a.interactions;
    | Label (a, msg) ->
        if a <> -1 then (
          let a = get_thread a in
          a.labels <- (time, msg) :: a.labels
        )
    | Switch a ->
        switch time (Some (get_thread a))
    | Gc duration ->
        vat.gc <- (time -. duration, time) :: vat.gc
    | Increases (a, counter, amount) ->
        let c = get_counter counter in
        let new_value = counter_value c + amount in
        c.mc_values <- (time, new_value) :: c.mc_values;
        let a = get_thread a in
        a.labels <- (time, counter ^ "+" ^ string_of_int amount) :: a.labels
  );
  switch top_thread.end_time None;
  top_thread |> iter (fun t ->
    let labels =
      match t.labels with
      | [] -> [t.start_time, string_of_int t.tid]
      | labels -> labels in
    let labels =
      match t.failure with
      | None -> labels
      | Some failure -> (t.end_time, failure) :: labels in
    t.labels <- List.rev labels
  );
  counters |> Hashtbl.iter (fun name mc ->
    let values = List.rev mc.mc_values |> List.map (fun (t, v) -> (t, float_of_int v)) |> Array.of_list in
    let low = ref 0. in
    let high = ref 0. in
    values |> Array.iter (fun (_, v) ->
      low := min !low v;
      high := max !high v;
    );
    let counter = { Counter.
      name;
      values;
      min = !low;
      max = !high;
    } in
    vat.counters <- counter :: vat.counters
  );
  vat

let top_thread v = v.top_thread
let gc_periods v = v.gc

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
  try
    Sexplib.Sexp.input_sexps ch |> of_sexp
  with Sexplib.Pre_sexp.Of_sexp_error (ex, t) ->
    failwith (Printf.sprintf "Error parsing '%s': %s" (Sexplib.Std.string_of_sexp t) (Printexc.to_string ex))

let counters vat = vat.counters
