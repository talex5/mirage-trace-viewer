(* Copyright (C) 2014, Thomas Leonard *)

type interaction = Resolve | Read

type time = float

type t = {
  tid : int;
  start_time : time;
  mutable end_time : time;
  mutable creates : t list;
  mutable becomes : t option;
  mutable label : string option;
  mutable interactions : (time * interaction * t) list;
  mutable activations : (time * time) list;
  mutable failed : bool;
  mutable y : float;
}

let make_thread ~tid ~start_time = {
  tid;
  start_time;
  end_time = infinity;
  creates = [];
  becomes = None;
  label = None;
  interactions = [];
  activations = [];
  failed = false;
  y = 0.0;
}

let rec iter fn thread =
  fn thread;
  thread.creates |> List.iter (iter fn)

let from_channel ch =
  let events = Sexplib.Sexp.input_sexps ch in
  let start_time =
    match events with
    | [] -> failwith "No events!"
    | hd :: _ -> Event.((t_of_sexp hd).time) in
  let top_thread = make_thread ~start_time ~tid:(-1) in

  let rec replacement thread =
    match thread.becomes with
    | None -> thread
    | Some t2 -> replacement t2 in

  let threads = Hashtbl.create 100 in
  let get_thread id =
    try Hashtbl.find threads id |> replacement
    with Not_found -> top_thread in

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
    | Creates (a, b) ->
        let a = get_thread a in
        assert (not (Hashtbl.mem threads b));
        let child = make_thread ~start_time:ev.time ~tid:b in
        Hashtbl.add threads b child;
        a.creates <- child :: a.creates
    | Resolves (a, b, success) ->
        let a = get_thread a in
        let b = get_thread b in
        a.interactions <- (ev.time, Resolve, b) :: a.interactions;
        b.failed <- not success;
        b.end_time <- ev.time
    | Becomes (a, b) ->
        let a = get_thread a in
        a.end_time <- ev.time;
        assert (a.becomes = None);
        a.becomes <- Some (get_thread b);
    | Reads (a, b) ->
        let a = get_thread a in
        let b = get_thread b in
        switch ev.time (Some a);
        a.interactions <- (ev.time, Read, b) :: a.interactions;
    | Label (a, msg) ->
        if a <> -1 then (get_thread a).label <- Some msg
    | Switch a ->
        switch ev.time (Some (get_thread a))
  );
  switch top_thread.end_time None;
  top_thread |> iter (fun t ->
    if t.end_time = infinity then t.end_time <- top_thread.end_time;
  );
  top_thread

let start_time t = t.start_time
let end_time t = t.end_time
let creates t = t.creates
let becomes t = t.becomes
let label t = t.label
let interactions t = t.interactions
let activations t = t.activations
let failed t = t.failed
let y t = t.y
let id t = t.tid

let set_y t y = t.y <- y

(** Sorts by y first, then by thread ID *)
let compare a b =
  match compare a.y b.y with
  | 0 -> compare a.tid b.tid
  | r -> r
