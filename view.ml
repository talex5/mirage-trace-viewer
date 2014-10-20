(* Copyright (C) 2014, Thomas Leonard *)

type t = {
  vat : Thread.vat;
  mutable scale : float;
  mutable view_width : float;
  mutable view_height : float;
  mutable view_start_time : float;
  mutable view_start_y : float;
  height : float;
  mutable grid_step : float;
  layout : Layout.t;
  arrow_events_by_first : (Thread.t * Thread.time * Thread.interaction * Thread.t * Thread.time) array;
  arrow_events_by_second : (Thread.t * Thread.time * Thread.interaction * Thread.t * Thread.time) array;
}

let h_margin = 20.
let v_margin = 30.

let calc_grid_step scale =
  let l = 2.5 -. (log scale /. log 10.) |> floor in
  10. ** l

let by_first_time (_, (t1:float), _, _, _) (_, (t2:float), _, _, _) = compare t1 t2
let by_second_time (_, _, _, _, (t1:float)) (_, _, _, _, (t2:float)) = compare t1 t2

let collect_events top =
  let events = ref [] in
  top |> Thread.iter (fun thread ->
    let interactions = Thread.interactions thread
      |> List.map (fun (time, op, other) ->
        match op with
        | Thread.Read ->
            let end_time = min time (Thread.end_time other) in
            (thread, time, op, other, end_time)
        | Thread.Resolve ->
            let start_time = min time (Thread.end_time thread) in
            (thread, time, op, other, start_time)
      ) in
    events := interactions @ !events
  );
  let by_first = Array.of_list !events in
  let by_second = Array.copy by_first in
  Array.sort by_first_time by_first;
  Array.sort by_second_time by_second;
  (by_first, by_second)

let make ~view_width ~view_height ~vat =
  let top_thread = Thread.top_thread vat in
  let time_range = Thread.end_time top_thread -. Thread.start_time top_thread in
  let scale = (view_width -. h_margin *. 2.) /. time_range in
  let (arrow_events_by_first, arrow_events_by_second) = collect_events top_thread in
  let layout, height = Layout.arrange top_thread in {
    vat;
    scale;
    view_width;
    view_height;
    view_start_time = Thread.start_time top_thread -. (h_margin /. scale);
    view_start_y = -.v_margin;
    height;
    grid_step = calc_grid_step scale;
    layout;
    arrow_events_by_first;
    arrow_events_by_second;
  }

let x_of_time v time = (time -. v.view_start_time)  *. v.scale
let time_of_x v x = (x /. v.scale) +. v.view_start_time

let x_of_start v t = x_of_time v (Thread.start_time t)
let x_of_end v t = x_of_time v (Thread.end_time t)

let clip_x_of_time v t =
  x_of_time v t
  |> min 1_000_000.
  |> max (-1_000_000.)

let y_of_thread v t = Thread.y t -. v.view_start_y

let width_of_timespan v t = t *. v.scale
let timespan_of_width v w = w /. v.scale

let set_scale v scale =
  let top_thread = Thread.top_thread v.vat in
  let time_range = Thread.end_time top_thread -. Thread.start_time top_thread in
  let min_scale = (v.view_width -. h_margin *. 2.) /. time_range in
  v.scale <- max min_scale scale;
  v.grid_step <- calc_grid_step scale

let scroll_bounds v =
  let top_thread = Thread.top_thread v.vat in
  let width = width_of_timespan v (Thread.end_time top_thread -. Thread.start_time top_thread) in
  (
    (-. h_margin, width +. h_margin, v.view_width, (v.view_start_time -. Thread.start_time top_thread) *. v.scale),
    (-. v_margin, v.height +. v_margin, v.view_height, v.view_start_y)
  )

let visible_threads v visible_time_range =
  Layout.IT.overlapping_interval v.layout visible_time_range

let set_size v width height =
  let scale_factor = width /. v.view_width in
  v.view_width <- width;
  v.view_height <- height;
  set_scale v (v.scale *. scale_factor)

(** Set [view_start_time], within the allowed limits.
 * Returns the new horizontol scrollbar position. *)
let set_start_time v t =
  let top_thread = Thread.top_thread v.vat in
  let trace_start_time = Thread.start_time top_thread in
  let trace_end_time = Thread.end_time top_thread in
  v.view_start_time <- t
    |> min (trace_end_time -. ((v.view_width -. h_margin) /. v.scale))
    |> max (trace_start_time -. (h_margin /. v.scale));
  (v.view_start_time -. trace_start_time) *. v.scale

let set_view_y v y =
  v.view_start_y <- y
    |> min (v.height +. v_margin -. v.view_height)
    |> max (-. v_margin);
  v.view_start_y

let iter_interactions v t1 t2 f =
  Sorted_array.iter_range v.arrow_events_by_first
    (fun (_, t, _, _, _) -> t >= t1)
    (fun (_, t, _, _, _) -> t < t2)
    f;
  Sorted_array.iter_range v.arrow_events_by_second
    (fun (_, _, _, _, t) -> t >= t1)
    (fun (_, _, _, _, t) -> t < t2)
    (fun i ->
      let (_, st, _, _, _) = i in
      if st < t1 || st >= t2 then f i
      (* else we already processed this one above *)
    )
