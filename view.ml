(* Copyright (C) 2014, Thomas Leonard *)

type t = {
  top_thread : Thread.t;
  mutable scale : float;
  mutable view_width : float;
  mutable view_height : float;
  mutable view_start_time : float;
  mutable view_start_y : float;
  height : float;
  mutable grid_step : float;
  layout : Layout.t;
}

let margin = 20.

let calc_grid_step scale =
  let l = 2.5 -. (log scale /. log 10.) |> floor in
  10. ** l

let make ~view_width ~view_height ~top_thread =
  let time_range = Thread.end_time top_thread -. Thread.start_time top_thread in
  let scale = (view_width -. margin *. 2.) /. time_range in
  let layout, height = Layout.arrange top_thread in {
    top_thread;
    scale;
    view_width;
    view_height;
    view_start_time = Thread.start_time top_thread -. (margin /. scale);
    view_start_y = -.margin;
    height;
    grid_step = calc_grid_step scale;
    layout;
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
  let time_range = Thread.end_time v.top_thread -. Thread.start_time v.top_thread in
  let min_scale = (v.view_width -. margin *. 2.) /. time_range in
  v.scale <- max min_scale scale;
  v.grid_step <- calc_grid_step scale

let scroll_bounds v =
  let width = width_of_timespan v (Thread.end_time v.top_thread -. Thread.start_time v.top_thread) in
  (
    (-. margin, width +. margin, v.view_width),
    (-. margin, v.height +. margin, v.view_height)
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
  let trace_start_time = Thread.start_time v.top_thread in
  let trace_end_time = Thread.end_time v.top_thread in
  v.view_start_time <- t
    |> min (trace_end_time -. ((v.view_width -. margin) /. v.scale))
    |> max (trace_start_time -. (margin /. v.scale));
  (v.view_start_time -. trace_start_time) *. v.scale

let set_view_y v y =
  v.view_start_y <- y
    |> min (v.height +. margin -. v.view_height)
    |> max (-. margin);
  v.view_start_y
