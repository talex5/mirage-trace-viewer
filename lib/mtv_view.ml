(* Copyright (C) 2014, Thomas Leonard *)

(** Some values used for calculating vertical positions.
 * Saved so we don't have to regenerate them for every thread. *)
type v_projection = {
  focal_y : float;                  (* The y position in thread coordinates which is most expanded. *)
  v_scale : float;                  (* To make the distance between threads sensible. *)
  unstretched_top_proj : float;     (* Where the top would be if we didn't stretch the result. *)
  unstretched_range : float;        (* Distance beween top and bottom. *)
}

module ThreadSet = Set.Make(Mtv_thread)

type t = {
  vat : Mtv_thread.vat;
  mutable scale : float;
  mutable view_width : float;
  mutable view_height : float;
  mutable view_start_time : float;
  mutable v_projection : v_projection;
  height : float;
  mutable grid_step : float;
  layout : Mtv_layout.t;
  arrow_events_by_first : (Mtv_thread.t * Mtv_thread.time * Mtv_thread.interaction * Mtv_thread.t * Mtv_thread.time) array;
  arrow_events_by_second : (Mtv_thread.t * Mtv_thread.time * Mtv_thread.interaction * Mtv_thread.t * Mtv_thread.time) array;
  mutable highlights : ThreadSet.t;
}

let clone t = { t with
    v_projection = { t.v_projection with v_scale = t.v_projection.v_scale }
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
  top |> Mtv_thread.iter (fun thread ->
    let interactions = Mtv_thread.interactions thread
      |> List.map (fun (time, op, other) ->
        match op with
        | Mtv_thread.Read | Mtv_thread.Try_read ->
            let end_time = min time (Mtv_thread.end_time other) in
            (thread, time, op, other, end_time)
        | Mtv_thread.Signal ->
            (thread, time, op, other, time)
        | Mtv_thread.Resolve ->
            let start_time = min time (Mtv_thread.end_time thread) in
            (thread, start_time, op, other, time)
      ) in
    events := interactions @ !events
  );
  let by_first = Array.of_list !events in
  let by_second = Array.copy by_first in
  Array.sort by_first_time by_first;
  Array.sort by_second_time by_second;
  (by_first, by_second)

let v_projection_for_focus ~height ~view_height focal_y =
  (* Hack: because we scale so that y=0 is always at the top and y=h is always
   * at the bottom, we need to adjust the scale factor to keep the lengths around
   * focal_y constant. I couldn't figure out the correct formula for this. *)
  let v_scale =
    if focal_y < height /. 2. then (
      let d = focal_y /. 500. in
      let top_unscaled = (1. -. exp d) /. (1. +. exp d) in
      view_height *. (0.5 +. (top_unscaled *. 0.25))
    ) else (
      let d = (focal_y -. height) /. 500. in
      let bottom_unscaled = (1. -. exp d) /. (1. +. exp d) in
      view_height *. (0.5 -. (bottom_unscaled *. 0.25))
    ) in

  let hyp_project d =
    let d = (focal_y -. d) /. v_scale in
    (1. -. exp d) /. (1. +. exp d) in

  let top_proj = hyp_project 0.0 in
  let bottom_proj = hyp_project height in {
    focal_y;
    v_scale;
    unstretched_top_proj = top_proj;
    unstretched_range = bottom_proj -. top_proj;
  }

let make ~view_width ~view_height ~vat =
  let top_thread = Mtv_thread.top_thread vat in
  let time_range = Mtv_thread.end_time top_thread -. Mtv_thread.start_time top_thread in
  let scale = (view_width -. h_margin *. 2.) /. time_range in
  let (arrow_events_by_first, arrow_events_by_second) = collect_events top_thread in
  let layout, height = Mtv_layout.arrange top_thread in {
    vat;
    scale;
    view_width;
    view_height;
    view_start_time = Mtv_thread.start_time top_thread -. (h_margin /. scale);
    v_projection = v_projection_for_focus ~height ~view_height 0.0;
    height;
    grid_step = calc_grid_step scale;
    layout;
    arrow_events_by_first;
    arrow_events_by_second;
    highlights = ThreadSet.empty;
  }

let x_of_time v time = (time -. v.view_start_time)  *. v.scale
let time_of_x v x = (x /. v.scale) +. v.view_start_time

let x_of_start v t = x_of_time v (Mtv_thread.start_time t)
let x_of_end v t = x_of_time v (Mtv_thread.end_time t)

let clip_x_of_time v t =
  x_of_time v t
  |> min 1_000_000.
  |> max (-1_000_000.)

let view_y_of_y v y =
  let p = v.v_projection in
  let focal_y = p.focal_y in

  let hyp_project d =
    let d = (focal_y -. d) /. p.v_scale in
    (1. -. exp d) /. (1. +. exp d) in

  let this_proj = hyp_project y in
  let frac = (this_proj -. p.unstretched_top_proj) /. p.unstretched_range in
  v_margin +. (v.view_height -. 2. *. v_margin) *. frac

let y_of_view_y v view_y =
  if view_y <= v_margin then 0.0
  else if view_y >= v.view_height -. v_margin then v.height
  else (
    let p = v.v_projection in
    let focal_y = p.focal_y in
    let frac = (view_y -. v_margin) /. (v.view_height -. 2. *. v_margin) in
    let this_proj = frac *. p.unstretched_range +. p.unstretched_top_proj in
    let d = -. log ((1. +. this_proj) /. (1. -. this_proj)) in
    ~-. (d *. p.v_scale -. focal_y)
  )

let y_of_thread v t = view_y_of_y v (Mtv_thread.y t)

let width_of_timespan v t = t *. v.scale
let timespan_of_width v w = w /. v.scale

let set_scale v scale =
  let top_thread = Mtv_thread.top_thread v.vat in
  let time_range = Mtv_thread.end_time top_thread -. Mtv_thread.start_time top_thread in
  let min_scale = (v.view_width -. h_margin *. 2.) /. time_range in
  v.scale <- max min_scale scale;
  v.grid_step <- calc_grid_step scale

let zoom v factor =
  set_scale v (v.scale *. factor)

let scroll_bounds v =
  let top_thread = Mtv_thread.top_thread v.vat in
  let width = width_of_timespan v (Mtv_thread.end_time top_thread -. Mtv_thread.start_time top_thread) in
  (
    (-. h_margin, width +. h_margin, v.view_width, (v.view_start_time -. Mtv_thread.start_time top_thread) *. v.scale),
    (-. v_margin, v.height +. v_margin +. v.view_height, v.view_height, v.v_projection.focal_y)
  )

let visible_threads v visible_time_range =
  Mtv_layout.IT.overlapping_interval v.layout visible_time_range

let set_start_time v t =
  let top_thread = Mtv_thread.top_thread v.vat in
  let trace_start_time = Mtv_thread.start_time top_thread in
  let trace_end_time = Mtv_thread.end_time top_thread in
  v.view_start_time <- t
    |> min (trace_end_time -. ((v.view_width -. h_margin) /. v.scale))
    |> max (trace_start_time -. (h_margin /. v.scale));
  (v.view_start_time -. trace_start_time) *. v.scale

let set_size v width height =
  let scale_factor = width /. v.view_width in
  v.view_width <- width;
  v.view_height <- height;
  v.v_projection <- v_projection_for_focus ~height:v.height ~view_height:v.view_height v.v_projection.focal_y;
  set_scale v (v.scale *. scale_factor);
  set_start_time v v.view_start_time |> ignore

let set_view_y v y =
  let focal_y = y
    |> min (v.height +. v_margin)
    |> max (-. v_margin) in
  v.v_projection <- v_projection_for_focus ~height:v.height ~view_height:v.view_height focal_y;
  focal_y

let set_view_y_so v y view_y =
  (* Binary search because I didn't pay attention in maths class. *)
  let rec aux lo high i =
    if i = 0 then ()
    else (
      let f = (lo +. high) /. 2. in
      v.v_projection <- v_projection_for_focus ~height:v.height ~view_height:v.view_height f;
      let this_view_y = view_y_of_y v y in
      let d = this_view_y -. view_y in
      if d < 1. then aux lo f (i - 1)
      else if d > -1.0 then aux f high (i - 1)
      else ()
    ) in
  if view_y > v_margin && view_y < v.view_height -. v_margin then (
    aux 0.0 v.height 20
  );
  v.v_projection.focal_y

let iter_interactions v t1 t2 f =
  Mtv_sorted_array.iter_range v.arrow_events_by_first
    (fun (_, t, _, _, _) -> t >= t1)
    (fun (_, t, _, _, _) -> t < t2)
    f;
  Mtv_sorted_array.iter_range v.arrow_events_by_second
    (fun (_, _, _, _, t) -> t >= t1)
    (fun (_, _, _, _, t) -> t < t2)
    (fun i ->
      let (_, st, _, _, _) = i in
      if st < t1 || st >= t2 then f i
      (* else we already processed this one above *)
    )

let dist_from_focus v t= Mtv_thread.y t -. v.v_projection.focal_y

let vat t = t.vat
let view_start_time t = t.view_start_time
let view_width t = t.view_width
let view_height t = t.view_height
let grid_step t = t.grid_step

let thread_at v ~x ~y =
  let range = 16.0 in (* How far you can be from the thread *)
  let best = ref None in
  visible_threads v (time_of_x v (x -. range), time_of_x v (x +. range))
  |> Mtv_layout.IT.IntervalSet.iter (fun i ->
    let thread = i.Interval_tree.Interval.value in
    let dist_y = abs_float (y_of_thread v thread -. y) in
    let start_x = x_of_time v (Mtv_thread.start_time thread) in
    let end_x = x_of_time v (Mtv_thread.end_time thread) in
    let dist_x =
      if x < start_x then start_x -. x
      else if x > end_x then x -. end_x
      else 0.0 in
    let dist = max dist_x dist_y in
    if dist <= range then (
      match !best with
      | Some (best_dist, _) when best_dist <= dist -> ()
      | _ -> best := Some (dist, thread)
    )
  );
  match !best with
  | None -> None
  | Some (_, thread) -> Some thread

let highlights t = t.highlights
let set_highlights t v = t.highlights <- v

let highlight_related v thread =
  let highlights = ref (ThreadSet.singleton thread) in
  let rec walk_successors th =
    match Mtv_thread.becomes th with
    | Some th ->
        highlights := !highlights |> ThreadSet.add th;
        walk_successors th
    | None -> () in
  walk_successors thread;
  let pred = Hashtbl.create 1000 in
  Mtv_thread.top_thread v.vat |> Mtv_thread.iter (fun th ->
    match Mtv_thread.becomes th with
    | None -> ()
    | Some s -> Hashtbl.add pred s th
  );
  let rec walk_preds th =
    let preds = Hashtbl.find_all pred th in
    preds |> List.iter (fun th ->
      highlights := !highlights |> ThreadSet.add th;
      walk_preds th
    ) in
  walk_preds thread;
  set_highlights v !highlights

let highlight_matches v query =
  let highlights = ref ThreadSet.empty in
  Mtv_thread.top_thread v.vat |> Mtv_thread.iter (fun th ->
    if Mtv_thread.labels th |> List.exists (fun (_time, label) -> query label) then
      highlights := !highlights |> ThreadSet.add th;
  );
  set_highlights v !highlights
