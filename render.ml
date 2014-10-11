(* Copyright (C) 2014, Thomas Leonard *)

let arrow_width = 4.
let arrow_height = 10.

let thin cr = Cairo.set_line_width cr 1.0

let thread_label cr =
  Cairo.set_source_rgb cr ~r:0.8 ~g:0.2 ~b:0.2

let anonymous_thread cr =
  Cairo.set_line_width cr 2.0;
  Cairo.set_source_rgb cr ~r:0.6 ~g:0.6 ~b:0.6

let named_thread cr =
  Cairo.set_line_width cr 2.0;
  Cairo.set_source_rgb cr ~r:0.2 ~g:0.2 ~b:0.2

let failed cr =
  Cairo.set_line_width cr 2.0;
  Cairo.set_source_rgb cr ~r:0.8 ~g:0.0 ~b:0.0

let activation cr =
  Cairo.set_line_width cr 2.0;
  Cairo.set_source_rgb cr ~r:1.0 ~g:1.0 ~b:1.0

let line v cr time src recv colour =
  colour cr;
  Cairo.move_to cr ~x:(View.x_of_time v time) ~y:(View.y_of_thread v src);
  Cairo.line_to cr ~x:(View.x_of_time v time) ~y:(View.y_of_thread v recv);
  Cairo.stroke cr

let arrow v cr src src_time recv recv_time (r, g, b) =
  let width = View.width_of_timespan v (recv_time -. src_time) in
  let alpha = 1.0 -. (min 1.0 (width /. 6000.)) in
  if alpha > 0.01 then (
    Cairo.set_source_rgba cr ~r ~g ~b ~a:alpha;

    if Thread.id src <> -1  && Thread.id src <> Thread.id recv then (
      let src_x = View.clip_x_of_time v src_time in
      let src_y = View.y_of_thread v src in
      let recv_y = View.y_of_thread v recv in

      Cairo.move_to cr ~x:src_x ~y:src_y;
      let arrow_head_y =
        if src_y < recv_y then recv_y -. arrow_height
        else recv_y +. arrow_height in
      let x = View.clip_x_of_time v recv_time in
      Cairo.line_to cr ~x ~y:arrow_head_y;
      Cairo.line_to cr ~x:(x +. arrow_width) ~y:arrow_head_y;
      Cairo.line_to cr ~x ~y:recv_y;
      Cairo.line_to cr ~x:(x -. arrow_width) ~y:arrow_head_y;
      Cairo.line_to cr ~x ~y:arrow_head_y;
      Cairo.stroke_preserve cr;
      Cairo.fill cr
    )
  )

let draw_grid v cr area_start_x area_end_x =
  Cairo.set_line_width cr 1.0;
  Cairo.set_source_rgb cr ~r:0.8 ~g:0.8 ~b:0.8;

  let grid_step = v.View.grid_step in
  let top = -. View.margin in
  let bottom = View.(v.view_height +. margin) in

  let area_start_time = View.time_of_x v area_start_x in
  let grid_start_x = floor (area_start_time /. grid_step) *. grid_step |> View.x_of_time v in
  let grid_step_x = View.width_of_timespan v grid_step in
  let rec draw x =
    if x < area_end_x then (
      Cairo.move_to cr ~x:x ~y:top;
      Cairo.line_to cr ~x:x ~y:bottom;
      Cairo.stroke cr;
      draw (x +. grid_step_x)
    ) in
  draw grid_start_x;
  Cairo.set_source_rgb cr ~r:0.4 ~g:0.4 ~b:0.4;
  let msg =
    if grid_step >= 1.0 then Printf.sprintf "Each grid division: %.f s" grid_step
    else if grid_step >= 0.001 then Printf.sprintf "Each grid division: %.f ms" (grid_step *. 1000.)
    else if grid_step >= 0.000_001 then Printf.sprintf "Each grid division: %.f us" (grid_step *. 1_000_000.)
    else if grid_step >= 0.000_000_001 then Printf.sprintf "Each grid division: %.f ns" (grid_step *. 1_000_000_000.)
    else Printf.sprintf "Each grid division: %.2g s" grid_step in
  let extents = Cairo.text_extents cr msg in
  let y = bottom +. Cairo.(extents.y_bearing -. extents.height) -. 2.0 in
  Cairo.move_to cr ~x:4.0 ~y;
  Cairo.show_text cr msg

let render v cr ~expose_area =
  let top_thread = v.View.top_thread in
  let ((expose_min_x, expose_min_y), (expose_max_x, expose_max_y)) = expose_area in

  Cairo.set_source_rgb cr ~r:0.9 ~g:0.9 ~b:0.9;
  Cairo.paint cr;

  (* When the system thread is "active", the system is idle. *)
  Cairo.set_source_rgb cr ~r:0.8 ~g:0.8 ~b:0.8;
  Thread.activations top_thread |> List.iter (fun (start_time, end_time) ->
    let start_x = View.clip_x_of_time v start_time in
    let end_x = View.clip_x_of_time v end_time in
    Cairo.rectangle cr ~x:start_x ~y:expose_min_y ~w:(end_x -. start_x) ~h:expose_max_y;
    Cairo.fill cr;
  );

  Cairo.set_font_size cr 12.;
  Cairo.select_font_face cr "Sans";

  draw_grid v cr expose_min_x expose_max_x;

  Cairo.set_line_width cr 2.0;
  Cairo.set_source_rgb cr ~r:1. ~g:1. ~b:1.;
  Cairo.set_line_join cr Cairo.JOIN_BEVEL;

  let visible_t_min = View.time_of_x v expose_min_x in
  let visible_t_max = View.time_of_x v expose_max_x in
  let visible_threads = View.visible_threads v (visible_t_min, visible_t_max) in
  visible_threads |> Layout.IT.IntervalSet.iter (fun i ->
    let t = i.Interval_tree.Interval.value in
    let y = View.y_of_thread v t in
    if Thread.label t <> None then
      named_thread cr
    else
      anonymous_thread cr;
    Cairo.move_to cr ~x:(max expose_min_x (View.x_of_start v t)) ~y;
    Cairo.line_to cr ~x:(min expose_max_x (View.x_of_end v t)) ~y;
    Cairo.stroke cr;
    Thread.creates t |> List.iter (fun child ->
      line v cr (Thread.start_time child) t child anonymous_thread
    );
    begin match Thread.becomes t with
    | None -> ()
    | Some child ->
        line v cr (Thread.end_time t) t child anonymous_thread end;
    activation cr;
    Thread.activations t |> List.iter (fun (start_time, end_time) ->
      Cairo.move_to cr ~x:(max expose_min_x (View.clip_x_of_time v start_time)) ~y;
      Cairo.line_to cr ~x:(min expose_max_x (View.clip_x_of_time v end_time)) ~y;
      Cairo.stroke cr;
    );
    if Thread.failure t <> None then (
      failed cr;
      let x = View.clip_x_of_time v (Thread.end_time t) in
      Cairo.move_to cr ~x ~y:(y -. 8.);
      Cairo.line_to cr ~x ~y:(y +. 8.);
      Cairo.stroke cr;
    )
  );

  top_thread |> Thread.iter (fun t ->
    Thread.interactions t |> List.iter (fun (time, op, other) ->
      match op with
      | Thread.Read ->
          let end_time = min time (Thread.end_time other) in
          thin cr;
          let colour =
            if Thread.failure other <> None then (0.8, 0.0, 0.0)
            else (0.0, 0.0, 1.0) in
          arrow v cr other end_time t time colour
      | Thread.Resolve ->
          if Thread.id t <> -1 then (
            let start_time = time
              |> min (Thread.end_time t) in
            arrow v cr t start_time other time (0.0, 0.5, 0.0)
          )
    )
  );

  visible_threads |> Layout.IT.IntervalSet.iter (fun i ->
    let t = i.Interval_tree.Interval.value in
    let start_x = View.x_of_start v t +. 2. in
    let end_x = View.x_of_end v t in
    let y = View.y_of_thread v t in
    let thread_width = end_x -. start_x in
    if thread_width > 16. then (
      let msg =
        match Thread.label t with
        | None -> string_of_int (Thread.id t)
        | Some label -> label in
      let msg =
        match Thread.failure t with
        | None -> msg
        | Some failure -> msg ^ " ->  " ^ failure in
      thread_label cr;

      let text_width = Cairo.((text_extents cr msg).x_advance) in
      if text_width > thread_width then (
        let x = start_x in
        Cairo.save cr;
        Cairo.rectangle cr ~x ~y:0.0 ~w:(end_x -. x) ~h:v.View.height;
        Cairo.clip cr;
        Cairo.move_to cr ~x ~y:(y -. 3.);
        Cairo.show_text cr msg;
        Cairo.restore cr;
      ) else (
        (* Show label on left margin if the thread starts off-screen *)
        let x =
          if start_x < 4.0 then min 4.0 (end_x -. text_width)
          else start_x in
        Cairo.move_to cr ~x ~y:(y -. 3.);
        Cairo.show_text cr msg;
      );
    )
  )
