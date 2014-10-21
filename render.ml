(* Copyright (C) 2014, Thomas Leonard *)

module type CANVAS = sig
  type context
  type text_extents = {
    x_bearing : float; 
    y_bearing : float;
    width : float;
    height : float;
    x_advance : float;
    y_advance : float;
  }

  val set_line_width : context -> float -> unit
  val set_source_rgb : context -> r:float -> g:float -> b:float -> unit
  val set_source_rgba : context -> r:float -> g:float -> b:float -> a:float -> unit
  (* (Cairo needs to know the r,g,b too) *)
  val set_source_alpha : context -> r:float -> g:float -> b:float -> float -> unit
  val move_to : context -> x:float -> y:float -> unit
  val line_to : context -> x:float -> y:float -> unit
  val rectangle : context -> x:float -> y:float -> w:float -> h:float -> unit
  val stroke : context -> unit
  val stroke_preserve : context -> unit
  val fill : context -> unit
  val text_extents : context -> string -> text_extents
  val paint_text : context -> ?clip_area:(float * float) -> x:float -> y:float -> string -> unit
  val paint : ?alpha:float -> context -> unit
end

module Make (C : CANVAS) = struct
  let arrow_width = 4.
  let arrow_height = 10.

  let thin cr = C.set_line_width cr 1.0

  let thread_label cr =
    C.set_source_rgb cr ~r:0.0 ~g:0.0 ~b:0.0

  let type_label cr =
    C.set_source_rgb cr ~r:0.5 ~g:0.5 ~b:0.5

  let counter_line cr =
    C.set_source_rgb cr ~r:1.0 ~g:0.0 ~b:0.0

  let anonymous_thread cr =
    C.set_line_width cr 2.0;
    C.set_source_rgb cr ~r:0.6 ~g:0.6 ~b:0.6

  let named_thread cr =
    C.set_line_width cr 2.0;
    C.set_source_rgb cr ~r:0.2 ~g:0.2 ~b:0.2

  let failed cr =
    C.set_line_width cr 2.0;
    C.set_source_rgb cr ~r:0.8 ~g:0.0 ~b:0.0

  let activation cr =
    C.set_line_width cr 3.0;
    C.set_source_rgb cr ~r:1.0 ~g:1.0 ~b:1.0

  let line v cr time src recv =
    C.move_to cr ~x:(View.x_of_time v time) ~y:(View.y_of_thread v src);
    C.line_to cr ~x:(View.x_of_time v time) ~y:(View.y_of_thread v recv);
    C.stroke cr

  let arrow v cr src src_time recv recv_time (r, g, b) =
    let width = View.width_of_timespan v (recv_time -. src_time) in
    let alpha = 1.0 -. (min 1.0 (width /. 6000.)) in
    if alpha > 0.01 then (
      C.set_source_alpha cr ~r ~g ~b alpha;

      if Thread.id src <> -1  && Thread.id src <> Thread.id recv then (
        let src_x = View.clip_x_of_time v src_time in
        let src_y = View.y_of_thread v src in
        let recv_y = View.y_of_thread v recv in

        C.move_to cr ~x:src_x ~y:src_y;
        let arrow_head_y =
          if src_y < recv_y then recv_y -. arrow_height
          else recv_y +. arrow_height in
        let x = View.clip_x_of_time v recv_time in
        C.line_to cr ~x ~y:arrow_head_y;
        C.stroke cr;

        C.move_to cr ~x ~y:arrow_head_y;
        C.line_to cr ~x:(x +. arrow_width) ~y:arrow_head_y;
        C.line_to cr ~x ~y:recv_y;
        C.line_to cr ~x:(x -. arrow_width) ~y:arrow_head_y;
        C.line_to cr ~x ~y:arrow_head_y;
        C.fill cr
      )
    )

  let draw_grid v cr area_start_x area_end_x =
    C.set_line_width cr 1.0;
    C.set_source_rgb cr ~r:0.7 ~g:0.7 ~b:0.7;

    let grid_step = v.View.grid_step in
    let top = -. View.v_margin in
    let bottom = v.View.view_height in

    let area_start_time = View.time_of_x v area_start_x in
    let grid_start_x = floor (area_start_time /. grid_step) *. grid_step |> View.x_of_time v in
    let grid_step_x = View.width_of_timespan v grid_step in
    let rec draw x =
      if x < area_end_x then (
        C.move_to cr ~x:x ~y:top;
        C.line_to cr ~x:x ~y:bottom;
        C.stroke cr;
        draw (x +. grid_step_x)
      ) in
    draw grid_start_x;
    C.set_source_rgb cr ~r:0.4 ~g:0.4 ~b:0.4;
    let msg =
      if grid_step >= 1.0 then Printf.sprintf "Each grid division: %.f s" grid_step
      else if grid_step >= 0.001 then Printf.sprintf "Each grid division: %.f ms" (grid_step *. 1000.)
      else if grid_step >= 0.000_001 then Printf.sprintf "Each grid division: %.f us" (grid_step *. 1_000_000.)
      else if grid_step >= 0.000_000_001 then Printf.sprintf "Each grid division: %.f ns" (grid_step *. 1_000_000_000.)
      else Printf.sprintf "Each grid division: %.2g s" grid_step in
    let extents = C.text_extents cr msg in
    let y = bottom -. C.(extents.height +. extents.y_bearing) -. 2.0 in
    C.paint_text cr ~x:4.0 ~y msg

  let draw_mark cr x y =
    C.move_to cr ~x ~y;
    C.line_to cr ~x ~y:(y +. 6.);
    C.stroke cr

  (** Draw [msg] in the area (min_x, max_x) and ideally centred at [x]. *)
  let draw_label cr ~v ~y ~min_x ~max_x x msg =
    let text_width = C.((text_extents cr msg).x_advance) in
    let x =
      x -. (text_width /. 2.)   (* Desired start for centred text *)
      |> min (max_x -. text_width)
      |> max min_x in

    if x +. text_width > max_x then (
      (* Doesn't fit. Draw as much as we can. *)
      C.paint_text cr ~x:min_x ~y ~clip_area:(max_x -. x, v.View.height) msg;
      max_x
    ) else (
      (* Show label on left margin if the thread starts off-screen *)
      let x =
        if x < 4.0 then min 4.0 (max_x -. text_width)
        else x in
        C.paint_text cr ~x ~y msg;
        x +. text_width
    )

  let rec draw_labels cr ~v ~y ~min_x ~max_x = function
    | [] -> ()
    | [(time, msg)] -> 
        let x = View.clip_x_of_time v time in
        let _end : float = draw_label cr ~v ~y ~min_x ~max_x x msg in
        draw_mark cr x y;
        ()
    | (t1, msg1) :: (((t2, _msg2) :: _) as rest) ->
        let x1 = View.clip_x_of_time v t1 in
        let x2 = View.clip_x_of_time v t2 in
        let min_x = draw_label cr ~v ~y ~min_x ~max_x:x2 x1 msg1 in
        draw_mark cr x1 y;
        draw_labels cr ~v ~y ~min_x ~max_x rest

  let render v cr ~expose_area =
    let vat = v.View.vat in
    let top_thread = Thread.top_thread vat in
    let ((expose_min_x, expose_min_y), (expose_max_x, expose_max_y)) = expose_area in

    (* Note: switching drawing colours is really slow with HTML canvas, so we try to group by colour. *)

    C.set_source_rgb cr ~r:0.9 ~g:0.9 ~b:0.9;
    C.paint cr;

    let region_labels = ref [] in

    (* When the system thread is "active", the system is idle. *)
    C.set_source_rgb cr ~r:0.8 ~g:0.8 ~b:0.8;
    Thread.activations top_thread |> List.iter (fun (start_time, end_time) ->
      let start_x = View.clip_x_of_time v start_time in
      let end_x = View.clip_x_of_time v end_time in
      if end_x >= expose_min_x && start_x < expose_max_x then (
        C.rectangle cr ~x:start_x ~y:expose_min_y ~w:(end_x -. start_x) ~h:expose_max_y;
        C.fill cr;
        if end_x -. start_x > 16. then region_labels := (start_x, end_x, "sleeping") :: !region_labels
      )
    );

    C.set_source_rgb cr ~r:0.7 ~g:0.6 ~b:0.6;
    Thread.gc_periods vat |> List.iter (fun (start_time, end_time) ->
      let start_x = View.clip_x_of_time v start_time in
      let end_x = View.clip_x_of_time v end_time in
      if end_x >= expose_min_x && start_x < expose_max_x then (
        C.rectangle cr ~x:start_x ~y:expose_min_y ~w:(end_x -. start_x) ~h:expose_max_y;
        C.fill cr;
        if end_x -. start_x > 16. then region_labels := (start_x, end_x, "GC") :: !region_labels
      )
    );

    C.set_source_rgb cr ~r:1.0 ~g:1.0 ~b:1.0;
    !region_labels |> List.iter (fun (min_x, max_x, label) ->
      let x = (min_x +. max_x) /. 2. in
      draw_label cr ~v ~y:(~-. 14.0 -. v.View.view_start_y) ~min_x ~max_x x label |> ignore
    );

    draw_grid v cr expose_min_x expose_max_x;

    let visible_t_min = View.time_of_x v expose_min_x in
    let visible_t_max = View.time_of_x v expose_max_x in
    let visible_threads = View.visible_threads v (visible_t_min, visible_t_max) in
    named_thread cr;
    visible_threads |> Layout.IT.IntervalSet.iter (fun i ->
    let t = i.Interval_tree.Interval.value in
      let start_x = View.clip_x_of_time v (Thread.start_time t) in
      let end_x = View.clip_x_of_time v (Thread.end_time t) in
      let y = View.y_of_thread v t in
      C.move_to cr ~x:start_x ~y;
      C.line_to cr ~x:end_x ~y;
      C.stroke cr;
      Thread.creates t |> List.iter (fun child ->
        let child_start_time = Thread.start_time child in
        line v cr child_start_time t child
      );
      begin match Thread.becomes t with
      | Some child when Thread.y child <> Thread.y t ->
          line v cr (Thread.end_time t) t child
      | _ -> () end;
    );

    activation cr;
    visible_threads |> Layout.IT.IntervalSet.iter (fun i ->
      let t = i.Interval_tree.Interval.value in
      let y = View.y_of_thread v t in
      Thread.activations t |> List.iter (fun (start_time, end_time) ->
        C.move_to cr ~x:(max expose_min_x (View.clip_x_of_time v start_time)) ~y;
        C.line_to cr ~x:(min expose_max_x (View.clip_x_of_time v end_time)) ~y;
        C.stroke cr;
      )
    );

    failed cr;
    visible_threads |> Layout.IT.IntervalSet.iter (fun i ->
      let t = i.Interval_tree.Interval.value in
      if Thread.failure t <> None then (
        let y = View.y_of_thread v t in
        let x = View.clip_x_of_time v (Thread.end_time t) in
        C.move_to cr ~x ~y:(y -. 8.);
        C.line_to cr ~x ~y:(y +. 8.);
        C.stroke cr;
      )
    );

    (* Arrows that are only just off screen can still be visible, so extend the
     * window slightly. Once we get wider than a screen width, they become invisible anyway. *)
    let view_timespace = View.timespan_of_width v v.View.view_width in
    let vis_arrows_min = visible_t_min -. view_timespace in
    let vis_arrows_max = visible_t_max +. view_timespace in
    thin cr;
    let c = (0.0, 0.0, 1.0) in
    begin let r, g, b = c in C.set_source_rgb cr ~r ~g ~b end;
    View.iter_interactions v vis_arrows_min vis_arrows_max (fun (t, start_time, op, other, end_time) ->
      match op with
      | Thread.Read when Thread.failure other = None -> arrow v cr other end_time t start_time c
      | _ -> ()
    );
    let c = (1.0, 0.0, 0.0) in
    begin let r, g, b = c in C.set_source_rgb cr ~r ~g ~b end;
    View.iter_interactions v vis_arrows_min vis_arrows_max (fun (t, start_time, op, other, end_time) ->
      match op with
      | Thread.Read when Thread.failure other <> None -> arrow v cr other end_time t start_time c
      | _ -> ()
    );
    let c = (0.0, 0.5, 0.0) in
    begin let r, g, b = c in C.set_source_rgb cr ~r ~g ~b end;
    View.iter_interactions v vis_arrows_min vis_arrows_max (fun (t, start_time, op, other, end_time) ->
      match op with
      | Thread.Resolve when Thread.id t <> -1 -> arrow v cr t start_time other end_time c
      | _ -> ()
    );

    thread_label cr;
    visible_threads |> Layout.IT.IntervalSet.iter (fun i ->
      let t = i.Interval_tree.Interval.value in
      let start_x = View.x_of_start v t +. 2. in
      let end_x = View.x_of_end v t in
      let thread_width = end_x -. start_x in
      if thread_width > 16. then (
        let y = View.y_of_thread v t -. 3.0 in
        let end_x =
          match Thread.becomes t with
          | Some child when Thread.y child = Thread.y t -> View.x_of_start v child
          | _ -> end_x in
        draw_labels cr ~v ~y ~min_x:start_x ~max_x:end_x (Thread.labels t)
      )
    );

    type_label cr;
    visible_threads |> Layout.IT.IntervalSet.iter (fun i ->
      let t = i.Interval_tree.Interval.value in
      let start_x = View.x_of_start v t +. 2. in
      let end_x = View.x_of_end v t in
      let thread_width = end_x -. start_x in
      if thread_width > 16. then (
        let y = View.y_of_thread v t +. 10.0 in
        let end_x =
          match Thread.becomes t with
          | Some child when Thread.y child = Thread.y t -> View.x_of_start v child
          | _ -> end_x in
        draw_label cr ~v ~y ~min_x:start_x ~max_x:end_x start_x (Thread.thread_type t)
        |> ignore;
      )
    );

    counter_line cr;
    Thread.counters vat |> List.iter (fun counter ->
      let open Counter in
      let range = counter.max -. counter.min in
      let v_scale = v.View.view_height /. range in
      let v_offset = v.View.view_height *. (1. +. counter.min /. range) in
      let y_of_value value = v_offset -. v_scale *. value in

      let values = counter.values in
      let i = Sorted_array.count_before (fun (time, _v) -> time >= v.View.view_start_time) values in
      let first_visible = max (i - 1) 0 in
      let y = ref (y_of_value (snd values.(first_visible))) in
      C.move_to cr ~x:0.0 ~y:!y;
      begin try
        values |> Array.iter (fun (time, value) ->
          let x = View.clip_x_of_time v time in
          C.line_to cr ~x ~y:!y;
          let new_y = y_of_value value in
          C.line_to cr ~x ~y:new_y;
          y := new_y;
          if x > v.View.view_width then raise Exit
        )
      with Exit -> () end;
      C.line_to cr ~x:v.View.view_width ~y:!y;
      C.stroke cr;
      draw_label cr ~v ~y:(!y +. 14.) ~min_x:0.0 ~max_x:v.View.view_width v.View.view_width counter.name |> ignore
    );
end
