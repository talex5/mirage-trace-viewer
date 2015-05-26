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

(* Find a place to put the label for the next stat line, ideally close to y. *)
let insert_label y stat_labels =
  let rec aux (y:float) = function
    | [] -> y, [y]
    | y2 :: ys when y +. 16. < y2 -> y, (y :: y2 :: ys)
    | y2 :: ys ->
        let y, ys = aux (max y (y2 +. 16.)) ys in
        y, (y2 :: ys) in
  let y, new_stats = aux y !stat_labels in
  stat_labels := new_stats;
  y

module Make (C : CANVAS) = struct
  let arrow_width = 4.
  let arrow_height = 10.

  let thin cr = C.set_line_width cr 1.0

  let thread_label cr =
    C.set_source_rgb cr ~r:0.0 ~g:0.0 ~b:0.0

  let type_label cr =
    C.set_source_rgb cr ~r:0.5 ~g:0.5 ~b:0.5

  let counter_line_width = 5.0

  let counter_shadow cr =
    C.set_source_rgb cr ~r:0.0 ~g:0.0 ~b:0.0;
    C.set_line_width cr 5.0

  let counter_line i cr =
    C.set_line_width cr 3.0;
    match i mod 4 with
    | 0 -> C.set_source_rgb cr ~r:1.0 ~g:0.4 ~b:0.4
    | 1 -> C.set_source_rgb cr ~r:1.0 ~g:0.5 ~b:0.0
    | 2 -> C.set_source_rgb cr ~r:0.4 ~g:0.8 ~b:0.8
    | _ -> C.set_source_rgb cr ~r:0.8 ~g:0.4 ~b:1.0

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
    C.move_to cr ~x:(Mtv_view.x_of_time v time) ~y:(Mtv_view.y_of_thread v src);
    C.line_to cr ~x:(Mtv_view.x_of_time v time) ~y:(Mtv_view.y_of_thread v recv);
    C.stroke cr

  let draw_arrow_head_v cr ~x ~y ~arrow_head_y =
    C.line_to cr ~x ~y:arrow_head_y;
    C.stroke cr;
    C.move_to cr ~x ~y;
    C.line_to cr ~x:(x +. arrow_width) ~y:arrow_head_y;
    C.line_to cr ~x:(x -. arrow_width) ~y:arrow_head_y;
    C.fill cr

  let draw_arrow_head_h cr ~x ~y ~arrow_head_x =
    C.line_to cr ~x:arrow_head_x ~y;
    C.stroke cr;
    C.move_to cr ~x ~y;
    C.line_to cr ~x:arrow_head_x ~y:(y +. arrow_width);
    C.line_to cr ~x:arrow_head_x ~y:(y -. arrow_width);
    C.fill cr

  let arrow v cr src src_time recv recv_time (r, g, b) =
    let width = Mtv_view.width_of_timespan v (recv_time -. src_time) in
    let alpha = 1.0 -. (min 1.0 (width /. 6000.)) in
    if alpha > 0.01 then (
      C.set_source_alpha cr ~r ~g ~b alpha;

      if Mtv_thread.id src <> -1  && Mtv_thread.id src <> Mtv_thread.id recv then (
        let src_x = Mtv_view.clip_x_of_time v src_time in
        let src_y = Mtv_view.y_of_thread v src in
        let recv_y = Mtv_view.y_of_thread v recv in

        C.move_to cr ~x:src_x ~y:src_y;

        let x = Mtv_view.clip_x_of_time v recv_time in
        let d = recv_y -. src_y in
        if d < -.arrow_height then draw_arrow_head_v cr ~x ~y:recv_y ~arrow_head_y:(recv_y +. arrow_height)
        else if d > arrow_height then draw_arrow_head_v cr ~x ~y:recv_y ~arrow_head_y:(recv_y -. arrow_height)
        else draw_arrow_head_h cr ~x ~y:recv_y ~arrow_head_x:(x -. arrow_height)
      )
    )

  let draw_grid v cr area_start_x area_end_x =
    C.set_line_width cr 1.0;
    C.set_source_rgb cr ~r:0.7 ~g:0.7 ~b:0.7;

    let grid_step = Mtv_view.grid_step v in
    let top = 0.0 in
    let bottom = Mtv_view.view_height v in

    let area_start_time = Mtv_view.time_of_x v area_start_x in
    let grid_start_x = floor (area_start_time /. grid_step) *. grid_step |> Mtv_view.x_of_time v in
    let grid_step_x = Mtv_view.width_of_timespan v grid_step in
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
      C.paint_text cr ~x:min_x ~y ~clip_area:(max_x -. x, Mtv_view.view_height v) msg;
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
        let x = Mtv_view.clip_x_of_time v time in
        let _end : float = draw_label cr ~v ~y ~min_x ~max_x x msg in
        draw_mark cr x y;
        ()
    | (t1, msg1) :: (((t2, _msg2) :: _) as rest) ->
        let x1 = Mtv_view.clip_x_of_time v t1 in
        let x2 = Mtv_view.clip_x_of_time v t2 in
        let min_x = draw_label cr ~v ~y ~min_x ~max_x:x2 x1 msg1 in
        draw_mark cr x1 y;
        draw_labels cr ~v ~y ~min_x ~max_x rest

  let render v cr ~expose_area =
    let vat = Mtv_view.vat v in
    let top_thread = Mtv_thread.top_thread vat in
    let ((expose_min_x, expose_min_y), (expose_max_x, expose_max_y)) = expose_area in

    (* Note: switching drawing colours is really slow with HTML canvas, so we try to group by colour. *)

    C.set_source_rgb cr ~r:0.9 ~g:0.9 ~b:0.9;
    C.paint cr;

    let region_labels = ref [] in

    (* When the system thread is "active", the system is idle. *)
    C.set_source_rgb cr ~r:0.8 ~g:0.8 ~b:0.8;
    Mtv_thread.activations top_thread |> List.iter (fun (start_time, end_time) ->
      let start_x = Mtv_view.clip_x_of_time v start_time in
      let end_x = Mtv_view.clip_x_of_time v end_time in
      if end_x >= expose_min_x && start_x < expose_max_x then (
        C.rectangle cr ~x:start_x ~y:expose_min_y ~w:(end_x -. start_x) ~h:expose_max_y;
        C.fill cr;
        if end_x -. start_x > 16. then region_labels := (start_x, end_x, "sleeping") :: !region_labels
      )
    );

    C.set_source_rgb cr ~r:0.7 ~g:0.6 ~b:0.6;
    Mtv_thread.gc_periods vat |> List.iter (fun (start_time, end_time) ->
      let start_x = Mtv_view.clip_x_of_time v start_time in
      let end_x = Mtv_view.clip_x_of_time v end_time in
      if end_x >= expose_min_x && start_x < expose_max_x then (
        C.rectangle cr ~x:start_x ~y:expose_min_y ~w:(end_x -. start_x) ~h:expose_max_y;
        C.fill cr;
        if end_x -. start_x > 16. then region_labels := (start_x, end_x, "GC") :: !region_labels
      )
    );

    C.set_source_rgb cr ~r:1.0 ~g:1.0 ~b:1.0;
    !region_labels |> List.iter (fun (min_x, max_x, label) ->
      let x = (min_x +. max_x) /. 2. in
      draw_label cr ~v ~y:14.0 ~min_x ~max_x x label |> ignore
    );

    draw_grid v cr expose_min_x expose_max_x;

    (* Draw the thread lines. *)
    let failed_thread_lines = ref [] in
    let draw_thread_line start_x end_x y =
      C.move_to cr ~x:start_x ~y;
      C.line_to cr ~x:end_x ~y;
      C.stroke cr in
    let visible_t_min = Mtv_view.time_of_x v expose_min_x in
    let visible_t_max = Mtv_view.time_of_x v expose_max_x in
    let visible_threads = Mtv_view.visible_threads v (visible_t_min, visible_t_max) in
    named_thread cr;
    visible_threads |> Mtv_layout.IT.IntervalSet.iter (fun i ->
      let t = i.Interval_tree.Interval.value in
      let start_x = Mtv_view.clip_x_of_time v (Mtv_thread.start_time t) in
      let end_x = Mtv_view.clip_x_of_time v (Mtv_thread.end_time t) in
      let y = Mtv_view.y_of_thread v t in
      if Mtv_thread.failure t = None then draw_thread_line start_x end_x y
      else failed_thread_lines := (start_x, end_x, y) :: !failed_thread_lines;
      Mtv_thread.creates t |> List.iter (fun child ->
        let child_start_time = Mtv_thread.start_time child in
        if Mtv_thread.show_creation child then
          line v cr child_start_time t child
      );
      begin match Mtv_thread.becomes t with
      | Some child when Mtv_thread.y child <> Mtv_thread.y t ->
          line v cr (Mtv_thread.end_time t) t child
      | _ -> () end;
      if not (Mtv_thread.resolved t) && end_x -. start_x > 4.0 then (
        C.move_to cr ~x:end_x ~y;
        C.line_to cr ~x:(end_x -. 6.) ~y:(y -. 4.);
        C.line_to cr ~x:(end_x -. 6.) ~y:(y +. 4.);
        C.fill cr;
      )
    );

    activation cr;
    visible_threads |> Mtv_layout.IT.IntervalSet.iter (fun i ->
      let t = i.Interval_tree.Interval.value in
      let y = Mtv_view.y_of_thread v t in
      Mtv_thread.activations t |> List.iter (fun (start_time, end_time) ->
        C.move_to cr ~x:(max expose_min_x (Mtv_view.clip_x_of_time v start_time)) ~y;
        C.line_to cr ~x:(min expose_max_x (Mtv_view.clip_x_of_time v end_time)) ~y;
        C.stroke cr;
      )
    );

    (* Arrows that are only just off screen can still be visible, so extend the
     * window slightly. Once we get wider than a screen width, they become invisible anyway. *)
    let view_timespace = Mtv_view.timespan_of_width v (Mtv_view.view_width v) in
    let vis_arrows_min = visible_t_min -. view_timespace in
    let vis_arrows_max = visible_t_max +. view_timespace in
    thin cr;
    let c = (0.8, 0.8, 0.4) in
    begin let r, g, b = c in C.set_source_rgb cr ~r ~g ~b end;
    Mtv_view.iter_interactions v vis_arrows_min vis_arrows_max (fun (t, start_time, op, other, end_time) ->
      match op with
      | Mtv_thread.Try_read -> arrow v cr t start_time other end_time c
      | _ -> ()
    );
    let c = (0.0, 0.0, 1.0) in
    begin let r, g, b = c in C.set_source_rgb cr ~r ~g ~b end;
    Mtv_view.iter_interactions v vis_arrows_min vis_arrows_max (fun (t, start_time, op, other, end_time) ->
      match op with
      | Mtv_thread.Read when Mtv_thread.failure other = None -> arrow v cr other end_time t start_time c
      | _ -> ()
    );
    let c = (1.0, 0.0, 0.0) in
    begin let r, g, b = c in C.set_source_rgb cr ~r ~g ~b end;
    Mtv_view.iter_interactions v vis_arrows_min vis_arrows_max (fun (t, start_time, op, other, end_time) ->
      match op with
      | Mtv_thread.Read when Mtv_thread.failure other <> None -> arrow v cr other end_time t start_time c
      | _ -> ()
    );
    let c = (0.0, 0.5, 0.0) in
    begin let r, g, b = c in C.set_source_rgb cr ~r ~g ~b end;
    Mtv_view.iter_interactions v vis_arrows_min vis_arrows_max (fun (t, start_time, op, other, end_time) ->
      match op with
      | Mtv_thread.Resolve when Mtv_thread.id t <> -1 -> arrow v cr t start_time other end_time c
      | _ -> ()
    );
    let c = (1.0, 0.6, 0.0) in
    begin let r, g, b = c in C.set_source_rgb cr ~r ~g ~b end;
    Mtv_view.iter_interactions v vis_arrows_min vis_arrows_max (fun (t, start_time, op, other, end_time) ->
      match op with
      | Mtv_thread.Signal -> arrow v cr t start_time other end_time c
      | _ -> ()
    );

    let text_visible t =
      let vert_dist = Mtv_view.dist_from_focus v t in
      vert_dist > -.2000. && vert_dist < 2000. in

    thread_label cr;
    visible_threads |> Mtv_layout.IT.IntervalSet.iter (fun i ->
      let t = i.Interval_tree.Interval.value in
      let start_x = Mtv_view.x_of_start v t +. 2. in
      let end_x = Mtv_view.x_of_end v t in
      let thread_width = end_x -. start_x in
      if thread_width > 16. && text_visible t then (
        let y = Mtv_view.y_of_thread v t -. 3.0 in
        let end_x =
          match Mtv_thread.becomes t with
          | Some child when Mtv_thread.y child = Mtv_thread.y t -> Mtv_view.x_of_start v child
          | _ -> end_x in
        draw_labels cr ~v ~y ~min_x:start_x ~max_x:(min end_x (Mtv_view.view_width v)) (Mtv_thread.labels t)
      )
    );

    let text_visible t =
      let vert_dist = Mtv_view.dist_from_focus v t in
      vert_dist > -.1000. && vert_dist < 1000. in

    type_label cr;
    visible_threads |> Mtv_layout.IT.IntervalSet.iter (fun i ->
      let t = i.Interval_tree.Interval.value in
      let start_x = Mtv_view.x_of_start v t +. 2. in
      let end_x = Mtv_view.x_of_end v t in
      let thread_width = end_x -. start_x in
      if thread_width > 16. && text_visible t then (
        let y = Mtv_view.y_of_thread v t +. 10.0 in
        let end_x =
          match Mtv_thread.becomes t with
          | Some child when Mtv_thread.y child = Mtv_thread.y t -> Mtv_view.x_of_start v child
          | _ -> end_x in
        draw_label cr ~v ~y ~min_x:start_x ~max_x:end_x start_x (Mtv_thread.thread_type t)
        |> ignore;
      )
    );

    failed cr;
    !failed_thread_lines |> List.iter (fun (start_x, end_x, y) ->
      draw_thread_line start_x end_x y;
      C.move_to cr ~x:end_x ~y:(y -. 8.);
      C.line_to cr ~x:end_x ~y:(y +. 8.);
      C.stroke cr;
    );

    let stat_labels = ref [] in
    Mtv_thread.counters vat |> List.iteri (fun counter_i counter ->
      let open Mtv_counter in
      if counter.shown then (
        let range = counter.max -. counter.min in
        let v_scale = (Mtv_view.view_height v -. (2.0 *. counter_line_width)) /. range in
        let v_offset = Mtv_view.view_height v *. (1. +. counter.min /. range) -. counter_line_width in
        let y_of_value value = v_offset -. v_scale *. value in

        let values = counter.values in
        let i = Mtv_sorted_array.count_before (fun (time, _v) -> time >= Mtv_view.view_start_time v) values in
        let first_visible = max (i - 1) 0 in
        let first_value =
          if i = 0 then 0.0
          else (snd values.(first_visible)) in
        let y = ref (y_of_value first_value) in
        C.move_to cr ~x:0.0 ~y:!y;
        begin try
          for i = first_visible to Array.length values - 1 do
            let time, value = Array.get values i in
            let x = Mtv_view.clip_x_of_time v time in
            C.line_to cr ~x ~y:!y;
            if x > Mtv_view.view_width v then raise Exit;
            let new_y = y_of_value value in
            C.line_to cr ~x ~y:new_y;
            y := new_y;
          done
        with Exit -> () end;
        C.line_to cr ~x:(Mtv_view.view_width v) ~y:!y;
        counter_shadow cr;
        C.stroke_preserve cr;
        counter_line counter_i cr;
        C.stroke cr;

        let y = insert_label (max 16. (!y -. 2.)) stat_labels in
        let max_x = Mtv_view.view_width v in
        draw_label cr ~v ~y ~min_x:0.0 ~max_x max_x counter.name |> ignore
      )
    );
end
