(* Copyright (C) 2014, Thomas Leonard *)

let t0 = Unix.gettimeofday ()

module Canvas = struct
  type context = Dom_html.canvasRenderingContext2D Js.t

  let font_size = 12.0

  type text_extents = {
    x_bearing : float; 
    y_bearing : float;
    width : float;
    height : float;
    x_advance : float;
    y_advance : float;
  }

  let set_line_width context width = context##lineWidth <- width

  let set_source_rgba (context:context) ~r ~g ~b ~a =
    let c = Printf.sprintf "#%02x%02x%02x"
      (r *. 255. |> truncate)
      (g *. 255. |> truncate)
      (b *. 255. |> truncate) |> Js.string in
    context##globalAlpha <- a;
    context##fillStyle <- c;
    context##strokeStyle <- c

  let set_source_alpha (context:context) ~r:_ ~g:_ ~b:_ a =
    context##globalAlpha <- a

  let set_source_rgb (context:context) ~r ~g ~b = set_source_rgba context ~r ~g ~b ~a:1.0

  let move_to context ~x ~y = context##moveTo (x, y)
  let line_to context ~x ~y = context##lineTo (x, y)
  let rectangle context ~x ~y ~w ~h = context##rect (x, y, w, h)

  let stroke_preserve (context:context) = context##stroke ()

  let stroke (context:context) =
    context##stroke ();
    context##beginPath ()

  let fill (context:context) =
    context##fill ();
    context##beginPath ()

  let text_extents (context:context) msg =
    let width = (context##measureText (Js.string msg))##width in {
      x_bearing = 0.0;
      y_bearing = -.font_size;
      width;
      height = font_size;
      x_advance = width;
      y_advance = 0.0;
    }

  let paint_text (context:context) ?clip_area ~x ~y msg =
    match clip_area with
    | None -> context##fillText (Js.string msg, x, y)
    | Some (w, h) ->
        context##save ();
        context##rect (x, y -. font_size, w, h +. font_size);
        context##clip ();
        context##fillText (Js.string msg, x, y);
        context##restore ();
        context##beginPath ()

  let paint ?alpha (context:context) =
    let c = context##canvas in
    assert (alpha = None);
    context##fillRect (0., 0., float_of_int c##width, float_of_int c##height)
end

module R = Render.Make(Canvas)

type touch =
  | Touch_none
  | Touch_drag of (Thread.time * float)
  | Touch_zoom of (Thread.time * Thread.time)

let resize_callbacks = ref []
let () =
  let cb () = !resize_callbacks |> List.iter (fun f -> f ()) in
  Js.Unsafe.global##resizeCanvasElements <- Js.wrap_callback cb

(** Connect callbacks to render view [v] on canvas [c]. *)
let attach (c:Dom_html.canvasElement Js.t) v =
  let rel_coords (x, y) =
    let rec adjust (elem:Dom_html.element Js.t) (x, y) = 
      let x = x - elem##offsetLeft + elem##scrollLeft in
      let y = y - elem##offsetTop + elem##scrollTop in
      Js.Opt.case (elem##offsetParent)
        (fun () ->
          (float_of_int x, float_of_int y))
        (fun parent -> adjust parent (x, y)) in
    adjust (c :> Dom_html.element Js.t) (x, y) in

  let rel_mouse_coords ev =
    let x = Js.Optdef.get (ev##pageX) (fun () -> ev##clientX) in
    let y = Js.Optdef.get (ev##pageY) (fun () -> ev##clientY) in
    rel_coords (x, y) in

  let rel_touch_coords ev =
    let x = ev##pageX in
    let y = ev##pageY in
    rel_coords (x, y) in

  let render_queued = ref false in
  let render_now () =
    render_queued := false;
    let ctx = c##getContext(Dom_html._2d_) in
    ctx##font <- Js.string (Printf.sprintf "%.fpx Sans" Canvas.font_size);
    R.render v ctx ~expose_area:((0.0, 0.0), (float_of_int c##width, float_of_int c##height)) in

  let render () =
    if not (!render_queued) then (
      Dom_html._requestAnimationFrame (Js.wrap_callback (fun _ev -> render_now ()));
      render_queued := true
    ) in

  let resize () =
    let view_width = c##clientWidth in
    let view_height = c##clientHeight in
    c##width <- view_width;
    c##height <- view_height;
    let view_width = float_of_int view_width in
    let view_height = float_of_int view_height in
    View.set_size v view_width view_height;
    render () in

  let zoom (ev:Dom_html.mouseEvent Js.t) ~dx:_ ~dy =
    let (x, _) = rel_mouse_coords ev in
    let t_at_pointer = View.time_of_x v x in

    if dy < 0 then
      View.set_scale v (v.View.scale *. 1.2)
    else
      View.set_scale v (v.View.scale /. 1.2);
    let t_new_at_pointer = View.time_of_x v x in
    let _hscroll = View.set_start_time v (v.View.view_start_time -. (t_new_at_pointer -. t_at_pointer)) in
    render ();
    Js._false in

  let motion_id = ref None in

  let mouse_up _ev =
    begin match !motion_id with
    | None -> ()
    | Some id -> Dom_html.removeEventListener id; motion_id := None end;
    Js._false in

  let mouse_down (ev:Dom_html.mouseEvent Js.t) =
    let (x, y) = rel_mouse_coords ev in
    let start_time = View.time_of_x v x in
    let start_y = View.y_of_view_y v y in

    let motion (ev:Dom_html.mouseEvent Js.t) =
      let (x, y) = rel_mouse_coords ev in
      let time_at_pointer = View.time_of_x v x in
      let y_at_pointer = View.y_of_view_y v y in
      if time_at_pointer <> start_time || y_at_pointer <> start_y then (
        View.set_start_time v (start_time -. View.timespan_of_width v x) |> ignore;
        View.set_view_y_so v start_y y |> ignore;
        Dom_html.window##setTimeout (Js.wrap_callback (fun _ev -> render ()), 10.0) |> ignore
      );
      Js._false in

    let _ = mouse_up () in
    motion_id := Some (Dom_html.addEventListener c Dom_html.Event.mousemove (Dom_html.handler motion) (Js._true));
    Js._false in

  let double_click _ev =
    let t_min = v.View.view_start_time in
    let t_max = t_min +. View.timespan_of_width v v.View.view_width in
    Printf.printf "?t_min=%f&t_max=%f\n" t_min t_max;
    Js._false in

  let touches ts =
    let l = ts##length in
    let rec aux acc i =
      if i = l then List.rev acc else (
      Js.Optdef.case (ts##item (i)) (fun () -> List.rev acc)
        (fun t -> aux (t :: acc) (i + 1))
      ) in
    aux [] 0 in

  let touch = ref Touch_none in
  let touch_change (ev:Dom_html.touchEvent Js.t) =
    Dom.preventDefault ev;
    begin match touches ev##touches with
    | [t] ->
        let (x, y) = rel_touch_coords t in
        touch := Touch_drag (
          View.time_of_x v x,
          View.view_y_of_y v y
        )
    | [t0; t1] ->
        let (x0, _) = rel_touch_coords t0 in
        let (x1, _) = rel_touch_coords t1 in
        touch := Touch_zoom (
          (View.time_of_x v x0),
          (View.time_of_x v x1)
        )
    | _ -> touch := Touch_none end;
    Js._false in

  let touch_move (ev:Dom_html.touchEvent Js.t) =
    begin match !touch, touches ev##touches with
    | Touch_drag (start_time, start_y), [touch] ->
        let x_new, view_y_new = rel_touch_coords touch in
        let t_new = View.x_of_time v x_new in
        let y_new = View.y_of_view_y v view_y_new in
        if t_new <> start_time || start_y <> y_new then (
          View.set_start_time v (start_time -. View.timespan_of_width v x_new) |> ignore;
          View.set_view_y_so v start_y view_y_new |> ignore;
          Dom_html.window##setTimeout (Js.wrap_callback (fun _ev -> render ()), 10.0) |> ignore
          )
    | Touch_zoom (start_t0, start_t1), [touch0; touch1] ->
        let (x0, _) = rel_touch_coords touch0 in
        let (x1, _) = rel_touch_coords touch1 in
        View.set_start_time v (start_t0 -. View.timespan_of_width v x0) |> ignore;
        View.set_scale v ((x1 -. x0) /. (start_t1 -. start_t0));
        Dom_html.window##setTimeout (Js.wrap_callback (fun _ev -> render ()), 10.0) |> ignore
    | _ -> ()
    end;
    Js._false in

  Dom_html.addMousewheelEventListener c zoom (Js.bool true) |> ignore;
  c##ondblclick <- Dom_html.handler double_click;
  c##onmousedown <- Dom_html.handler mouse_down;
  c##onmouseup <- Dom_html.handler mouse_up;
  c##onmouseout <- Dom_html.handler mouse_up;

  Dom_html.addEventListener c Dom_html.Event.touchstart (Dom_html.handler touch_change) (Js.bool true) |> ignore;
  Dom_html.addEventListener c Dom_html.Event.touchmove (Dom_html.handler touch_move) (Js.bool true) |> ignore;
  Dom_html.addEventListener c Dom_html.Event.touchend (Dom_html.handler touch_change) (Js.bool true) |> ignore;
  Dom_html.addEventListener c Dom_html.Event.touchcancel (Dom_html.handler touch_change) (Js.bool true) |> ignore;

  let resize_false _ = resize (); Js._false in
  Dom_html.addEventListener Dom_html.window Dom_html.Event.resize (Dom_html.handler resize_false) (Js.bool true) |> ignore;
  resize_callbacks := resize :: !resize_callbacks;
  resize ();
