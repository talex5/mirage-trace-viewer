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

let arg name =
  let qname = "?" ^ name in
  let _, v = Url.Current.arguments |> List.find (fun (k, _v) -> k = name || k = qname) in
  v

let v =
  let t1 = Unix.gettimeofday () in
  let ch = open_in "/static/log-x86.bin" in
  let v = Marshal.from_channel ch in
  close_in ch;
  let t2 = Unix.gettimeofday () in
  Printf.printf "Load time: %.2f\n" (t2 -. t1);
  Printf.printf "Since startup: %.2f\n" (t2 -. t0);
  begin try
    let t_min = arg "t_min" |> float_of_string in
    let t_max = arg "t_max" |> float_of_string in
    let scale = (v.View.view_width -. View.h_margin *. 2.) /. (t_max -. t_min) in
    View.set_scale v scale;
    View.set_start_time v t_min |> ignore
  with Not_found -> print_endline "Not_found" end;
  v

let render_queued = ref false
let render_now c =
  render_queued := false;
  Printf.printf "render_now: %.2f\n" (Unix.gettimeofday () -. t0);
  (* let t0 = Unix.gettimeofday () in *)
  let ctx = c##getContext(Dom_html._2d_) in
  ctx##font <- Js.string (Printf.sprintf "%.fpx Sans" Canvas.font_size);
  R.render v ctx ~expose_area:((0.0, 0.0), (float_of_int c##width, float_of_int c##height));
  Printf.printf "render done: %.2f\n" (Unix.gettimeofday () -. t0)
  (* ;
  let t1 = Unix.gettimeofday () in
  Printf.printf "Render time: %.2f\n" (t1 -. t0)
  *)

let render c =
  if not (!render_queued) then (
    Dom_html._requestAnimationFrame (Js.wrap_callback (fun _ev -> render_now c));
    render_queued := true
  )

let main c _ =
  let view_width = c##clientWidth in
  let view_height = c##clientHeight in
  c##width <- view_width;
  c##height <- view_height;
  let view_width = float_of_int view_width in
  let view_height = float_of_int view_height in
  View.set_size v view_width view_height;
  render c;
  Js._false

let zoom c (ev:Dom_html.mouseEvent Js.t) ~dx:_ ~dy =
  let x = float_of_int ev##clientX in
  let t_at_pointer = View.time_of_x v x in

  if dy < 0 then
    View.set_scale v (v.View.scale *. 1.2)
  else
    View.set_scale v (v.View.scale /. 1.2);
  let t_new_at_pointer = View.time_of_x v x in
  let _hscroll = View.set_start_time v (v.View.view_start_time -. (t_new_at_pointer -. t_at_pointer)) in
  render c;
  Js._false

let motion_id = ref None

let mouse_up _ev =
  begin match !motion_id with
  | None -> ()
  | Some id -> Dom_html.removeEventListener id; motion_id := None end;
  Js._false

let mouse_down c (ev:Dom_html.mouseEvent Js.t) =
  let start_time = View.time_of_x v (float_of_int ev##clientX) in
  let start_y = float_of_int ev##clientY in

  let motion (ev:Dom_html.mouseEvent Js.t) =
    let x = float_of_int ev##clientX in
    let y = float_of_int ev##clientY in
    let time_at_pointer = View.time_of_x v x in
    if time_at_pointer <> start_time || start_y <> y then (
      View.set_start_time v (start_time -. View.timespan_of_width v x) |> ignore;
      View.set_view_y v (start_y -. y) |> ignore;
      render c;
    );
    Js._false in

  let _ = mouse_up () in
  motion_id := Some (Dom_html.addEventListener c Dom_html.Event.mousemove (Dom_html.handler motion) (Js._true));
  Js._false

let double_click _ev =
  let t_min = v.View.view_start_time in
  let t_max = t_min +. View.timespan_of_width v v.View.view_width in
  Printf.printf "?t_min=%f&t_max=%f\n" t_min t_max;
  Js._false

type touch =
  | Touch_none
  | Touch_drag of (Thread.time * float)
  | Touch_zoom of (Thread.time * Thread.time)

let touches ts =
  let l = ts##length in
  let rec aux acc i =
    if i = l then List.rev acc else (
    Js.Optdef.case (ts##item (i)) (fun () -> List.rev acc)
      (fun t -> aux (t :: acc) (i + 1))
    ) in
  aux [] 0

let touch = ref Touch_none
let touch_change (ev:Dom_html.touchEvent Js.t) =
  Dom.preventDefault ev;
  begin match touches ev##touches with
  | [t] -> touch := Touch_drag (View.time_of_x v (float_of_int t##clientX), float_of_int t##clientY)
  | [t0; t1] ->
      touch := Touch_zoom (
        (View.time_of_x v (float_of_int t0##clientX)),
        (View.time_of_x v (float_of_int t1##clientX))
      )
  | _ -> touch := Touch_none end;
  Js._false

let touch_move c (ev:Dom_html.touchEvent Js.t) =
  begin match !touch, touches ev##touches with
  | Touch_drag (start_time, start_y), [touch] ->
      let x_new = float_of_int touch##clientX in
      let y_new = float_of_int touch##clientY in
      let t_new = View.x_of_time v x_new in
      if t_new <> start_time || start_y <> y_new then (
        View.set_start_time v (start_time -. View.timespan_of_width v x_new) |> ignore;
        View.set_view_y v (start_y -. y_new) |> ignore;
        render c;
        )
  | Touch_zoom (start_t0, start_t1), [touch0; touch1] ->
      let x0 = float_of_int touch0##clientX in
      let x1 = float_of_int touch1##clientX in
      View.set_start_time v (start_t0 -. View.timespan_of_width v x0) |> ignore;
      View.set_scale v ((x1 -. x0) /. (start_t1 -. start_t0));
      render c;
  | _ -> ()
  end;
  Js._false

let () =
  let c =
    match Dom_html.tagged (Dom_html.getElementById "canvas") with
    | Dom_html.Canvas c -> c
    | _ -> assert false in
  Dom_html.addMousewheelEventListener c (zoom c) (Js.bool true) |> ignore;
  c##ondblclick <- Dom_html.handler double_click;
  c##onmousedown <- Dom_html.handler (mouse_down c);
  c##onmouseup <- Dom_html.handler mouse_up;
  c##onmouseout <- Dom_html.handler mouse_up;

  Dom_html.addEventListener c Dom_html.Event.touchstart (Dom_html.handler touch_change) (Js.bool true) |> ignore;
  Dom_html.addEventListener c Dom_html.Event.touchmove (Dom_html.handler (touch_move c)) (Js.bool true) |> ignore;
  Dom_html.addEventListener c Dom_html.Event.touchend (Dom_html.handler touch_change) (Js.bool true) |> ignore;
  Dom_html.addEventListener c Dom_html.Event.touchcancel (Dom_html.handler touch_change) (Js.bool true) |> ignore;

  Dom_html.window##onload <- Dom_html.handler (main c);
  Dom_html.window##onresize <- Dom_html.handler (main c)
