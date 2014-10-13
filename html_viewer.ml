(* Copyright (C) 2014, Thomas Leonard *)

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
    let c = Printf.sprintf "rgba(%.f,%.f,%.f,%f)"
        (r *. 255.)
        (g *. 255.)
        (b *. 255.)
        a |> Js.string in
    context##fillStyle <- c;
    context##strokeStyle <- c

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
          context##rect (x, y, w, h);
          context##clip ();
          context##fillText (Js.string msg, x, y);
          context##restore ()

  let paint ?alpha (context:context) =
    let c = context##canvas in
    assert (alpha = None);
    context##fillRect (0., 0., float_of_int c##width, float_of_int c##height)
end

module R = Render.Make(Canvas)

let top_thread =
  let ch = open_in "/static/log-x86.sexp" in
  let top_thread = Thread.from_channel ch in
  close_in ch;
  top_thread

let v = View.make ~top_thread ~view_width:640. ~view_height:480.

let render c =
  let ctx = c##getContext(Dom_html._2d_) in
  ctx##font <- Js.string (Printf.sprintf "%.fpx Sans" Canvas.font_size);
  R.render v ctx ~expose_area:((0.0, 0.0), (float_of_int c##width, float_of_int c##height))

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
    );
    render c;
    Js._false in

  let _ = mouse_up () in
  motion_id := Some (Dom_html.addEventListener c Dom_html.Event.mousemove (Dom_html.handler motion) (Js._true));
  Js._false

let () =
  let c =
    match Dom_html.tagged (Dom_html.getElementById "canvas") with
    | Dom_html.Canvas c -> c
    | _ -> assert false in
  Dom_html.addMousewheelEventListener c (zoom c) (Js.bool true) |> ignore;
  c##onmousedown <- Dom_html.handler (mouse_down c);
  c##onmouseup <- Dom_html.handler mouse_up;
  c##onmouseout <- Dom_html.handler mouse_up;
  Dom_html.window##onload <- Dom_html.handler (main c);
  Dom_html.window##onresize <- Dom_html.handler (main c)
