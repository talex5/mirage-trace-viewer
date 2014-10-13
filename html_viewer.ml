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

let events = Event.([
  {time = 8249.521266; op = Switch 0};
  {time = 8249.521280; op = Reads (0, -1)};
  {time = 8249.521297; op = Label (0, "XenStore")};
  {time = 8249.521306; op = Reads (0, 0)};
  {time = 8249.521319; op = Creates (0, 1)};
  {time = 8249.521328; op = Creates (0, 2)};
  {time = 8249.521337; op = Creates (0, 3)};
  {time = 8249.521347; op = Label (3, "after-chn-1")};
  {time = 8249.521357; op = Creates (0, 4)};
  {time = 8249.521366; op = Creates (0, 5)};
  {time = 8249.521375; op = Creates (0, 6)};
  {time = 8249.521383; op = Creates (0, 7)};
  {time = 8249.521392; op = Creates (0, 8)};
  {time = 8249.521403; op = Creates (0, 9)};
  {time = 8249.521412; op = Reads (0, 0)};
  {time = 8249.521420; op = Reads (0, 0)};
  {time = 8249.521429; op = Reads (0, -1)};
  {time = 8249.521438; op = Reads (0, 0)};
  {time = 8249.521449; op = Creates (0, 10)};
  {time = 8249.521458; op = Reads (0, -1)};
  {time = 8249.521467; op = Reads (0, 0)};
  {time = 8249.521532; op = Reads (0, 0)};
  {time = 8249.521541; op = Reads (0, 0)};
  {time = 8249.521550; op = Reads (0, -1)};
  {time = 8249.521558; op = Reads (0, 0)};
  {time = 8249.521567; op = Creates (0, 11)};
  {time = 8249.521576; op = Creates (0, 12)};
  {time = 8249.521584; op = Label (12, "blkfront.enumerate")};
])

let top_thread = Thread.of_sexp (List.map Event.sexp_of_t events)

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

let () =
  let c =
    match Dom_html.tagged (Dom_html.getElementById "canvas") with
    | Dom_html.Canvas c -> c
    | _ -> assert false in
  Dom_html.addMousewheelEventListener c (zoom c) (Js.bool true) |> ignore;
  Dom_html.window##onload <- Dom_html.handler (main c);
  Dom_html.window##onresize <- Dom_html.handler (main c)
