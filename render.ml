let (==>) (signal:(callback:_ -> GtkSignal.id)) callback =
  ignore (signal ~callback)

let margin = 20.

let calc_grid_step scale =
  let l = 2.5 -. (log scale /. log 10.) |> floor in
  10. ** l

let scale = ref 1000.
let trace_start_time = ref 0.0
let view_start_time = ref 0.0
let x_of_time t = (t -. !view_start_time)  *. !scale
let time_of_x x = (x /. !scale) +. !view_start_time

let x_of_start t = x_of_time (Thread.start_time t)
let x_of_end t = x_of_time (Thread.end_time t)

let clip_x_of_time t =
  x_of_time t
  |> min 1_000_000.
  |> max (-1_000_000.)

let grid_step = ref (calc_grid_step !scale)

let view_start_y = ref 0.0
let y_of_thread t = Thread.y t -. !view_start_y

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

let line cr time src recv colour =
  colour cr;
  Cairo.move_to cr ~x:(x_of_time time) ~y:(y_of_thread src);
  Cairo.line_to cr ~x:(x_of_time time) ~y:(y_of_thread recv);
  Cairo.stroke cr

let arrow cr src src_time recv recv_time (r, g, b) =
  let width = (recv_time -. src_time) *. !scale in
  let alpha = 1.0 -. (min 1.0 (width /. 6000.)) in
  if alpha > 0.01 then (
    Cairo.set_source_rgba cr ~r ~g ~b ~a:alpha;

    if Thread.id src <> -1  && Thread.id src <> Thread.id recv then (
      let src_x = clip_x_of_time src_time in
      let src_y = y_of_thread src in
      let recv_y = y_of_thread recv in

      Cairo.move_to cr ~x:src_x ~y:src_y;
      let arrow_head_y =
        if src_y < recv_y then recv_y -. arrow_height
        else recv_y +. arrow_height in
      let x = clip_x_of_time recv_time in
      Cairo.line_to cr ~x ~y:arrow_head_y;
      Cairo.line_to cr ~x:(x +. arrow_width) ~y:arrow_head_y;
      Cairo.line_to cr ~x ~y:recv_y;
      Cairo.line_to cr ~x:(x -. arrow_width) ~y:arrow_head_y;
      Cairo.line_to cr ~x ~y:arrow_head_y;
      Cairo.stroke_preserve cr;
      Cairo.fill cr
    )
  )

let draw_grid cr area  =
  Cairo.set_line_width cr 1.0;
  Cairo.set_source_rgb cr ~r:0.8 ~g:0.8 ~b:0.8;

  let top = Gdk.Rectangle.(y area) |> float_of_int in
  let bottom = Gdk.Rectangle.(y area + height area) |> float_of_int in

  let area_start_time = time_of_x (float_of_int (Gdk.Rectangle.(x area))) in
  let grid_start_x = floor (area_start_time /. !grid_step) *. !grid_step |> x_of_time in
  let area_end_x = Gdk.Rectangle.(x area + width area) |> float_of_int in
  let grid_step_x = !grid_step *. !scale in
  let rec draw x =
    if x < area_end_x then (
      Cairo.move_to cr ~x:x ~y:top;
      Cairo.line_to cr ~x:x ~y:bottom;
      Cairo.stroke cr;
      draw (x +. grid_step_x)
    ) in
  draw grid_start_x;
  Cairo.move_to cr ~x:4.0 ~y:(bottom -. 4.);
  Cairo.set_source_rgb cr ~r:0.4 ~g:0.4 ~b:0.4;
  let msg =
    if !grid_step >= 1.0 then Printf.sprintf "Each grid division: %.f s" !grid_step
    else if !grid_step >= 0.001 then Printf.sprintf "Each grid division: %.f ms" (!grid_step *. 1000.)
    else if !grid_step >= 0.000_001 then Printf.sprintf "Each grid division: %.f us" (!grid_step *. 1_000_000.)
    else if !grid_step >= 0.000_000_001 then Printf.sprintf "Each grid division: %.f ns" (!grid_step *. 1_000_000_000.)
    else Printf.sprintf "Each grid division: %.2g s" !grid_step in
  Cairo.show_text cr msg

let render top_thread =
  GMain.init () |> ignore;
  let win = GWindow.window ~title:"Mirage Trace Toolkit" () in
  win#set_default_size
    ~width:(Gdk.Screen.width () / 2)
    ~height:(Gdk.Screen.height () / 2);
  let hadjustment = GData.adjustment () in
  let vadjustment = GData.adjustment () in
  let table = GPack.table ~rows:2 ~columns:2 ~homogeneous:false ~packing:win#add () in
  let area = GMisc.drawing_area ~packing:(table#attach ~left:0 ~top:0 ~expand:`BOTH ~fill:`BOTH) () in

  let _hscroll = GRange.scrollbar `HORIZONTAL ~adjustment:hadjustment ~packing:(table#attach ~left:0 ~top:1 ~expand:`X ~fill:`BOTH) () in
  let _vscroll = GRange.scrollbar `VERTICAL ~adjustment:vadjustment ~packing:(table#attach ~left:1 ~top:0 ~expand:`Y ~fill:`BOTH) () in

  win#show ();
  let trace_end_time = Thread.end_time top_thread in
  trace_start_time := Thread.start_time top_thread;
  view_start_time := !trace_start_time -. (margin /. !scale);

  let layout, max_y = Layout.arrange top_thread in

  let set_scollbars alloc =
    let max_x = (trace_end_time -. !trace_start_time) *. !scale in
    hadjustment#set_bounds ~lower:(-.margin) ~upper:(max_x +. margin) ~page_size:(float_of_int alloc.Gtk.width) ();
    vadjustment#set_bounds ~lower:(-.margin) ~upper:(max_y +. margin) ~page_size:(float_of_int alloc.Gtk.height) () in

  area#misc#connect#size_allocate ==> (fun alloc -> set_scollbars alloc);

  let draw_interactions cr t =
    Thread.interactions t |> List.iter (fun (time, op, other) ->
      match op with
      | Thread.Read ->
          let end_time = Thread.end_time other in
          thin cr;
          arrow cr other end_time t time (0.0, 0.0, 1.0)
      | Thread.Resolve ->
          if Thread.id t <> -1 then (
            let start_time = time
              |> min (Thread.end_time t) in
            arrow cr t start_time other time (0.0, 0.5, 0.0)
          )
    ) in

  area#event#connect#expose ==> (fun ev ->
    let cr = Cairo_gtk.create area#misc#window in
    let expose_area = GdkEvent.Expose.area ev in

    Cairo.set_source_rgb cr ~r:0.9 ~g:0.9 ~b:0.9;
    Cairo.paint cr;

    Cairo.set_font_size cr 12.;
    Cairo.select_font_face cr "Sans";

    draw_grid cr expose_area;

    Cairo.set_line_width cr 2.0;
    Cairo.set_source_rgb cr ~r:1. ~g:1. ~b:1.;
    Cairo.set_line_join cr Cairo.JOIN_BEVEL;

    let visible_x_min = float_of_int (Gdk.Rectangle.x expose_area) in
    let visible_x_max = float_of_int (Gdk.Rectangle.(x expose_area + width expose_area)) in
    let visible_t_min = time_of_x visible_x_min in
    let visible_t_max = time_of_x visible_x_max in
    let visible_threads = Layout.IT.overlapping_interval layout (visible_t_min, visible_t_max) in
    visible_threads |> Layout.IT.IntervalSet.iter (fun i ->
      let t = i.Interval_tree.Interval.value in
      if Thread.label t <> None then
        named_thread cr
      else
        anonymous_thread cr;
      Cairo.move_to cr ~x:(max visible_x_min (x_of_start t)) ~y:(y_of_thread t);
      Cairo.line_to cr ~x:(min visible_x_max (x_of_end t)) ~y:(y_of_thread t);
      Cairo.stroke cr;
      Thread.creates t |> List.iter (fun child ->
        line cr (Thread.start_time child) t child anonymous_thread
      );
      match Thread.becomes t with
      | None -> ()
      | Some child ->
          line cr (Thread.end_time t) t child anonymous_thread
    );

    top_thread |> Thread.iter (draw_interactions cr);

    visible_threads |> Layout.IT.IntervalSet.iter (fun i ->
      let t = i.Interval_tree.Interval.value in
      let start_x = x_of_start t +. 2. in
      let end_x = x_of_end t in
      if end_x -. start_x > 16. then (
        let msg =
          match Thread.label t with
          | None -> string_of_int (Thread.id t)
          | Some label -> label in
        thread_label cr;
        Cairo.move_to cr ~x:start_x ~y:(y_of_thread t -. 3.);
        Cairo.show_text cr msg
      );

      (* show label on left margin if the thread starts off-screen *)
      if start_x < -32. && end_x >= 32. then (
        let msg =
          match Thread.label t with
          | None -> string_of_int (Thread.id t)
          | Some label -> label in
        thread_label cr;
        Cairo.move_to cr ~x:4. ~y:(y_of_thread t -. 3.);
        Cairo.show_text cr msg
      );
    );

    true
  );

  win#event#connect#delete ==> (fun _ev -> GMain.Main.quit (); true);

  let set_start_time t =
    view_start_time := t
      |> min (trace_end_time -. ((hadjustment#page_size -. margin) /. !scale))
      |> max (!trace_start_time -. (margin /. !scale));
    hadjustment#set_value ((!view_start_time -. !trace_start_time) *. !scale) in

  let set_view_y y =
    view_start_y := y
      |> min (max_y -. (vadjustment#page_size /. !scale))
      |> max 0.0;
    vadjustment#set_value !view_start_y in

  area#misc#set_app_paintable true;
  area#event#add [`SCROLL; `BUTTON1_MOTION; `BUTTON_PRESS];
  area#event#connect#scroll ==> (fun ev ->
    let x = GdkEvent.Scroll.x ev in
    let t_at_pointer = time_of_x x in
    let redraw () =
      grid_step := calc_grid_step !scale;
      let t_new_at_pointer = time_of_x x in
      set_start_time (!view_start_time -. (t_new_at_pointer -. t_at_pointer));
      GtkBase.Widget.queue_draw area#as_widget in
    begin match GdkEvent.Scroll.direction ev with
    | `UP -> scale := !scale *. 1.2; set_scollbars area#misc#allocation; redraw ()
    | `DOWN -> scale := !scale /. 1.2; redraw (); set_scollbars area#misc#allocation
    | _ -> () end;
    true
  );

  let drag_start = ref None in
  area#event#connect#button_press ==> (fun ev ->
    if GdkEvent.Button.button ev = 1 then (
      drag_start := Some (GdkEvent.Button.(time_of_x (x ev), y ev +. !view_start_y));
      true;
    ) else false
  );

  area#event#connect#motion_notify ==> (fun ev ->
    match !drag_start with
    | None -> false
    | Some (start_time, start_y) ->
        let x = GdkEvent.Motion.x ev in
        let y = GdkEvent.Motion.y ev in
        let time_at_pointer = time_of_x x in
        if time_at_pointer <> start_time || start_y <> y then (
          set_start_time (start_time -. (x /. !scale));
          set_view_y (start_y -. y);
          GtkBase.Widget.queue_draw area#as_widget
        );
        true
  );

  hadjustment#connect#value_changed ==> (fun () ->
    set_start_time (!trace_start_time +. (hadjustment#value /. !scale));
    GtkBase.Widget.queue_draw area#as_widget
  );

  vadjustment#connect#value_changed ==> (fun () ->
    set_view_y vadjustment#value;
    GtkBase.Widget.queue_draw area#as_widget
  );

  GMain.Main.main ()
