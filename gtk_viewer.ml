(* Copyright (C) 2014, Thomas Leonard *)

let (==>) (signal:(callback:_ -> GtkSignal.id)) callback =
  ignore (signal ~callback)

module Canvas = struct
  include Cairo

  let paint_text cr ?clip_area ~x ~y msg =
    match clip_area with
    | None ->
        move_to cr ~x ~y;
        show_text cr msg
    | Some (w, h) ->
        save cr;
        rectangle cr ~x ~y:0.0 ~w ~h;
        clip cr;
        move_to cr ~x ~y;
        show_text cr msg;
        restore cr
end

module R = Render.Make(Canvas)

let make top_thread =
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

  win#event#connect#delete ==> (fun _ev -> GMain.Main.quit (); true);
  win#show ();

  let alloc = area#misc#allocation in
  let v = View.make ~top_thread
    ~view_width:(float_of_int alloc.Gtk.width)
    ~view_height:(float_of_int alloc.Gtk.height) in

  let set_scollbars () =
    let (xlo, xhi, xsize), (ylo, yhi, ysize) = View.scroll_bounds v in
    hadjustment#set_bounds ~lower:xlo ~upper:xhi ~page_size:xsize ();
    vadjustment#set_bounds ~lower:ylo ~upper:yhi ~page_size:ysize () in

  area#misc#connect#size_allocate ==> (fun alloc ->
    View.set_size v (float_of_int alloc.Gtk.width) (float_of_int alloc.Gtk.height);
    set_scollbars ()
  );

  area#event#connect#expose ==> (fun ev ->
    let cr = Cairo_gtk.create area#misc#window in
    Cairo.set_font_size cr 12.;
    Cairo.select_font_face cr "Sans";
    Cairo.set_line_join cr Cairo.JOIN_BEVEL;

    let expose_area = GdkEvent.Expose.area ev in
    let x, y = Gdk.Rectangle.(x expose_area, y expose_area) in
    let width, height = Gdk.Rectangle.(width expose_area, height expose_area) in
    R.render v cr ~expose_area:(
      (float_of_int x, float_of_int y),
      (float_of_int (x + width), float_of_int (y + height))
    );
    true
  );

  let set_start_time t =
    View.set_start_time v t
    |> hadjustment#set_value in

  let set_view_y y =
    View.set_view_y v y
    |> vadjustment#set_value in

  area#misc#set_app_paintable true;
  area#event#add [`SCROLL; `BUTTON1_MOTION; `BUTTON_PRESS];
  area#event#connect#scroll ==> (fun ev ->
    let x = GdkEvent.Scroll.x ev in
    let t_at_pointer = View.time_of_x v x in
    let redraw () =
      let t_new_at_pointer = View.time_of_x v x in
      set_start_time (v.View.view_start_time -. (t_new_at_pointer -. t_at_pointer));
      GtkBase.Widget.queue_draw area#as_widget in
    begin match GdkEvent.Scroll.direction ev with
    | `UP -> View.set_scale v (v.View.scale *. 1.2); set_scollbars (); redraw ()
    | `DOWN -> View.set_scale v (v.View.scale /. 1.2); redraw (); set_scollbars ()
    | _ -> () end;
    true
  );

  let drag_start = ref None in
  area#event#connect#button_press ==> (fun ev ->
    if GdkEvent.Button.button ev = 1 then (
      drag_start := Some (GdkEvent.Button.(View.time_of_x v (x ev), y ev +. v.View.view_start_y));
      true;
    ) else false
  );

  area#event#connect#motion_notify ==> (fun ev ->
    match !drag_start with
    | None -> false
    | Some (start_time, start_y) ->
        let x = GdkEvent.Motion.x ev in
        let y = GdkEvent.Motion.y ev in
        let time_at_pointer = View.time_of_x v x in
        if time_at_pointer <> start_time || start_y <> y then (
          set_start_time (start_time -. View.timespan_of_width v x);
          set_view_y (start_y -. y);
          GtkBase.Widget.queue_draw area#as_widget
        );
        true
  );

  hadjustment#connect#value_changed ==> (fun () ->
    set_start_time (Thread.start_time top_thread +. (hadjustment#value /. v.View.scale));
    GtkBase.Widget.queue_draw area#as_widget
  );

  vadjustment#connect#value_changed ==> (fun () ->
    set_view_y vadjustment#value;
    GtkBase.Widget.queue_draw area#as_widget
  )

let () =
  let trace_file =
    match Sys.argv with
    | [| _prog |] -> "log.sexp"
    | [| _prog; path |] -> path
    | _ -> assert false in
  let ch = open_in trace_file in
  let trace = Thread.from_channel ch in
  close_in ch;

  make trace;

  GMain.Main.main ()
