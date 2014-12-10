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

  let set_source_alpha cr ~r ~g ~b a = set_source_rgba cr ~r ~g ~b ~a
end

module R = Mtv_render.Make(Canvas)

let export_as_svg v fname =
  let surface = Cairo.SVG.create
    ~fname
    ~width:(Mtv_view.view_width v)
    ~height:(Mtv_view.view_height v) in
  let cr = Cairo.create surface in
  Cairo.set_font_size cr 12.;
  Cairo.select_font_face cr "Sans";
  Cairo.set_line_join cr Cairo.JOIN_BEVEL;

  (* Note: bounds are slightly smaller than the page because otherwise Cairo
   * optimises the clip region out (but Inkscape displays things beyond the
   * page boundaries). *)
  Cairo.rectangle cr ~x:1.0 ~y:0.0 ~w:(Mtv_view.view_width v -. 2.0) ~h:(Mtv_view.view_height v);
  Cairo.clip cr;

  R.render v cr ~expose_area:(
    (0.0, 0.0),
    (Mtv_view.view_width v, Mtv_view.view_height v)
  );
  Cairo.Surface.finish surface

let show_menu ~parent ~v bev =
  let menu = GMenu.menu () in
  let packing = menu#add in
  let export_svg = GMenu.menu_item ~packing ~label:"Export as SVG..." () in
  export_svg#connect#activate ==> (fun () ->
    let save_box = GWindow.file_chooser_dialog
      ~action:`SAVE
      ~parent
      ~title:"Export as SVG"
      ~position:`MOUSE
      () in
    save_box#add_button_stock `CANCEL `CANCEL;
    save_box#add_select_button "Export" `ACCEPT;
    save_box#connect#response ==> (function
      | `ACCEPT ->
          begin match save_box#filename with
          | Some fname -> export_as_svg v fname; save_box#destroy ()
          | None -> () end;
      | _ -> save_box#destroy ()
    );
    save_box#show ()
  );
  menu#popup ~button:(GdkEvent.Button.button bev) ~time:(GdkEvent.Button.time bev)

let make source =
  let vat = Plugin.load source |> Mtv_thread.of_events in
  let top_thread = Mtv_thread.top_thread vat in
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
  let v = Mtv_view.make ~vat
    ~view_width:(float_of_int alloc.Gtk.width)
    ~view_height:(float_of_int alloc.Gtk.height) in

  let set_scollbars () =
    let (xlo, xhi, xsize, xvalue), (ylo, yhi, ysize, yvalue) = Mtv_view.scroll_bounds v in
    hadjustment#set_bounds ~lower:xlo ~upper:xhi ~page_size:xsize ();
    vadjustment#set_bounds ~lower:ylo ~upper:yhi ~page_size:ysize ();
    hadjustment#set_value xvalue;
    vadjustment#set_value yvalue;
    in

  area#misc#connect#size_allocate ==> (fun alloc ->
    Mtv_view.set_size v (float_of_int alloc.Gtk.width) (float_of_int alloc.Gtk.height);
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
    Mtv_view.set_start_time v t
    |> hadjustment#set_value in

  let set_view_y y =
    Mtv_view.set_view_y v y
    |> vadjustment#set_value in

  area#misc#set_app_paintable true;
  area#event#add [`SCROLL; `BUTTON1_MOTION; `BUTTON_PRESS];
  area#event#connect#scroll ==> (fun ev ->
    let x = GdkEvent.Scroll.x ev in
    let t_at_pointer = Mtv_view.time_of_x v x in
    let redraw () =
      let t_new_at_pointer = Mtv_view.time_of_x v x in
      set_start_time (Mtv_view.view_start_time v -. (t_new_at_pointer -. t_at_pointer));
      GtkBase.Widget.queue_draw area#as_widget in
    begin match GdkEvent.Scroll.direction ev with
    | `UP -> Mtv_view.zoom v 1.2; set_scollbars (); redraw ()
    | `DOWN -> Mtv_view.zoom v (1. /. 1.2); redraw (); set_scollbars ()
    | _ -> () end;
    true
  );

  let drag_start = ref None in
  area#event#connect#button_press ==> (fun ev ->
    match GdkEvent.Button.button ev with
    | 1 ->
        let start_t = Mtv_view.time_of_x v (GdkEvent.Button.x ev) in
        let start_y = Mtv_view.y_of_view_y v (GdkEvent.Button.y ev) in
        drag_start := Some (start_t, start_y);
        true;
    | 3 -> show_menu ~parent:win ~v ev; true
    | _ -> false
  );

  area#event#connect#motion_notify ==> (fun ev ->
    match !drag_start with
    | None -> false
    | Some (start_time, start_y) ->
        let x = GdkEvent.Motion.x ev in
        let y = GdkEvent.Motion.y ev in
        let time_at_pointer = Mtv_view.time_of_x v x in
        let y_at_pointer = Mtv_view.y_of_view_y v y in
        if time_at_pointer <> start_time || start_y <> y_at_pointer then (
          set_start_time (start_time -. Mtv_view.timespan_of_width v x);
          Mtv_view.set_view_y_so v start_y y
          |> vadjustment#set_value;
          GtkBase.Widget.queue_draw area#as_widget
        );
        true
  );

  hadjustment#connect#value_changed ==> (fun () ->
    set_start_time (Mtv_thread.start_time top_thread +. (Mtv_view.timespan_of_width v hadjustment#value));
    GtkBase.Widget.queue_draw area#as_widget
  );

  vadjustment#connect#value_changed ==> (fun () ->
    set_view_y vadjustment#value;
    GtkBase.Widget.queue_draw area#as_widget
  )

let () =
  let run_plugin sources =
    sources |> List.iter make;
    GMain.Main.main () in
  Plugin.register_output run_plugin
