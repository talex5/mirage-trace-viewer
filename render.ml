let (==>) (signal:(callback:_ -> GtkSignal.id)) callback =
  ignore (signal ~callback)

let margin = 20.

module Thread = struct
  type t = {
    tid : Event.thread;
    start_time : float;
    mutable end_time : float;
    mutable y : float;
    mutable prev : t option;
    mutable next : t option;
    mutable children' : t list;
    mutable becomes : Event.thread option;
  }

  let threads : (Event.thread, t) Hashtbl.t = Hashtbl.create 10

  let top_thread = {
    tid = -1;
    start_time = 0.0;
    end_time = 0.0;
    y = 0.0;
    prev = None;
    next = None;
    children' = [];
    becomes = None;
  }

  let create ~parent start_time end_time tid =
    let t = {
      tid;
      start_time;
      end_time;
      y = 0.0;
      prev = Some parent;
      next = parent.next;
      children' = [];
      becomes = None;
    } in
    parent.next <- Some t;
    parent.children' <- t :: parent.children';
    begin match t.next with
    | Some next -> next.prev <- Some t
    | None -> () end;
    Hashtbl.add threads tid t;
    t

  let iter_threads f =
    let rec process = function
      | None -> ()
      | Some node -> f node; process node.next in
    process (top_thread.next)

  (* Sort by y first, so we can quickly find the lowest *)
  let compare a b =
    match compare a.y b.y with
    | 0 -> compare a.tid b.tid
    | r -> r
end

module IT = ITree.Make(Thread)

exception Found_gap

let arrange () =
  let max_y = ref 0. in
  let open Thread in
  let add_interval _tid t acc =
    assert (t.end_time >= t.start_time);
    { Interval_tree.Interval.
      lbound = t.start_time;
      rbound = t.end_time;
      value = t
    } :: acc in
  let intervals = Hashtbl.fold add_interval Thread.threads [] in
  let layout = IT.create intervals in
  let rec process t ~parent =
    let overlaps = IT.overlapping_interval layout (t.start_time, t.end_time) in
    let p_interval = {Interval_tree.Interval.lbound = parent.start_time; rbound = parent.end_time; value = parent} in
    let _, overlap_parent, below_parent = overlaps |> IT.IntervalSet.split p_interval in
    let y = ref parent.y in
    if overlap_parent && parent.becomes <> Some t.tid then y := !y +. 30.;

    begin try
      below_parent |> IT.IntervalSet.iter (fun i ->
        let iy = i.Interval_tree.Interval.value.y in
        if iy = !y then y := !y +. 30.
        else if iy > !y then raise Found_gap
      );
    with Found_gap -> () end;

    t.y <- !y;
    max_y := max !max_y t.y;
    t.children' |> List.iter (process ~parent:t) in
  top_thread.children' |> List.iter (process ~parent:top_thread);
  layout, !max_y

let calc_grid_step scale =
  let l = 2.5 -. (log scale /. log 10.) |> floor in
  10. ** l

let scale = ref 1000.
let trace_start_time = ref 0.0
let view_start_time = ref 0.0
let x_of_time t = (t -. !view_start_time)  *. !scale
let time_of_x x = (x /. !scale) +. !view_start_time

let clip_x_of_time t =
  x_of_time t
  |> min 1_000_000.
  |> max (-1_000_000.)

let grid_step = ref (calc_grid_step !scale)

let () = Printf.printf "grid_step = %.2f\n%!" !grid_step

let view_start_y = ref 0.0
let y_of_thread t = t.Thread.y -. !view_start_y

let arrow_width = 4.
let arrow_height = 10.

let thin   cr = Cairo.set_line_width cr 1.0

let thread_label cr =
  Cairo.set_source_rgb cr ~r:0.8 ~g:0.2 ~b:0.2

let anonymous_thread cr =
  Cairo.set_line_width cr 2.0;
  Cairo.set_source_rgb cr ~r:0.6 ~g:0.6 ~b:0.6

let named_thread cr =
  Cairo.set_line_width cr 2.0;
  Cairo.set_source_rgb cr ~r:0.2 ~g:0.2 ~b:0.2

let get_thread tid =
  try Hashtbl.find Thread.threads tid
  with Not_found ->
    Thread.top_thread

let extend _cr _t _time =
  ()

let line cr time src recv colour =
  let src = get_thread src in
  let recv = get_thread recv in
  colour cr;
  Cairo.move_to cr ~x:(x_of_time time) ~y:(y_of_thread src);
  Cairo.line_to cr ~x:(x_of_time time) ~y:(y_of_thread recv);
  Cairo.stroke cr

let arrow cr src src_time recv recv_time (r, g, b) =
  let width = (recv_time -. src_time) *. !scale in
  let alpha = 1.0 -. (min 1.0 (width /. 6000.)) in
  if alpha > 0.01 then (
    Cairo.set_source_rgba cr ~r ~g ~b ~a:alpha;

    let src = get_thread src in
    let recv = get_thread recv in

    if src.Thread.tid <> -1  && src.Thread.tid <> recv.Thread.tid then (
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

let is_label ev =
  match ev.Event.op with
  | Event.Label _ -> true
  | _ -> false

let labels = Hashtbl.create 1000

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
    else Printf.sprintf "Each grid division: %.2g ns" (!grid_step *. 1_000_000_000.) in
  Cairo.show_text cr msg

let render events =
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
  let open Event in
  let trace_end_time = (List.nth events (List.length events - 1)).time in
  trace_start_time := (List.hd events).time;
  view_start_time := !trace_start_time -. (margin /. !scale);

  events |> List.iter (fun ev ->
    let time = ev.time in
    match ev.op with
    | Creates (parent, child) ->
        (* Printf.printf "%a creates %a at %.1f\n" fmt parent fmt child time; *)
        let p = get_thread parent in
        Thread.create ~parent:p time trace_end_time child |> ignore
    | Resolves (_a, b) -> (get_thread b).Thread.end_time <- time
    | Becomes (a, b) ->
        let a = get_thread a in
        a.Thread.end_time <- time;
        a.Thread.becomes <- Some b;
    | Reads _ -> ()
    | Label (a, msg) -> Hashtbl.add labels a msg
  );

  let layout, max_y = arrange () in

  let set_scollbars alloc =
    let max_x = (trace_end_time -. !trace_start_time) *. !scale in
    hadjustment#set_bounds ~lower:(-.margin) ~upper:(max_x +. margin) ~page_size:(float_of_int alloc.Gtk.width) ();
    vadjustment#set_bounds ~lower:(-.margin) ~upper:(max_y +. margin) ~page_size:(float_of_int alloc.Gtk.height) () in

  area#misc#connect#size_allocate ==> (fun alloc -> set_scollbars alloc);

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
    let visible_threads = IT.overlapping_interval layout (visible_t_min, visible_t_max) in
    visible_threads |> IT.IntervalSet.iter (fun i ->
      let t = i.Interval_tree.Interval.value in
      if Hashtbl.mem labels t.Thread.tid then
        named_thread cr
      else
        anonymous_thread cr;
      Cairo.move_to cr ~x:(max visible_x_min (x_of_time t.Thread.start_time)) ~y:(y_of_thread t);
      Cairo.line_to cr ~x:(min visible_x_max (x_of_time t.Thread.end_time)) ~y:(y_of_thread t);
      Cairo.stroke cr;
    );

    events |> List.iter (fun ev ->
      let time = ev.time in
      match ev.op with
      | Creates (parent, child) -> line cr time parent child anonymous_thread
      | Reads (a, b) ->
          let end_time = (get_thread b).Thread.end_time in
          thin cr;
          arrow cr b end_time a time (0.0, 0.0, 1.0)
      | Resolves (a, b) ->
          if a <> -1 then (
            let start_time = time
              |> min (get_thread a).Thread.end_time in
            arrow cr a start_time b time (0.0, 0.5, 0.0)
          )
      | Becomes (a, b) ->
          line cr time a b anonymous_thread
      | Label _ -> ()
    );

    thread_label cr;
    visible_threads |> IT.IntervalSet.iter (fun i ->
      let t = i.Interval_tree.Interval.value in
      let start_x = x_of_time t.Thread.start_time +. 2. in
      let end_x = x_of_time t.Thread.end_time in
      if end_x -. start_x > 16. then (
        let msg =
          try Hashtbl.find labels t.Thread.tid
          with Not_found -> string_of_int (t.Thread.tid :> int) in
        Cairo.move_to cr ~x:start_x ~y:(y_of_thread t -. 3.);
        Cairo.show_text cr msg
      )
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
