let (==>) (signal:(callback:_ -> GtkSignal.id)) callback =
  ignore (signal ~callback)

module Thread = struct
  type t = {
    tid : Event.thread;
    start_time : float;
    mutable end_time : float;
    mutable y : float;
    mutable prev : t option;
    mutable next : t option;
    mutable children' : t list;
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
    let _, overlap_parent, below_parent = overlaps |> IT.IntervalSet.split {Interval_tree.Interval.lbound = 0.; rbound = 0.; value = parent} in
    let y = ref parent.y in
    if overlap_parent then y := !y +. 30.;

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

let scale = ref 1000.
let trace_start_time = ref 0.0
let view_start_time = ref 0.0
let x_of_time t = 20. +. (t -. !view_start_time)  *. !scale
let time_of_x x = ((x -. 20.) /. !scale) +. !view_start_time

let arrow_width = 4.
let arrow_height = 10.

let thin   cr = Cairo.set_line_width cr 1.0
let green  cr = thin cr; Cairo.set_source_rgb cr ~r:0.0 ~g:0.5 ~b:0.0

let thread_label cr =
  Cairo.set_source_rgb cr ~r:0.8 ~g:0.2 ~b:0.2

let thread cr =
  Cairo.set_line_width cr 2.0;
  Cairo.set_source_rgb cr ~r:0.2 ~g:0.2 ~b:0.2

let get_thread _time tid =
  try Hashtbl.find Thread.threads tid
  with Not_found ->
    Thread.top_thread

let extend _cr _t _time =
  ()

let line cr time src recv colour =
  let src = get_thread time src in
  let recv = get_thread time recv in
  colour cr;
  Cairo.move_to cr ~x:(x_of_time time) ~y:src.Thread.y;
  Cairo.line_to cr ~x:(x_of_time time) ~y:recv.Thread.y;
  Cairo.stroke cr

let arrow cr src src_time recv recv_time =
  let src = get_thread src_time src in
  let recv = get_thread recv_time recv in

  if src.Thread.tid <> -1  && src.Thread.tid <> recv.Thread.tid then (
    let src_x = x_of_time src_time in
    let src_y = src.Thread.y in

    Cairo.move_to cr ~x:src_x ~y:src_y;
    let arrow_head_y =
      if src_y < recv.Thread.y then recv.Thread.y -. arrow_height
      else recv.Thread.y +. arrow_height in
    let x = x_of_time recv_time in
    Cairo.line_to cr ~x ~y:arrow_head_y;
    Cairo.line_to cr ~x:(x +. arrow_width) ~y:arrow_head_y;
    Cairo.line_to cr ~x ~y:recv.Thread.y;
    Cairo.line_to cr ~x:(x -. arrow_width) ~y:arrow_head_y;
    Cairo.line_to cr ~x ~y:arrow_head_y;
    Cairo.stroke_preserve cr;
    Cairo.fill cr
  )

let is_label ev =
  match ev.Event.op with
  | Event.Label _ -> true
  | _ -> false

let render events =
  GMain.init () |> ignore;
  let win = GWindow.window ~title:"Mirage Trace Toolkit" () in
  let swin = GBin.scrolled_window
    ~packing:win#add
    ~hpolicy:`NEVER
    () in
  let area = GMisc.drawing_area ~packing:swin#add_with_viewport () in
  win#show ();
  let open Event in
  let trace_end_time = (List.nth events (List.length events - 1)).time in
  trace_start_time := (List.hd events).time;
  view_start_time := !trace_start_time;

  events |> List.iter (fun ev ->
    let time = ev.time in
    match ev.op with
    | Creates (parent, child) ->
        (* Printf.printf "%a creates %a at %.1f\n" fmt parent fmt child time; *)
        let p = get_thread time parent in
        Thread.create ~parent:p time trace_end_time child |> ignore
    | Resolves (_a, b) -> (get_thread time b).Thread.end_time <- time
    | Becomes (a, _b) -> (get_thread time a).Thread.end_time <- time
    | Label _ | Reads _ -> ()
  );

  let layout, max_y = arrange () in
  area#misc#set_size_request ~height:(max_y +. 20. |> truncate) ();

  area#event#connect#expose ==> (fun ev ->
    let cr = Cairo_gtk.create area#misc#window in

    Cairo.set_source_rgb cr ~r:0.9 ~g:0.9 ~b:0.9;
    Cairo.paint cr;

    Cairo.set_font_size cr 12.;
    Cairo.select_font_face cr "Sans";

    Cairo.set_line_width cr 2.0;
    Cairo.set_source_rgb cr ~r:1. ~g:1. ~b:1.;
    Cairo.set_line_join cr Cairo.JOIN_BEVEL;

    let expose_area = GdkEvent.Expose.area ev in

    thread cr;
    let visible_x_min = float_of_int (Gdk.Rectangle.x expose_area) in
    let visible_x_max = float_of_int (Gdk.Rectangle.(x expose_area + width expose_area)) in
    let visible_t_min = time_of_x visible_x_min in
    let visible_t_max = time_of_x visible_x_max in
    let visible_threads = IT.overlapping_interval layout (visible_t_min, visible_t_max) in
    visible_threads |> IT.IntervalSet.iter (fun i ->
      let t = i.Interval_tree.Interval.value in
      Cairo.move_to cr ~x:(max visible_x_min (x_of_time t.Thread.start_time)) ~y:t.Thread.y;
      Cairo.line_to cr ~x:(min visible_x_max (x_of_time t.Thread.end_time)) ~y:t.Thread.y;
      Cairo.stroke cr;
    );

    events |> List.iter (fun ev ->
      let time = ev.time in
      match ev.op with
      | Creates (parent, child) -> line cr time parent child thread
      | Reads (a, b) ->
          let end_time = (get_thread time b).Thread.end_time in
          thin cr;
          let alpha = 1.0 -. (min 1.0 ((x_of_time time -. x_of_time end_time) /. 6000.)) in
          Cairo.set_source_rgba cr ~r:0.0 ~g:0.0 ~b:1.0 ~a:alpha;
          arrow cr b end_time a time
      | Resolves (a, b) ->
          if a <> -1 then (
            let end_time = (get_thread time a).Thread.end_time in
            green cr;
            arrow cr a (min end_time time) b time
          )
      | Becomes (a, b) ->
          line cr time a b thread
      | Label _ -> ()
    );

    thread_label cr;
    visible_threads |> IT.IntervalSet.iter (fun i ->
      let t = i.Interval_tree.Interval.value in
      let start_x = x_of_time t.Thread.start_time +. 2. in
      let end_x = x_of_time t.Thread.end_time in
      if end_x -. start_x > 16. then (
        Cairo.move_to cr ~x:start_x ~y:(t.Thread.y -. 3.);
        Cairo.show_text cr (string_of_int (t.Thread.tid :> int));
      )
    );

    events |> List.iter (fun ev ->
      let time = ev.time in
      match ev.op with
      | Label (a, msg) ->
          let a = get_thread time a in
          thread_label cr;
          Cairo.move_to cr ~x:(x_of_time time) ~y:(a.Thread.y -. 5.);
          Cairo.show_text cr msg
      | _ -> ()
    );

    true
  );

  win#event#connect#delete ==> (fun _ev -> GMain.Main.quit (); true);

  area#misc#set_app_paintable true;
  area#event#add [`SCROLL];
  area#event#connect#scroll ==> (fun ev ->
    let x = GdkEvent.Scroll.x ev in
    let t_at_pointer = time_of_x x in
    let redraw () =
      let t_new_at_pointer = time_of_x x in
      view_start_time :=
        min
          trace_end_time
          (max
            !trace_start_time
            (!view_start_time -. (t_new_at_pointer -. t_at_pointer)));
      GtkBase.Widget.queue_draw area#as_widget in
    begin match GdkEvent.Scroll.direction ev with
    | `UP -> scale := !scale *. 1.2; redraw ()
    | `DOWN -> scale := !scale /. 1.2; redraw ()
    | _ -> () end;
    true
  );

  GMain.Main.main ()
