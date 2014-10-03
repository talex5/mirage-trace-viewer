module Thread = struct
  type t = {
    tid : Lwt.thread_id;
    start_time : float;
    mutable end_time : float;
    mutable y : float;
    mutable prev : t option;
    mutable next : t option;
    mutable children' : t list;
  }

  let threads : (Event.thread, t) Hashtbl.t = Hashtbl.create 10

  let top_thread = {
    tid = Lwt.current_id ();
    start_time = 0.0;
    end_time = 0.0;
    y = 0.0;
    prev = None;
    next = None;
    children' = [];
  }

  let create ~parent start_time tid =
    let t = {
      tid;
      start_time;
      end_time = 0.0;
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

let arrange () =
  let open Thread in
  let add_interval _tid t acc =
    { Interval_tree.Interval.
      lbound = t.start_time;
      rbound = t.end_time;
      value = t
    } :: acc in
  let intervals = Hashtbl.fold add_interval Thread.threads [] in
  let layout = IT.create intervals in
  let rec process t =
    let overlaps = IT.overlapping_interval layout (t.start_time, t.end_time) in
    let y =
      try (IT.IntervalSet.max_elt overlaps).Interval_tree.Interval.value.y +. 40.
      with Not_found -> 10. in
    t.y <- y;
    t.children' |> List.iter process in
  top_thread.children' |> List.iter process

let x_of_time t = 20. +. t *. 100.

let arrow_width = 4.
let arrow_height = 10.

let thin   cr = Cairo.set_line_width cr 2.0
let green  cr = thin cr; Cairo.set_source_rgb cr ~r:0.0 ~g:0.5 ~b:0.0
let blue   cr = thin cr; Cairo.set_source_rgb cr ~r:0.0 ~g:0.0 ~b:1.0

let thread_label cr =
  Cairo.set_source_rgb cr ~r:0.8 ~g:0.2 ~b:0.2

let thread cr =
  Cairo.set_line_width cr 4.0;
  Cairo.set_source_rgb cr ~r:0.2 ~g:0.2 ~b:0.2

let get_thread time tid =
  try Hashtbl.find Thread.threads tid
  with Not_found ->
    Thread.create ~parent:Thread.top_thread time tid

let extend _cr _t _time =
  ()

let min_time = ref 0.0
let stagger = 0.0

let line cr time src recv colour =
  let src = get_thread time src in
  let recv = get_thread time recv in
  colour cr;
  Cairo.move_to cr ~x:(x_of_time time) ~y:src.Thread.y;
  Cairo.line_to cr ~x:(x_of_time time) ~y:recv.Thread.y;
  Cairo.stroke cr

let arrow cr src src_time recv recv_time arrow_colour =
  let src = get_thread src_time src in
  let recv = get_thread recv_time recv in

  let src_y =
    if (src.Thread.tid :> int) = 0 then recv.Thread.y +. 20. else src.Thread.y in

  arrow_colour cr;
  Cairo.move_to cr ~x:(x_of_time src_time) ~y:src_y;
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

let is_label ev =
  match ev.Event.op with
  | Event.Label _ -> true
  | _ -> false

let render events path =
  let open Event in
  let surface = Cairo.Image.(create RGB24 ~width:900 ~height:600) in
  let cr = Cairo.create surface in

  Cairo.set_source_rgb cr ~r:0.9 ~g:0.9 ~b:0.9;
  Cairo.paint cr;

  Cairo.set_font_size cr 20.;
  Cairo.select_font_face cr "Sans";

  Cairo.set_line_width cr 2.0;
  Cairo.set_source_rgb cr ~r:1. ~g:1. ~b:1.;
  Cairo.set_line_join cr Cairo.JOIN_BEVEL;

  events |> List.iter (fun ev ->
    let time = ev.time in
    match ev.op with
    | Creates (parent, child) ->
        (* Printf.printf "%a creates %a at %.1f\n" fmt parent fmt child time; *)
        let p = get_thread time parent in
        Thread.create ~parent:p time child |> ignore
    | Resolves (_a, b) -> (get_thread time b).Thread.end_time <- time
    | Becomes (a, _b) -> (get_thread time a).Thread.end_time <- time
    | Label _ | Reads _ -> ()
  );

  arrange ();

  thread cr;
  Thread.iter_threads (fun t ->
    Cairo.move_to cr ~x:(x_of_time t.Thread.start_time) ~y:t.Thread.y;
    Cairo.line_to cr ~x:(x_of_time t.Thread.end_time) ~y:t.Thread.y;
    Cairo.stroke cr;
  );

  events |> List.iter (fun ev ->
    let time = max !min_time ev.time in
    if not (is_label ev) then min_time := time +. stagger;
    match ev.op with
    | Creates (parent, child) -> line cr time parent child thread
    | Reads (a, b) ->
        let end_time = (get_thread time b).Thread.end_time in
        arrow cr b end_time a time blue
    | Resolves (a, b) -> arrow cr a time b time green
    | Becomes (a, b) ->
        Printf.printf "%a becomes %a\n" fmt a fmt b;
        line cr time a b thread
    | Label _ -> ()
  );

  thread_label cr;
  Thread.iter_threads (fun t ->
    Cairo.move_to cr ~x:(x_of_time t.Thread.start_time -. 15.) ~y:(t.Thread.y +. 5.);
    Cairo.show_text cr (string_of_int (t.Thread.tid :> int));
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

  Cairo.PNG.write surface path
