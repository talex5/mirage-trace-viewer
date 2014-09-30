type thread = {
  mutable x : float;
  mutable y : float;
}

let x_of_time t = 20. +. t *. 100.

let arrow_width = 10.
let arrow_height = 20.

let threads = Hashtbl.create 10

let get_thread time tid =
  try Hashtbl.find threads tid
  with Not_found ->
    let t = {
      x = x_of_time time;
      y = 10.0 +. float_of_int tid *. 50.;
    } in
    Hashtbl.add threads tid t;
    t

let extend cr t time =
  let old_x = t.x in
  Cairo.move_to cr ~x:old_x ~y:t.y;
  t.x <- x_of_time time;
  Cairo.line_to cr ~x:t.x ~y:t.y

let arrow cr time src recv =
  let src = get_thread time src in
  let recv = get_thread time recv in
  extend cr src time;
  extend cr recv time;
  Cairo.move_to cr ~x:src.x ~y:src.y;
  let arrow_head_y =
    if src.y < recv.y then recv.y -. arrow_height
    else recv.y +. arrow_height in
  Cairo.line_to cr ~x:recv.x ~y:arrow_head_y;
  Cairo.line_to cr ~x:(recv.x +. arrow_width) ~y:arrow_head_y;
  Cairo.line_to cr ~x:recv.x ~y:recv.y;
  Cairo.line_to cr ~x:(recv.x -. arrow_width) ~y:arrow_head_y;
  Cairo.line_to cr ~x:recv.x ~y:arrow_head_y

let render events path =
  let surface = Cairo.Image.(create RGB24 ~width:900 ~height:600) in
  let cr = Cairo.create surface in

  Cairo.set_font_size cr 24.;
  Cairo.select_font_face cr "Sans" ~weight:Cairo.Bold;

  Cairo.set_line_width cr 4.0;
  Cairo.set_source_rgb cr ~r:1. ~g:1. ~b:1.;
  Cairo.set_line_join cr Cairo.JOIN_BEVEL;

  events |> List.iter (fun ev ->
    let open Event in
    match ev.op with
    | `creates (parent, child) -> arrow cr ev.time parent child
    | `notifies (sender, recv) -> arrow cr ev.time sender recv
  );

  Cairo.stroke cr;

  Cairo.PNG.write surface path
