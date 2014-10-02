type thread = {
  mutable x : float;
  mutable y : float;
}

let x_of_time t = 20. +. t *. 100.

let arrow_width = 4.
let arrow_height = 10.

let thin   cr = Cairo.set_line_width cr 2.0
let green  cr = thin cr; Cairo.set_source_rgb cr ~r:0.0 ~g:1.0 ~b:0.0
let blue   cr = thin cr; Cairo.set_source_rgb cr ~r:0.0 ~g:0.0 ~b:1.0
let yellow cr = thin cr; Cairo.set_source_rgb cr ~r:1.0 ~g:1.0 ~b:0.0
let label  cr = Cairo.set_source_rgb cr ~r:1.0 ~g:1.0 ~b:1.0

let thread cr =
  Cairo.set_line_width cr 4.0;
  Cairo.set_source_rgb cr ~r:0.8 ~g:0.8 ~b:0.8

let threads = Hashtbl.create 10

let get_thread time tid =
  try Hashtbl.find threads tid
  with Not_found ->
    let t = {
      x = x_of_time time;
      y = 10.0 +. float_of_int (tid : Lwt.thread_id :> int) *. 50.;
    } in
    Hashtbl.add threads tid t;
    t

let extend cr t time =
  let old_x = t.x in
  Cairo.move_to cr ~x:old_x ~y:t.y;
  t.x <- x_of_time time;
  Cairo.line_to cr ~x:t.x ~y:t.y

let min_time = ref 0.0
let stagger = 0.1

let extend cr time src recv =
  thread cr;
  extend cr src time;
  extend cr recv time;
  Cairo.stroke cr

let line cr time src recv colour =
  let src = get_thread time src in
  let recv = get_thread time recv in
  extend cr time src recv;

  colour cr;
  Cairo.move_to cr ~x:src.x ~y:src.y;
  Cairo.line_to cr ~x:recv.x ~y:recv.y;
  Cairo.stroke cr

let arrow cr time src recv arrow_colour =
  let src = get_thread time src in
  let recv = get_thread time recv in
  extend cr time src recv;

  arrow_colour cr;
  Cairo.move_to cr ~x:src.x ~y:src.y;
  let arrow_head_y =
    if src.y < recv.y then recv.y -. arrow_height
    else recv.y +. arrow_height in
  Cairo.line_to cr ~x:recv.x ~y:arrow_head_y;
  Cairo.line_to cr ~x:(recv.x +. arrow_width) ~y:arrow_head_y;
  Cairo.line_to cr ~x:recv.x ~y:recv.y;
  Cairo.line_to cr ~x:(recv.x -. arrow_width) ~y:arrow_head_y;
  Cairo.line_to cr ~x:recv.x ~y:arrow_head_y;
  Cairo.stroke_preserve cr;
  Cairo.fill cr

let render events path =
  let surface = Cairo.Image.(create RGB24 ~width:900 ~height:600) in
  let cr = Cairo.create surface in

  Cairo.set_font_size cr 20.;
  Cairo.select_font_face cr "Sans";

  Cairo.set_line_width cr 2.0;
  Cairo.set_source_rgb cr ~r:1. ~g:1. ~b:1.;
  Cairo.set_line_join cr Cairo.JOIN_BEVEL;

  events |> List.iter (fun ev ->
    let time = max !min_time ev.Event.time in
    min_time := time +. stagger;
    let open Event in
    match ev.op with
    | `creates (parent, child) -> line cr time parent child yellow
    | `reads (a, b) -> arrow cr time b a blue
    | `resolves (a, b) -> arrow cr time a b green
    | `becomes (a, b) -> line cr time a b thread
    | `label (a, msg) ->
        let a = get_thread time a in
        label cr;
        Cairo.move_to cr ~x:(x_of_time time) ~y:(a.y -. 5.);
        Cairo.show_text cr msg
  );

  Cairo.PNG.write surface path
