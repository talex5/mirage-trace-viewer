(* Copyright (C) 2014, Thomas Leonard *)

open Gg
open Vg

module Vg_canvas = struct
  type context = {
    mutable image : image;
    mutable path : path;
    mutable colour : Color.t;
    mutable pen_width : float;
  }

  let font = { Font.
    name = "Sans";
    slant = `Normal;
    weight = `W400;
    size = 14.0;
  }

  type text_extents = {
    x_bearing : float; 
    y_bearing : float;
    width : float;
    height : float;
    x_advance : float;
    y_advance : float;
  }

  let create_context () = {
    image = I.void;
    path = P.empty;
    colour = Color.black;
    pen_width = 1.0;
  }

  let set_line_width context width =
    context.pen_width <- width

  let set_source_rgba context ~r ~g ~b ~a =
    context.colour <- Color.v_srgb ~a r g b

  let set_source_rgb context ~r ~g ~b =
    context.colour <- Color.v_srgb r g b

  let move_to context ~x ~y =
    context.path <- context.path |> P.sub (P2.v x y)

  let line_to context ~x ~y =
    context.path <- context.path |> P.line (P2.v x y)

  let rectangle context ~x ~y ~w ~h =
    context.path <- context.path |> P.rect (Box2.v (P2.v x y) (Size2.v w h))

  let stroke_preserve context =
    context.image <- context.image |> I.blend begin
      let area = `O { P.o with P.width = context.pen_width } in
      I.cut ~area context.path (I.const context.colour)
    end

  let stroke context =
    stroke_preserve context;
    context.path <- P.empty

  let fill context =
    context.image <- context.image |> I.blend begin
      I.cut context.path (I.const context.colour)
    end;
    context.path <- P.empty

  let text_extents _context _msg = {
    x_bearing = 0.0;  (* FIXME *)
    y_bearing = 0.0;
    width = 10.0;
    height = 24.0;
    x_advance = 10.0;
    y_advance = 0.0;
  }

  let paint_text context ?clip_area ~x ~y msg =
    context.image <- context.image |> I.blend begin
      let text =
        I.cut_glyphs ~text:msg font [] (I.const context.colour)
        |> I.scale (V2.v 1.0 ~-.1.0)
        |> I.move (V2.v x y) in
      match clip_area with
      | None -> text
      | Some (w, h) ->
          let rect = P.empty |> P.rect (Box2.v (P2.v x y) (V2.v w ~-.h)) in
          text |> I.cut rect
    end

  let paint ?alpha context =
    assert (alpha = None);
    context.image <- I.const (context.colour)
end

module R = Render.Make(Vg_canvas)

let size = Size2.v 200. 200. (* mm *)
let view_size = Size2.v 1000. 1000.
let view = Box2.v (P2.v 0.0 ~-.(V2.y view_size)) view_size

let main _ =
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
  ]) in
  let top_thread = Thread.of_sexp (List.map Event.sexp_of_t events) in
  let v = View.make ~top_thread
    ~view_width:(V2.x view_size)
    ~view_height:(V2.y view_size) in
  let context = Vg_canvas.create_context () in
  R.render v context ~expose_area:((0.0, 0.0), V2.to_tuple view_size);

  let d = Dom_html.window ## document in
  let c =
    let c = Dom_html.createCanvas d in
    Dom.appendChild (d ## body ) c; c
  in
  let r = Vgr.create (Vgr_htmlc.target c) `Other in
  let image = context.Vg_canvas.image |> I.scale (V2.v 1.0 ~-.1.0) in
  ignore (Vgr.render r (`Image (size, view, image)));
  ignore (Vgr.render r `End);
  Js._false

let () = Dom_html.window ## onload <- Dom_html.handler main
