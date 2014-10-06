let () =
  let ch = open_in "log.sexp" in
  let events = Sexplib.Sexp.input_sexps ch in
  close_in ch;
  let events = List.map Event.t_of_sexp events in

  Render.render (Simplify.simplify events) "graph.png"
