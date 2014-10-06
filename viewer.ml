let () =
  let trace_file =
    match Sys.argv with
    | [| _prog |] -> "log.sexp"
    | [| _prog; path |] -> path
    | _ -> assert false in
  let ch = open_in trace_file in
  let events = Sexplib.Sexp.input_sexps ch in
  close_in ch;
  let events = List.map Event.t_of_sexp events in

  Render.render (Simplify.simplify events)
