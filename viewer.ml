let () =
  let trace_file =
    match Sys.argv with
    | [| _prog |] -> "log.sexp"
    | [| _prog; path |] -> path
    | _ -> assert false in
  let ch = open_in trace_file in
  let trace = Thread.from_channel ch in
  close_in ch;

  Render.render trace
