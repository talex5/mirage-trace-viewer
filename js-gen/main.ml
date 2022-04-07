(* Copyright (C) 2014, Thomas Leonard *)

open Mirage_trace_viewer

let main dir paths =
  let sources =
    paths |> List.map (fun path ->
        let data = Mtv_unix.load path |> Mtv_ctf_loader.from_bigarray |> Mtv_thread.of_events in
        (path :> string), data
      )
  in
  Html.write_to dir sources

open Cmdliner

let html_output =
  let doc = "Output directory." in
  Arg.(required @@ opt (some string) None @@ info ~doc ~docv:"DIR" ["out"])

let () =
  let doc = "view mirage-profile trace data" in
  let man =[
    `S "DESCRIPTION";
    `P "To generate HTML and JavaScript files in $(b,htdocs):";
    `P "mirage-trace-viewer-js --out=htdocs trace1.ctf trace2.ctf";
  ] in
  let info = Cmd.info ~doc ~man "mirage-trace-viewer-js" in
  let term = Term.(ret (const main $ html_output $ Mtv_unix.trace_files)) in
  exit @@ Cmd.eval (Cmd.v info term)
