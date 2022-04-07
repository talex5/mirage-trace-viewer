(* Copyright (C) 2014, Thomas Leonard *)

open Mirage_trace_viewer

let main paths =
  try
    GMain.init () |> ignore;
    paths |> List.iter (fun path ->
        let vat = Mtv_unix.load path |> Mtv_ctf_loader.from_bigarray |> Mtv_thread.of_events in
        let win = Gtk_viewer.make ~name:(path :> string) vat in
        ignore (win#connect#destroy ~callback:(fun _ ->
            (* todo: only quit when last window is closed *)
            GMain.Main.quit ()));
      );
    GMain.Main.main ();
    `Ok ()
  with Gtk.Error msg ->
    `Error (false, msg)

open Cmdliner

let () =
  let doc = "view mirage-profile trace data" in
  let man =[
    `S "DESCRIPTION";
    `P "$(tname) views trace data from a file.";
    `P "mirage-trace-viewer-gtk trace.ctf";
  ] in
  let info = Cmd.info ~doc ~man "mirage-trace-viewer-gtk" in
  let term = Term.(ret (const main $ Mtv_unix.trace_files)) in
  exit @@ Cmd.eval (Cmd.v info term)
