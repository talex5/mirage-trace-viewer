(* Copyright (C) 2014, Thomas Leonard *)

open Cmdliner

let output_method =
  let docs = "OUTPUT METHODS" in
  let flags =
    Arg.(value @@ vflag `Default [
      `Gtk, info ["gtk"] ~docs ~doc:"Display in a GTK window.";
      `Web, info ["web-server"] ~docs ~doc:"Run a web-server showing the trace.";
    ]) in
  let html_output =
    let doc = "Output files for a static web site." in
    Arg.(value @@ opt (some string) None @@ info ~docs ~doc ~docv:"DIR" ["html"]) in
  let write =
    let doc = "Write out a CTF-format trace file. " in
    Arg.(value @@ opt (some string) None @@ info ~docs ~doc ~docv:"FILE" ["write"]) in

  let choose_output f h w =
    match f, h, w with
    | `Default, Some dir, None -> `Ok (`Html dir)
    | `Default, None, Some path -> `Ok (`Write path)
    | f, None, None -> `Ok f
    | _ -> `Error (true, "Multiple output methods requested!") in

  Term.(ret (pure choose_output $ flags $ html_output $ write))

let parse_trace_filename trace_file =
  match fst Arg.non_dir_file trace_file with
  | `Error _ as e -> e
  | `Ok trace_file ->
  let load () =
    let ch = open_in trace_file in
    let trace =
      if Filename.check_suffix trace_file ".sexp" then Mtv_thread.from_channel ch
      else Mtv_thread.of_events (Mtv_ctf_loader.from_channel ch) in
    close_in ch;
    trace in
  `Ok {Plugin.load; name = trace_file}

let format_source fmt src = Format.pp_print_string fmt src.Plugin.name

let input_file : (_ Arg.converter) = (parse_trace_filename, format_source)

let trace_files =
  let doc = "The CTF-format trace file from which to load the trace data." in
  Arg.(value @@ pos_all input_file [] @@ info ~doc ~docv:"TRACE-FILE" [])

let view_with_gtk source =
  match Plugin.load_output_plugin "mtv-gtk-plugin.cma" with
  | `Ok gtk -> gtk source; `Ok ()
  | `Error msg -> `Error (false, msg)

let view sources = function
  | `Gtk -> view_with_gtk sources
  | `Write _ | `Web -> `Error (false, "Not implemented")  (* TODO *)
  | `Html dir -> Html.write_to dir sources
  | `Default -> view_with_gtk sources  (* TODO *)

let parse_domain_id _domid =
  `Error "TODO"

let xen_domain_id : (_ Arg.converter) = (parse_domain_id, format_source)

let trace_dom =
  let doc = "The Xen domain name or ID from which to load the trace data." in
  Arg.(value @@ opt (some xen_domain_id) None @@ info ~doc ~docv:"DOMID" ["d"; "dom"])

let get_input trace_files domid =
  match trace_files, domid with
  | [], Some dom_source -> `Ok [dom_source]
  | [], None -> `Error (true, "No input source given")
  | trace_files, None -> `Ok trace_files
  | _, Some _ -> `Error (true, "Multiple input sources given!")

let () =
  let doc = "view mirage-profile trace data" in
  let man =[
    `S "DESCRIPTION";
    `P "$(tname) loads trace data from a file or Xen domain and displays or saves it.";
    `P "To view a trace with the GTK graphical user interface:";
    `P "mirage-trace-viewer --gtk trace.ctf";
    `P "To generate HTML and JavaScript files in $(b,htdocs):";
    `P "mirage-trace-viewer --html htdoc trace1.ctf trace2.ctf";
    `P "To view the trace buffer of a Xen guest in a browser:";
    `P "mirage-trace-viewer --web-server --dom mydomain";
  ] in
  let info = Term.info ~doc ~man "mirage-trace-viewer" in
  let input_source = Term.(ret (pure get_input $ trace_files $ trace_dom)) in
  let term = Term.(ret (pure view $ input_source $ output_method)) in
  match Term.eval (term, info) with
  | `Ok () -> ()
  | `Version | `Help -> ()
  | `Error _ -> exit 1
