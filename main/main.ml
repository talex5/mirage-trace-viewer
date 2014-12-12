(* Copyright (C) 2014, Thomas Leonard *)

open Cmdliner

let ( >>= ) x fn =
  match x with
  | `Error _ as e -> e
  | `Ok x -> fn x

let output_method =
  let docs = "OUTPUT METHODS" in
  let flags =
    Arg.(value @@ vflag `Default [
      `Gtk, info ["gtk"] ~docs ~doc:"Display in a GTK window.";
(*       `Web, info ["web-server"] ~docs ~doc:"Run a web-server showing the trace."; *)
    ]) in
  let html_output =
    let doc = "Output files for a static web site." in
    Arg.(value @@ opt (some string) None @@ info ~docs ~doc ~docv:"DIR" ["html"]) in
  let write =
    let doc = "Write out a CTF-format trace file. " in
    Arg.(value @@ opt (some string) None @@ info ~docs ~doc ~docv:"FILE" ["w"; "write"]) in

  let choose_output f h w =
    match f, h, w with
    | `Default, Some dir, None -> `Ok (`Html dir)
    | `Default, None, Some path -> `Ok (`Write path)
    | f, None, None -> `Ok f
    | _ -> `Error (true, "Multiple output methods requested!") in

  Term.(ret (pure choose_output $ flags $ html_output $ write))

let copy src dst =
  let len = 4096 in
  let buf = String.make len ' ' in
  let rec aux () =
    match input src buf 0 len with
    | 0 -> ()
    | n -> output dst buf 0 n; aux () in
  aux ()

let open_trace_file = function
  | "-" ->
      let tmp = Filename.temp_file "mtv-" ".ctf" in
      let fd = Unix.(openfile tmp [O_RDWR] 0) in
      let fd2 = Unix.(openfile tmp [O_RDONLY] 0) in
      Unix.unlink tmp;
      let ch = Unix.out_channel_of_descr fd in
      copy stdin ch;
      close_out ch;
      fd2
  | trace_file ->
      Unix.(openfile trace_file [O_RDONLY] 0)

let parse_trace_filename trace_file =
  let trace_file =
    match trace_file with
    | "-" -> `Ok "-"
    | trace_file -> fst Arg.non_dir_file trace_file in
  trace_file >>= fun trace_file ->
  let load () =
    let open Bigarray in
    let fd = open_trace_file trace_file in
    let size = Unix.((fstat fd).st_size) in
    let ba = Array1.map_file fd char c_layout false size in
    Unix.close fd;
    ba in
  `Ok {Plugin.load; name = trace_file}

let format_source fmt src = Format.pp_print_string fmt src.Plugin.name

let input_file : (_ Arg.converter) = (parse_trace_filename, format_source)

let trace_files =
  let doc = "The CTF-format trace file from which to load the trace data." in
  Arg.(value @@ pos_all input_file [] @@ info ~doc ~docv:"TRACE-FILE" [])

let view_with_gtk source =
  match Plugin.load_output_plugin "gtk/mtv-gtk-plugin.cma" with
  | `Error msg -> `Error (false, msg)
  | `Ok gtk ->
      match gtk source with
      | `Ok () as ok -> ok
      | `Error msg -> `Error (false, msg)

let save_as path sources =
  let open Bigarray in
  match sources with
  | [source] ->
      let src = source.Plugin.load () in
      begin match path with
      | "-" ->
          for i = 0 to Array1.dim src - 1 do
            output_char stdout (Array1.get src i)
          done
      | path ->
          let fd = Unix.(openfile path [O_RDWR; O_CREAT; O_TRUNC; O_CLOEXEC] 0o644) in
          let size = Array1.dim src in
          Unix.ftruncate fd size;
          let dst = Array1.map_file fd char c_layout true size in
          Array1.blit src dst;
          Unix.close fd end;
      `Ok ()
  | _ -> `Error (true, "Save only works with a single input source")

let view sources = function
  | `Gtk -> view_with_gtk sources
  | `Write path -> save_as path sources
(*   | `Web -> `Error (false, "Not implemented")  (* TODO *) *)
  | `Html dir -> Html.write_to dir sources
  | `Default ->
      match view_with_gtk sources with
      | `Ok () as ok -> ok
      | `Error (_, msg) -> 
          `Error (false, Printf.sprintf "GTK plugin failed: %s\nHint: try --html=dir instead." msg)

let parse_domain_id dom_name =
  Plugin.load_gnttab_plugin "xen/mtv-xen-plugin.cma" >>= fun (module G : Plugin.GNTTAB) ->
  Xen_trace_source.domid_of_string dom_name >>= fun domid ->
  Xen_trace_source.connect domid >>= fun source ->
  let load () =
    let open Bigarray in
    let ba = Array1.create char c_layout (Xen_trace_source.size source) in
    Xen_trace_source.load (module G) source ba;
    ba in
  `Ok { Plugin.load; name = dom_name }

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
    `P "To display a trace from a remote Xen guest:";
    `P "ssh xen-dom0 mirage-trace-viewer -d my-xen-guest -w - | mirage-trace-viewer -";
(*
    `P "To view the trace buffer of a Xen guest in a browser:";
    `P "mirage-trace-viewer --web-server --dom mydomain";
*)
  ] in
  let info = Term.info ~doc ~man "mirage-trace-viewer" in
  let input_source = Term.(ret (pure get_input $ trace_files $ trace_dom)) in
  let term = Term.(ret (pure view $ input_source $ output_method)) in
  match Term.eval (term, info) with
  | `Ok () -> ()
  | `Version | `Help -> ()
  | `Error _ -> exit 1
