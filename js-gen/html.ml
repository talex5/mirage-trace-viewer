(* Copyright (C) 2014, Thomas Leonard *)

open Mirage_trace_viewer

let ( / ) = Filename.concat

let error fmt =
  let do_raise msg = `Error (false, msg) in
  Printf.ksprintf do_raise fmt

let check_exit_status = function
  | Unix.WEXITED 0 -> `Ok ()
  | Unix.WEXITED code -> error "Child returned error exit status %d" code
  | Unix.WSIGNALED signal -> error "Child aborted (signal %d)" signal
  | Unix.WSTOPPED signal -> error "Child is currently stopped (signal %d)" signal

(* From Unix.ml (not exported) *)
let rec waitpid_non_intr pid =
  try Unix.waitpid [] pid
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_non_intr pid

let reap_child child_pid =
  check_exit_status @@ snd @@ waitpid_non_intr child_pid

let ( >>= ) x fn =
  match x with
  | `Error _ as e -> e
  | `Ok x -> fn x

let rec iter_s f = function
  | [] -> `Ok ()
  | x::xs -> f x >>= fun () -> iter_s f xs

let finally_do cleanup f resource =
  let result =
    try f resource
    with ex -> cleanup resource; raise ex in
  cleanup resource;
  result

let write_if_missing dir (name, contents) =
  let path = dir / name in
  if Sys.file_exists path then `Ok ()
  else (
    let ch =
      try `Ok (open_out_gen [Open_creat; Open_binary; Open_wronly] 0o644 path)
      with Sys_error msg -> error "Open failed: %s" msg in
    ch >>= finally_do close_out (fun ch ->
        output_string ch contents;
        `Ok ()
      )
  )

let html : (_, _, _) format = "\
<!DOCTYPE html>\n\
<html lang='en'>\n\
<head>\n\
  <meta charset='utf-8'>\n\
  <meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1, user-scalable=0'>\n\
  <script type='text/javascript' defer='defer' src='loader.js'></script>\n\
  <style type='text/css'>\n\
    body { margin: 0; padding: 0; border: 0; }\n\
    canvas { position: absolute; left: 0; top: 0; }\n\
    div.side-panel {\n\
      z-index: 80;\n\
      margin: 0;\n\
      padding: 0;\n\
      border-right: 1px solid black;\n\
      position: absolute; left: 0; top: 0; height: 100%%; background: rgba(220, 220, 220, 0.9);\n\
      display: flex;\n\
    }\n\
    div.side-panel > div {\n\
      margin: 0.5em;\n\
      display: flex;\n\
      flex-direction: column;\n\
    }\n\
    ul.metrics {\n\
      list-style: none;\n\
      padding-left: 0;\n\
      overflow-y: scroll;\n\
    }\n\
  </style>\n\
  <title>Mirage Trace Toolkit</title>\n\
</head>\n\
<body>\n\
  <canvas id='%s' style='width: 100%%; height:100%%' tabindex='1'>\n\
  <noscript>Sorry, you need to enable JavaScript to see this page.</noscript>\n\
  </canvas>\n\
</body>\n\
</html>"

let run argv =
  print_endline (String.concat " " argv);
  Unix.create_process (List.hd argv) (Array.of_list argv) Unix.stdin Unix.stdout Unix.stderr
  |> reap_child

let write_to dir sources =
  if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
  if not (Sys.is_directory dir) then `Error (false, "Not a directory: " ^ dir)
  else (
    let loads = ref [] in
    let bin_args =
      sources |> List.mapi (fun i (name, vat) ->
          let name = Filename.basename name in
          let v = Mtv_view.make ~vat ~view_width:640. ~view_height:480. in
          let bin_file = name ^ ".bin" in
          let ch = open_out (dir / bin_file) in
          Marshal.to_channel ch v [];
          close_out ch;
          let focus =
            if i = 0 then "~grab_focus:true " else "" in
          loads := Printf.sprintf "Html_viewer.load %s%S;" focus name :: !loads;
          ["--file"; String.escaped bin_file]
        )
      |> List.concat in
    let skeleton_files = [
      "dune-project", "(lang dune 2.8)";
      "dune",
      Printf.sprintf
        {|(executable
            (name loader)
            (modes byte)
            (libraries mirage-trace-viewer-js.runtime))
         |};
      "Makefile",
      Printf.sprintf
        "all:\
         \n\tdune build --profile=release ./loader.bc\
         \n\tjs_of_ocaml --opt=3 _build/default/loader.bc -I . %s -o loader.js" (String.concat " " bin_args);
      "loader.ml",
      "let () =\n  " ^ String.concat "\n  " !loads ^ "\n";
      "trace.html",
      Printf.sprintf html (Filename.basename (fst (List.hd sources)))
    ] in
    skeleton_files |> iter_s (write_if_missing dir) >>= fun () ->
    run ["make"; "-C"; dir]
  )
