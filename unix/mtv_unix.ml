type source = string

let copy src dst =
  let len = 4096 in
  let buf = Bytes.make len ' ' in
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

let load trace_file =
  let open Bigarray in
  let fd = open_trace_file trace_file in
  let size = Unix.((fstat fd).st_size) in
  let ba =
    Unix.map_file fd char c_layout false [| size |]
    |> Bigarray.array1_of_genarray
  in
  Unix.close fd;
  ba

open Cmdliner

let parse_trace_filename trace_file =
  match trace_file with
  | "-" -> `Ok "-"
  | trace_file -> fst Arg.non_dir_file trace_file

let input_file : (_ Arg.conv) = (parse_trace_filename, Format.pp_print_string)

let trace_files =
  let doc = "The CTF-format trace file from which to load the trace data." in
  Arg.(non_empty @@ pos_all input_file [] @@ info ~doc ~docv:"TRACE-FILE" [])
