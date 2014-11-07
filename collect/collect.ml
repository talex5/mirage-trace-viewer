(* Copyright (C) 2014, Thomas Leonard *)

(** Save a trace from another process's trace buffer (in a shared memory region). *)

module Xs = Xs_client_unix.Client(Xs_transport_unix_client)

let get_refs dom =
  let c = Xs.make () in
  let domainpath = Xs.(immediate c (fun h -> getdomainpath h dom)) in
  let xs_path = Printf.sprintf "%s/data/mprof" domainpath in
  Xs.(immediate c (fun h -> read h (xs_path ^ "/ring-ref")))
  |> Str.split (Str.regexp_string ",")
  |> List.map int_of_string

let page_size = 4096

let main domid path =
  let open Bigarray in

  let refs = get_refs domid in

  let size = page_size * List.length refs in
  let fd = Unix.(openfile path [O_RDWR; O_CREAT; O_TRUNC; O_CLOEXEC] 0o644) in
  Unix.ftruncate fd size;
  let ba = Array1.map_file fd char c_layout true size in
  Unix.close fd;

  let pos = ref 0 in

  let iface = Gnt.Gnttab.interface_open () in
  refs |> List.iter (fun ref ->
    let mapping = Gnt.Gnttab.(map_exn iface {domid; ref} false |> Local_mapping.to_buf) in
    let dst = Array1.sub ba !pos page_size in
    Array1.blit mapping dst;
    pos := !pos + page_size;
  );

  Printf.printf "Wrote %s\n" path

let () =
  match Array.to_list Sys.argv with
  | [_prog; domID; path] ->
      let domID = int_of_string domID in
      main domID path
  | _ -> failwith "Usage: mirage-trace-collect dom-id output-file"
