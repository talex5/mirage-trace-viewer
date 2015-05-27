(* Copyright (C) 2014, Thomas Leonard *)

(** Load a trace from another process's trace buffer (in a shared memory region). *)

open Bigarray

module Xs = Xs_client_unix.Client(Xs_transport_unix_client)

type log_buffer = (char, int8_unsigned_elt, c_layout) Array1.t

type t = {
  dom : int;
  refs : int list;
}

type domid = int

let domid_of_string s =
  try `Ok (int_of_string s)
  with _ ->
    let c = Xs.make () in
    let vms = Xs.(immediate c (fun h -> directory h "/local/domain")) in
    let rec aux = function
      | [] -> `Error (Printf.sprintf "Domain '%s' not found" s)
      | x::_ when Xs.(immediate c (fun h -> read h (Printf.sprintf "/local/domain/%s/name" x))) = s ->
          `Ok (int_of_string x)
      | _::xs -> aux xs in
    aux vms

let connect dom =
  let c = Xs.make () in
  let domainpath = Xs.(immediate c (fun h -> getdomainpath h dom)) in
  let xs_path = Printf.sprintf "%s/data/mprof/ring-ref" domainpath in
  try
    let refs =
      Xs.(immediate c (fun h -> read h xs_path))
      |> Str.split (Str.regexp_string ",")
      |> List.map int_of_string in
    `Ok { dom; refs }
  with Xs_protocol.Enoent "read" ->
    `Error (Printf.sprintf "No path %s found in XenStore - domain not configured for tracing?" xs_path)

let page_size = 4096

let size t =
  page_size * List.length t.refs

let load (module G : Plugin.GNTTAB) t ba =
  let pos = ref 0 in
  let iface = G.interface_open () in
  t.refs |> List.iter (fun ref ->
    let mapping = G.(map_exn iface {domid = t.dom; ref} false |> Local_mapping.to_buf) in
    let dst = Array1.sub ba !pos page_size in
    Array1.blit (Cstruct.to_bigarray (Io_page.to_cstruct mapping)) dst;
    pos := !pos + page_size;
  )
