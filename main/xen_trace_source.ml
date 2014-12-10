(* Copyright (C) 2014, Thomas Leonard *)

(** Load a trace from another process's trace buffer (in a shared memory region). *)

open Bigarray

module Xs = Xs_client_unix.Client(Xs_transport_unix_client)

type log_buffer = (char, int8_unsigned_elt, c_layout) Array1.t

type t = {
  dom : int;
  refs : int list;
}

let connect dom =
  let dom = int_of_string dom in
  let c = Xs.make () in
  let domainpath = Xs.(immediate c (fun h -> getdomainpath h dom)) in
  let xs_path = Printf.sprintf "%s/data/mprof" domainpath in
  let refs =
    Xs.(immediate c (fun h -> read h (xs_path ^ "/ring-ref")))
    |> Str.split (Str.regexp_string ",")
    |> List.map int_of_string in
  `Ok { dom; refs }

let page_size = 4096

let size t =
  page_size * List.length t.refs

let load (module G : Plugin.GNTTAB) t ba =
  let pos = ref 0 in
  let iface = G.interface_open () in
  t.refs |> List.iter (fun ref ->
    let mapping = G.(map_exn iface {domid = t.dom; ref} false |> Local_mapping.to_buf) in
    let dst = Array1.sub ba !pos page_size in
    Array1.blit mapping dst;
    pos := !pos + page_size;
  )
