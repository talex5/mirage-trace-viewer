(* Copyright (C) 2014, Thomas Leonard *)

open Bigarray

type log_buffer = (char, int8_unsigned_elt, c_layout) Array1.t

module type GNTTAB = sig
  module Local_mapping : sig
    type t
    val to_buf : t -> log_buffer
  end
  type interface
  type grant = {
    domid: int;
    ref: int;
  }
  val interface_open : unit -> interface
  val map_exn : interface -> grant -> bool -> Local_mapping.t
end
type gnttab_plugin = (module GNTTAB)

type input_method = {
  load : unit -> log_buffer;
  name : string;
}
type output_plugin = input_method list -> [`Ok of unit | `Error of string]

let gnttab_plugin = ref None
let output_plugin = ref None

let register_output p = output_plugin := Some p
let register_gnttab p = gnttab_plugin := Some p

let is_directory path =
  try Sys.is_directory path
  with Sys_error _ -> false

let load_plugin r name =
  let (/) = Filename.concat in
  let plugins_dir = Filename.dirname Sys.executable_name / ".." / "lib" / "mirage-trace-viewer" in

  (* To allow testing locally. *)
  let plugin_path =
    if is_directory plugins_dir then plugins_dir / Filename.basename name
    else Filename.dirname Sys.executable_name / ".." / name in

  r := None;
  try
    Dynlink.allow_unsafe_modules true;
    Dynlink.(adapt_filename plugin_path |> loadfile);
    match !r with
    | None -> failwith "BUG: Plugin failed to register!"
    | Some p -> `Ok p
  with
  | Dynlink.Error e ->
      `Error (Printf.sprintf "Error loading plugin '%s': %s" name (Dynlink.error_message e))
  | ex ->
      `Error (Printf.sprintf "Error loading plugin '%s': %s" name (Printexc.to_string ex))

let load_output_plugin = load_plugin output_plugin
let load_gnttab_plugin = load_plugin gnttab_plugin

let load input_method =
  input_method.load () |> Mtv_ctf_loader.from_bigarray
