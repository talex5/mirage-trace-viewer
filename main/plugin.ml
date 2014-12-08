(* Copyright (C) 2014, Thomas Leonard *)

type input_method = {
  load : unit -> Mtv_thread.vat;
  name : string;
}
type output_plugin = input_method list -> unit

let output_plugin = ref None

let register_output p = output_plugin := Some p

let is_directory path =
  try Sys.is_directory path
  with Sys_error _ -> false

let load_output_plugin name =
  let (/) = Filename.concat in
  let plugins_dir = Filename.dirname Sys.executable_name / ".." / "lib" / "mirage-trace-viewer" in

  (* To allow testing locally. *)
  let plugins_dir =
    if is_directory plugins_dir then plugins_dir
    else Filename.dirname Sys.executable_name / ".." / "gtk" in

  output_plugin := None;
  try
    Dynlink.allow_unsafe_modules true;
    Dynlink.(plugins_dir / adapt_filename name |> loadfile);
    match !output_plugin with
    | None -> failwith "BUG: Plugin failed to register!"
    | Some p -> `Ok p
  with
  | Dynlink.Error e ->
      `Error (Printf.sprintf "Error loading plugin '%s': %s" name (Dynlink.error_message e))
  | ex ->
      `Error (Printf.sprintf "Error loading plugin '%s': %s" name (Printexc.to_string ex))
