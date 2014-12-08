(* Copyright (C) 2014, Thomas Leonard *)

type input_method = {
  load : unit -> Mtv_thread.vat;
  name : string;
}
(** An input_method provides a way to load a trace. [load] may be called more than once
    if the refresh feature is used. *)

type output_plugin = input_method list -> unit
(** An output plugin can display or output trace data from a source. *)

val register_output : output_plugin -> unit
(** A plugin adding an output/display should call this when loaded. *)

val load_output_plugin : string -> [`Ok of output_plugin | `Error of string]
