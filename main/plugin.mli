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

type input_method = {
  load : unit -> log_buffer;  (** [load] may be called more than once if the refresh feature is used. *)
  name : string;
}
(** An input_method provides a way to load a trace. *)

type gnttab_plugin = (module GNTTAB)

type output_plugin = input_method list -> [`Ok of unit | `Error of string]
(** An output plugin can display or output trace data from a source. *)

val register_gnttab : gnttab_plugin -> unit
(** xen-gnttab.unix adds runtime dependencies, so we load it as a plugin. *)

val register_output : output_plugin -> unit
(** A plugin adding an output/display should call this when loaded. *)

val load_gnttab_plugin : string -> [`Ok of gnttab_plugin | `Error of string]

val load_output_plugin : string -> [`Ok of output_plugin | `Error of string]

val load : input_method -> Mtv_event.t list
