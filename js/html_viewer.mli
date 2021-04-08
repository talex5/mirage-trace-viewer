(* Copyright (C) 2014, Thomas Leonard *)

open Mirage_trace_viewer
open Js_of_ocaml

val attach : ?grab_focus:bool -> Dom_html.canvasElement Js.t -> Mtv_view.t -> unit
(** [attach canvas view] renders the view to the canvas and attaches event handlers
    to respond to events. *)

val load : ?grab_focus:bool -> ?file:string -> ?metrics:string list -> ?range:(float * float) -> string -> unit
(** [load name] loads "/static/<name>.bin" and attaches it to the canvas element with ID [name].
    @param grab_focus gives the keyboard focus to the new element after creating it
    @param file can be used to override the file name
    @param metrics limits the displayed metrics to those in the list
    @param range can be used to set the default timespan to display *)
