(* Copyright (C) 2014, Thomas Leonard *)

val attach : Dom_html.canvasElement Js.t -> Mtv_view.t -> unit
(** [attach canvas view] renders the view to the canvas and attaches event handlers
 * to respond to events. *)

val load : ?file:string -> ?range:(float * float) -> string -> unit
(** [load name] loads "/static/<name>.bin" and attaches it to the canvas element with ID [name].
 * @param file can be used to override the file name
 * @param range can be used to set the default timespan to display *)
