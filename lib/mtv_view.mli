(* Copyright (C) 2014, Thomas Leonard *)

(** Some values used for calculating vertical positions.
 * Saved so we don't have to regenerate them for every thread. *)
type v_projection

type t

val h_margin : float
val v_margin : float

val x_of_time : t -> Mtv_thread.time -> float
val time_of_x : t -> float -> Mtv_thread.time
val clip_x_of_time : t -> Mtv_thread.time -> float
val width_of_timespan : t -> Mtv_thread.time -> float
val timespan_of_width : t -> float -> Mtv_thread.time

val x_of_start : t -> Mtv_thread.t -> float
val x_of_end : t -> Mtv_thread.t -> float

val y_of_thread : t -> Mtv_thread.t -> float

(** Convert a y-position in screen units to thread units (undoing the effect of the projection). *)
val y_of_view_y : t -> float -> float

(** Convert a y-position in thread units to screen units. *)
val view_y_of_y : t -> float -> float

(** Distance between the focal y and this thread in thread coordinates.
 * This is used to decide whether it's worth rendering text on this thread. *)
val dist_from_focus : t -> Mtv_thread.t -> float

val visible_threads : t -> (float * float) -> Mtv_layout.IT.IntervalSet.t

val iter_interactions : t -> float -> float -> (Mtv_thread.t * Mtv_thread.time * Mtv_thread.interaction * Mtv_thread.t * Mtv_thread.time -> unit) -> unit

val make : view_width:float -> view_height:float -> vat:Mtv_thread.vat -> t

(** Returns [min, max, size, value] for each scrollbar. *)
val scroll_bounds : t -> (float * float * float * float) * (float * float * float * float)

val set_size : t -> float -> float -> unit

(** Set [view_start_time], within the allowed limits.
 * Returns the new horizontol scrollbar position. *)
val set_start_time : t -> Mtv_thread.time -> float

(** Set the focal y. Returns the input value clamped to the acceptable range. *)
val set_view_y : t -> float -> float

(** Set the scale factor for converting times to widths. *)
val set_scale : t -> float -> unit

(** Multiply the scale by the given factor. *)
val zoom : t -> float -> unit

(** [set_view_y_so v y view_y] sets the focal_y so that [y] appears at [view_y].
 * Returns the new focal y. *)
val set_view_y_so : t -> float -> float -> float

(** Timespan between grid lines. *)
val grid_step : t -> float

val vat : t -> Mtv_thread.vat

(** The time corresponding to the left edge of the visible area. *)
val view_start_time :  t -> Mtv_thread.time

(** The width of the viewport, in screen units. *)
val view_width : t -> float

(** The height of the viewport, in screen units. *)
val view_height : t -> float

val clone : t -> t
