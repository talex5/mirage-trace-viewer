(* Copyright (C) 2014, Thomas Leonard *)

(** Some values used for calculating vertical positions.
 * Saved so we don't have to regenerate them for every thread. *)
type v_projection

type t = {
  vat : Thread.vat;
  mutable scale : float;
  mutable view_width : float;
  mutable view_height : float;
  mutable view_start_time : float;
  mutable v_projection : v_projection;
  height : float;
  mutable grid_step : float;
  layout : Layout.t;
  arrow_events_by_first : (Thread.t * Thread.time * Thread.interaction * Thread.t * Thread.time) array;
  arrow_events_by_second : (Thread.t * Thread.time * Thread.interaction * Thread.t * Thread.time) array;
}

val h_margin : float
val v_margin : float

val x_of_time : t -> Thread.time -> float
val time_of_x : t -> float -> Thread.time
val clip_x_of_time : t -> Thread.time -> float
val width_of_timespan : t -> Thread.time -> float
val timespan_of_width : t -> float -> Thread.time

val x_of_start : t -> Thread.t -> float
val x_of_end : t -> Thread.t -> float

val y_of_thread : t -> Thread.t -> float

(** Convert a y-position in screen units to thread units (undoing the effect of the projection). *)
val y_of_view_y : t -> float -> float

(** Distance between the focal y and this thread in thread coordinates.
 * This is used to decide whether it's worth rendering text on this thread. *)
val dist_from_focus : t -> Thread.t -> float

val visible_threads : t -> (float * float) -> Layout.IT.IntervalSet.t

val iter_interactions : t -> float -> float -> (Thread.t * Thread.time * Thread.interaction * Thread.t * Thread.time -> unit) -> unit

val make : view_width:float -> view_height:float -> vat:Thread.vat -> t

(** Returns [min, max, size, value] for each scrollbar. *)
val scroll_bounds : t -> (float * float * float * float) * (float * float * float * float)

val set_size : t -> float -> float -> unit

(** Set [view_start_time], within the allowed limits.
 * Returns the new horizontol scrollbar position. *)
val set_start_time : t -> Thread.time -> float

(** Set the focal y. Returns the input value clamped to the acceptable range. *)
val set_view_y : t -> float -> float

(** Set the scale factor for converting times to widths. *)
val set_scale : t -> float -> unit

(** [set_view_y_so v y view_y] sets the focal_y so that [y] appears at [view_y].
 * Returns the new focal y. *)
val set_view_y_so : t -> float -> float -> float
