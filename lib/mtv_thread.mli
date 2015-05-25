(* Copyright (C) 2014, Thomas Leonard *)

(** A single thread/promise. *)
type t

(** A group of threads, cooperatively threaded within the vat. *)
type vat

type time = float
type interaction = Resolve | Read | Try_read | Signal

val top_thread : vat -> t
val gc_periods : vat -> (time * time) list

val thread_type : t -> string

val start_time : t -> time
val end_time : t -> time

(** For simplified binds, we don't show the creating thread (parent might no longer exist). *)
val show_creation : t -> bool

(** Threads created by this one, in *reverse* order. *)
val creates : t -> t list

(** At this thread's end_time, it is not resolved, but merges with another thread. *)
val becomes : t -> t option

val labels : t -> (time * string) list

(** If the thread failed, the string of the exception. *)
val failure : t -> string option

(** Interactions initiated by this thread (reverse order) *)
val interactions : t -> (time * interaction * t) list

(** Return the times when the thread was running. *)
val activations : t -> (time * time) list

(** Does the thread end because it was resolved, or just because we didn't
 * see any more events? *)
val resolved : t -> bool

(** Parse a trace file, returning the root thread. *)
val of_events : ?simplify:bool -> Mtv_event.t list -> vat

val set_y : t -> float -> unit
val y : t -> float

(** Sorts by y first, then by thread ID *)
val compare : t -> t -> int

val id : t -> int

val iter : (t -> unit) -> t -> unit

val counters : vat -> Mtv_counter.t list
