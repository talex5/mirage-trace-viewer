(* Copyright (C) 2014, Thomas Leonard *)

(** A single thread/promise. *)
type t

type time = float
type interaction = Resolve | Read

val start_time : t -> time
val end_time : t -> time

(** Threads created by this one, in *reverse* order. *)
val creates : t -> t list

(** At this thread's end_time, it is not resolved, but merges with another thread. *)
val becomes : t -> t option

val label : t -> string option

(** Interactions initiated by this thread (reverse order) *)
val interactions : t -> (time * interaction * t) list

(** Return the times when the thread was running. *)
val activations : t -> (time * time) list

(** Parse a trace file, returning the root thread. *)
val from_channel : in_channel -> t

val set_y : t -> float -> unit
val y : t -> float

(** Sorts by y first, then by thread ID *)
val compare : t -> t -> int

val id : t -> int

val iter : (t -> unit) -> t -> unit
