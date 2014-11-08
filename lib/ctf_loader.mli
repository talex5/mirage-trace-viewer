(* Copyright (C) 2014, Thomas Leonard *)

open Bigarray

type log_buffer = (char, int8_unsigned_elt, c_layout) Array1.t

(** Locate packets in a trace stream and return them (using [Array1.sub]) in the correct order. *)
val packets : log_buffer -> log_buffer list

val from_channel : in_channel -> Event.t list
