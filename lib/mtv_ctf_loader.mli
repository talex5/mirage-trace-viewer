(* Copyright (C) 2014, Thomas Leonard *)

open Bigarray

type packet
type log_buffer = (char, int8_unsigned_elt, c_layout) Array1.t

(** Locate packets in a trace stream and return them (using [Array1.sub]) in the correct order. *)
val packets : log_buffer -> packet list

val packet_data : packet -> log_buffer

val from_channel : in_channel -> Mtv_event.t list
val from_bigarray : log_buffer -> Mtv_event.t list
