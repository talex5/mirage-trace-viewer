(* Copyright (C) 2014, Thomas Leonard *)

type time = float

(** Measures some user-defined quantity (e.g. packets sent over time). *)
type t = {
  name : string;
  values : (time * float) array;  (* Sorted by time *)
  min : float;
  max : float;
}
