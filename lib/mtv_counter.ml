(* Copyright (C) 2014, Thomas Leonard *)

type time = float

type scale = {
  mutable min : float;
  mutable max : float;
}

(** Measures some user-defined quantity (e.g. packets sent over time). *)
type t = {
  name : string;
  values : (time * float) array;  (* Sorted by time *)
  scale : scale;
  mutable shown : bool;
}
