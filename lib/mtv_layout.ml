(* Copyright (C) 2014, Thomas Leonard *)

module IT = Mtv_ITree.Make(Mtv_thread)

type t = IT.Interval.t IT.tree

let fold ~init fn thread =
  let rec aux acc t =
    let acc = fn acc t in
    List.fold_left aux acc (Mtv_thread.creates t) in
  Mtv_thread.creates thread |> List.fold_left aux init

exception Found_gap

let arrange top_thread =
  let max_y = ref 0. in
  let add_interval acc t =
    assert (Mtv_thread.end_time t >= Mtv_thread.start_time t);
    { Interval_tree.Interval.
      lbound = Mtv_thread.start_time t;
      rbound = Mtv_thread.end_time t;
      value = t
    } :: acc in
  let intervals = fold add_interval top_thread ~init:[] in
  let layout = IT.create intervals in
  let rec process t ~parent =
    let overlaps = IT.overlapping_interval layout (Mtv_thread.start_time t, Mtv_thread.end_time t) in
    let p_interval = {Interval_tree.Interval.lbound = Mtv_thread.start_time parent; rbound = Mtv_thread.end_time parent; value = parent} in
    let _, overlap_parent, below_parent = overlaps |> IT.IntervalSet.split p_interval in
    let y = ref (max 0.5 (Mtv_thread.y parent)) in
    begin match Mtv_thread.becomes parent with
    | Some child when child == t -> ()
    | _ -> if overlap_parent && parent != top_thread then y := !y +. 30. end;

    begin try
      below_parent |> IT.IntervalSet.iter (fun i ->
        let iy = Mtv_thread.y i.Interval_tree.Interval.value in
        if iy = !y then y := !y +. 30.
        else if iy > !y then raise Found_gap
      );
    with Found_gap -> () end;

    Mtv_thread.set_y t !y;
    max_y := max !max_y !y;
    Mtv_thread.creates t |> List.iter (process ~parent:t) in
  Mtv_thread.creates top_thread |> List.iter (process ~parent:top_thread);
  layout, !max_y

