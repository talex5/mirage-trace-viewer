(* Copyright (C) 2014, Thomas Leonard *)

module IT = ITree.Make(Thread)

let fold ~init fn thread =
  let rec aux acc t =
    let acc = fn acc t in
    List.fold_left aux acc (Thread.creates t) in
  aux init thread

exception Found_gap

let arrange top_thread =
  let max_y = ref 0. in
  let add_interval acc t =
    assert (Thread.end_time t >= Thread.start_time t);
    { Interval_tree.Interval.
      lbound = Thread.start_time t;
      rbound = Thread.end_time t;
      value = t
    } :: acc in
  let intervals = fold add_interval top_thread ~init:[] in
  let layout = IT.create intervals in
  let rec process t ~parent =
    let overlaps = IT.overlapping_interval layout (Thread.start_time t, Thread.end_time t) in
    let p_interval = {Interval_tree.Interval.lbound = Thread.start_time parent; rbound = Thread.end_time parent; value = parent} in
    let _, overlap_parent, below_parent = overlaps |> IT.IntervalSet.split p_interval in
    let y = ref (Thread.y parent) in
    begin match Thread.becomes parent with
    | Some child when child == t -> ()
    | _ -> if overlap_parent then y := !y +. 30. end;

    begin try
      below_parent |> IT.IntervalSet.iter (fun i ->
        let iy = Thread.y i.Interval_tree.Interval.value in
        if iy = !y then y := !y +. 30.
        else if iy > !y then raise Found_gap
      );
    with Found_gap -> () end;

    Thread.set_y t !y;
    max_y := max !max_y !y;
    Thread.creates t |> List.iter (process ~parent:t) in
  Thread.creates top_thread |> List.iter (process ~parent:top_thread);
  layout, !max_y

