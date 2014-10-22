module Make(Value : Set.OrderedType) = struct

  module Interval = struct
    open Interval_tree.Interval

    type t = Value.t Interval_tree.Interval.t

    let compare a b =
      match Value.compare a.value b.value with
      | 0 ->
          begin match compare a.lbound b.lbound with
          | 0 -> compare a.rbound b.rbound
          | r -> r end
      | r -> r
  end

  module IntervalSet = Set.Make(Interval)

  type 'a tree = {
    by_start : Interval.t array;
    by_end : Interval.t array;
    by_point : Value.t Interval_tree.t;
  }

  let compare_by_start a b =
    let open Interval_tree.Interval in
    compare a.lbound b.lbound

  let compare_by_end a b =
    let open Interval_tree.Interval in
    compare a.rbound b.rbound

  let create intervals =
    let by_start = Array.of_list intervals in
    Array.sort compare_by_start by_start;
    let by_end = Array.copy by_start in
    Array.sort compare_by_end by_end;
    {
      by_start;
      by_end;
      by_point = Interval_tree.create intervals;
    }

  (* Extend [acc] with intervals from the sorted array [intervals] from the
   * index where [test_start interval] is true until [test_end interval] is false. *)
  let add_range intervals test_start test_end acc =
    let l = Array.length intervals in
    let first = intervals |> Sorted_array.count_before (fun i2 -> test_start i2) in
    let rec collect acc i =
      if i = l then acc
      else (
        let i2 = intervals.(i) in
        if test_end i2 then collect (IntervalSet.add i2 acc) (i + 1)
        else acc
      ) in
    collect acc first

  let overlapping_interval t (lbound, rbound) =
    (* Intervals overlapping lbound..!rbound are those which:
     * - straddle the whole interval OR
     * - start inside the interval OR
     * - end inside the interval *)

    (* Everything that straddles the interval will also straddle its lower bound.
     * This will also include intervals that end inside the interval, but we want those anyway. *)
    let straddling_start =
      Interval_tree.query t.by_point lbound
      |> List.fold_left (fun acc i ->
          if i.Interval_tree.Interval.rbound > lbound then   (* (right bound is exclusive) *)
            IntervalSet.add i acc
          else
            acc
        ) IntervalSet.empty in

    let open Interval_tree.Interval in
    straddling_start
    |> add_range t.by_start (fun i -> lbound <= i.lbound) (fun i -> i.lbound < rbound)
    |> add_range t.by_end   (fun i -> lbound < i.rbound) (fun i -> i.rbound <= rbound)
end
