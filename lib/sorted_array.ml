(* The number of items at the start of the array for which [pred item] is false.
 * The array must be sorted so that the first part is all false and the second all true. *)
let count_before pred arr =
  let rec loop lo hi =
    (* Answer is at least [lo] and at most [hi]. *)
    if hi = lo then lo
    else (
      let mid = (lo + hi) / 2 in
      if pred arr.(mid) then loop lo mid
      else loop (mid + 1) hi
    ) in
  loop 0 (Array.length arr)

(* Call [f i] on each item in the sorted array where [test_start i] is
 * true until [test_end i] is false (binary search). *)
let iter_range arr test_start test_end f =
  let l = Array.length arr in
  let first = arr |> count_before (fun i2 -> test_start i2) in
  let rec aux i =
    if i = l then ()
    else (
      let i2 = arr.(i) in
      if test_end i2 then (f i2; aux (i + 1))
      else ()
    ) in
  aux first
