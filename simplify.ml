let becomes : (Event.thread, (Event.thread * float)) Hashtbl.t = Hashtbl.create 10

let rec filter_map f = function
  | [] -> []
  | x::xs ->
      match f x with
      | Some y -> y :: filter_map f xs
      | None -> filter_map f xs

let rec replacement thread time =
  try
    let r, r_time = Hashtbl.find becomes thread in
    if r_time > time then thread    (* not yet *)
    else replacement r time
  with Not_found -> thread

let simplify evs =
  let open Event in
  evs |> List.iter (fun ev ->
    match ev.Event.op with
    | `becomes (old, replacement) ->
        assert (not (Hashtbl.mem becomes old));
        Hashtbl.add becomes old (replacement, ev.time);
    | _ -> ()
  );
  evs |> filter_map (fun ev ->
    let r t = replacement t ev.time in
    match ev.op with
    | `creates (parent, child) ->
        let a = r parent in
        let b = r child in
        if a <> b then Some ({ev with op = `creates (a, b)})
        else None
    | `notifies (sender, recv) ->
        let a = r sender in
        let b = r recv in
        if a <> b then Some ({ev with op = `notifies (a, b)})
        else None
    | `becomes _ -> None
  )
