(** If one thread becomes another, then it didn't do anything interesting before that, so
 * just replace all references to it with the final thread.
 * Note: this does mean that we might "create" a thread that already exists. *)
let becomes : (Event.thread, Event.thread) Hashtbl.t = Hashtbl.create 10

let rec filter_map f = function
  | [] -> []
  | x::xs ->
      match f x with
      | Some y -> y :: filter_map f xs
      | None -> filter_map f xs

let rec replacement thread =
  try
    let r = Hashtbl.find becomes thread in
    (* Printf.printf "replacement for %d is %d\n" thread r; *)
    let r' = replacement r in
    if r <> r' then Hashtbl.replace becomes thread r';
    r'
  with Not_found -> thread

let simplify evs =
  evs |> filter_map (fun ev ->
    let r = replacement in
    match ev.Event.op with
    | `creates (parent, child) ->
        let a = r parent in
        let b = r child in
        if a <> b then Some {ev with Event.op = `creates (a, b)}
        else None
    | `reads (a, b) ->
        let a = r a in
        let b = r b in
        if a <> b then Some {ev with Event.op = `reads (a, b)}
        else None
    | `resolves (a, b) ->
        let a = r a in
        let b = r b in
        if a <> b then Some {ev with Event.op = `resolves (a, b)}
        else None
    | `becomes (a, b) ->
        let a = r a in
        let b = r b in
        let a, b =
          if a < b then a, b
          else b, a in
        Hashtbl.add becomes a b;
        Some {ev with Event.op = `becomes (a, b)}
  )
