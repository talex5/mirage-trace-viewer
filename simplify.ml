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

(*
let rec shorten = function
  | e1 :: e2 :: rest ->
      begin match e1.Event.op, e2.Event.op with
      | `creates (a, b), `becomes (a', b') when a = a' && b = b' -> {e2 with Event.time = e1.Event.time} :: shorten rest
      | _ -> e1 :: shorten (e2 :: rest) end
  | x -> x
*)

let simplify evs =
  evs |> filter_map (fun ev ->
    let open Event in
    let r = replacement in
    match ev.op with
    | Creates (parent, child) ->
        let a = r parent in
        let b = r child in
        if a <> b then Some {ev with Event.op = Creates (a, b)}
        else None
    | Reads (a, b) ->
        let a = r a in
        let b = r b in
        if a <> b then Some {ev with Event.op = Reads (a, b)}
        else None
    | Resolves (a, b) ->
        let a = r a in
        let b = r b in
        Some {ev with Event.op = Resolves (a, b)}
    | Label (a, msg) ->
        let a = r a in
        Some {ev with Event.op = Label (a, msg)}
    | Becomes (a, b) ->
        let a = r a in
        let b = r b in
        let a, b =
          if a < b then a, b
          else b, a in
        Hashtbl.add becomes a b;
        Some {ev with Event.op = Becomes (a, b)}
  )
