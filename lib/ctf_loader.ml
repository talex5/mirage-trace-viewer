(* Copyright (C) 2014, Thomas Leonard *)

open Event
open Bigarray

type log_buffer = (char, int8_unsigned_elt, c_layout) Array1.t

let thread_type_of_int = function
  | 0 -> "Wait"
  | 1 -> "Task"
  | 2 -> "Bind"
  | 3 -> "Try"
  | 4 -> "Choose"
  | 5 -> "Pick"
  | 6 -> "Join"
  | 7 -> "Map"
  | 8 -> "Condition"
  | _ -> assert false

let uuid = "\x05\x88\x3b\x8d\x52\x1a\x48\x7b\xb3\x97\x45\x6a\xb1\x50\x68\x0c"

let from_channel ch =
  let fd = Unix.descr_of_in_channel ch in
  let size = Unix.((fstat fd).st_size) in
  let data = Array1.map_file fd char c_layout false size in
  let events = ref [] in
  let pos = ref 0 in
  let packet_end = ref 0 in

  let read64 () =
    let v = EndianBigstring.LittleEndian.get_int64 data !pos in
    pos := !pos + 8;
    v in
  let read32 () =
    let v = EndianBigstring.LittleEndian.get_int32 data !pos in
    pos := !pos + 4;
    v in
  let read8 () =
    let v = EndianBigstring.LittleEndian.get_int8 data !pos in
    pos := !pos + 1;
    v in
  let read_thread () =
    read64 () |> Int64.to_int in    (* FIXME: will fail on 32-bit platforms *)
  let read_string () =
    let b = Buffer.create 10 in
    let rec aux i =
      match EndianBigstring.LittleEndian.get_char data i with
      | '\x00' -> pos := i + 1; Buffer.contents b
      | x -> Buffer.add_char b x; aux (i + 1) in
    aux !pos in

  let read_packet_header () =
    let packet_start = !pos in
    (* Printf.printf "Read header at %d\n" !pos; *)
    if read32 () <> 0xc1fc1fc1l then failwith "Not a CTF log packet (bad magic)";
    for i = 0 to 15 do
      if Array1.get data (!pos + i) <> uuid.[i] then failwith "Packet UUID doesn't match!"
    done;
    pos := !pos + 16;
    packet_end := packet_start + Int32.to_int (read32 ()) / 8 in

  while !pos < Array1.dim data do
    read_packet_header ();

    while !pos < !packet_end do
      let time = read64 () in
      let op =
        match read8 () with
        | 0 ->
            let parent = read_thread () in
            let child = read_thread () in
            let thread_type = read8 () in
            Creates (parent, child, thread_type_of_int thread_type)
        | 1 ->
            let reader = read_thread () in
            let input = read_thread () in
            Reads (reader, input)
        | 2 ->
            let resolver = read_thread () in
            let thread = read_thread () in
            Resolves (resolver, thread, None)
        | 3 ->
            let resolver = read_thread () in
            let thread = read_thread () in
            let ex = read_string () in
            Resolves (resolver, thread, Some ex)
        | 4 ->
            let bind = read_thread () in
            let thread = read_thread () in
            Becomes (bind, thread)
        | 5 ->
            let thread = read_thread () in
            let label = read_string () in
            Label (thread, label)
        | 6 ->
            let thread = read_thread () in
            let amount = read64 () |> Int64.to_int in
            let counter = read_string () in
            Increases (thread, counter, amount)
        | 7 ->
            let thread = read_thread () in
            Switch thread
        | 8 ->
            let duration = read64 () in
            Gc (Int64.to_float duration /. 1_000_000_000.)
        | x -> failwith (Printf.sprintf "Unknown event op %d" x) in
      let event = {
        time = Int64.to_float time /. 1_000_000_000.;
        op;
      } in
      events := event :: !events
    done;

    pos := !packet_end
  done;
  List.rev !events
