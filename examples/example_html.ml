(* Copyright (C) 2014, Thomas Leonard *)

let arg name =
  let qname = "?" ^ name in
  let _, v = Url.Current.arguments |> List.find (fun (k, _v) -> k = name || k = qname) in
  v

let v =
  let ch = open_in "/static/log-x86.bin" in
  let v = Marshal.from_channel ch in
  close_in ch;
  begin try
    let t_min = arg "t_min" |> float_of_string in
    let t_max = arg "t_max" |> float_of_string in
    let scale = (View.view_width v -. View.h_margin *. 2.) /. (t_max -. t_min) in
    View.set_scale v scale;
    View.set_start_time v t_min |> ignore
  with Not_found -> () end;
  v

let () =
  let c =
    match Dom_html.tagged (Dom_html.getElementById "canvas") with
    | Dom_html.Canvas c -> c
    | _ -> assert false in
  Html_viewer.attach c v
