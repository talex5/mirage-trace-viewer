(* Copyright (C) 2014, Thomas Leonard *)

(** Load a trace and calculate the layout, then dump the results to a file.
 * This is useful to speed up loading in the HTML renderer. *)

let () =
  match Array.to_list Sys.argv with
  | [] | [_] -> failwith "Usage: precompute log.sexp ..."
  | _prog :: args ->
      args |> List.iter (fun name ->
        let vat, stem =
          if Filename.check_suffix name ".sexp" then (
            let ch = open_in name in
            let vat = Thread.from_channel ch in
            close_in ch;
            vat, Filename.chop_suffix name ".sexp"
          ) else if Filename.check_suffix name ".ctf" then (
            let ch = open_in name in
            let events = Ctf_loader.from_channel ch in
            close_in ch;
            Thread.of_events events, Filename.chop_suffix name ".ctf"
          ) else (
            failwith ("Not a .sexp or .ctf file: " ^ name)
          ) in
        let v = View.make ~vat ~view_width:640. ~view_height:480. in
        let ch = open_out (stem ^ ".bin") in
        Marshal.to_channel ch v [];
        close_out ch
      )
