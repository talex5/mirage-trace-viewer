(* Copyright (C) 2014, Thomas Leonard *)

(** Load a trace and calculate the layout, then dump the results to a file.
 * This is useful to speed up loading in the HTML renderer. *)

let () =
  match Array.to_list Sys.argv with
  | [] | [_] -> failwith "Usage: precompute log.sexp ..."
  | _prog :: args ->
      args |> List.iter (fun name ->
        if Filename.check_suffix name ".sexp" then (
          let ch = open_in name in
          let vat = Thread.from_channel ch in
          close_in ch;
          let v = View.make ~vat ~view_width:640. ~view_height:480. in
          let ch = open_out (Filename.chop_suffix name ".sexp" ^ ".bin") in
          Marshal.to_channel ch v [];
          close_out ch
        ) else (
          failwith ("Not a .sexp file: " ^ name)
        )
      )
