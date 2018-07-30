(*s: common.ml *)
(* Yoann Padioleau
 *
 * Copyright (C) 1998-2009 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)


let rec (do_n: int -> (unit -> unit) -> unit) = fun i f ->
  if i = 0 then () else (f (); do_n (i-1) f)


let command2 s = ignore(Sys.command s)



let _tab_level_print = ref 0

let _prefix_pr = ref ""


let _chan_pr2 = ref (None: out_channel option)

let out_chan_pr2 ?(newline=true) s =
  match !_chan_pr2 with
  | None -> ()
  | Some chan ->
      output_string chan (s ^ (if newline then "\n" else ""));
      flush chan

let pr2 s =
    prerr_string !_prefix_pr;
    do_n !_tab_level_print (fun () -> prerr_string " ");
    prerr_string s;
    prerr_string "\n";
    flush stderr;
    out_chan_pr2 s;
    ()

let pr2_xxxxxxxxxxxxxxxxx () =
  pr2 "-----------------------------------------------------------------------"


let some = function
  | (Some x) -> x
  | _ -> failwith "just: pb"

let optionise f =
  try Some (f ()) with Not_found -> None

let chop = function
  | "" -> ""
  | s -> String.sub s 0 (String.length s - 1)


type filename = string (* TODO could check that exist :) type sux *)

(* tail recursive efficient version *)
let cat file =
  let chan = open_in file in
  let rec cat_aux acc ()  =
      (* cant do input_line chan::aux() cos ocaml eval from right to left ! *)
    let (b, l) = try (true, input_line chan) with End_of_file -> (false, "") in
    if b
    then cat_aux (l::acc) ()
    else acc
  in
  cat_aux [] () |> List.rev |> (fun x -> close_in chan; x)

let cat_array file =
  (""::cat file) |> Array.of_list


let unix_stat file =
  Common.profile_code "Unix.stat" (fun () ->
    Unix.stat file
  )

let filesize file =
  (unix_stat file).Unix.st_size



let rec join_gen a = function
  | [] -> []
  | [x] -> [x]
  | x::xs -> x::a::(join_gen a xs)

let hfind_default key value_if_not_found h =
  try Hashtbl.find h key
  with Not_found ->
    (Hashtbl.add h key (value_if_not_found ()); Hashtbl.find h key)

(* not as easy as Perl  $h->{key}++; but still possible *)
let hupdate_default key ~update:op ~default:value_if_not_found h =
  let old = hfind_default key value_if_not_found h in
  Hashtbl.replace h key (op old)

(* julia: convert something printed using format to print into a string *)
let format_to_string f =
  let (nm,o) = Filename.open_temp_file "format_to_s" ".out" in
  (* to avoid interference with other code using Format.printf, e.g.
   * Ounit.run_tt
   *)
  Format.print_flush();
  Format.set_formatter_out_channel o;
  let _ = f () in
  Format.print_newline();
  Format.print_flush();
  Format.set_formatter_out_channel stdout;
  close_out o;
  let i = open_in nm in
  let lines = ref [] in
  let rec loop _ =
    let cur = input_line i in
    lines := cur :: !lines;
    loop() in
  (try loop() with End_of_file -> ());
  close_in i;
  command2 ("rm -f " ^ nm);
  String.concat "\n" (List.rev !lines)



