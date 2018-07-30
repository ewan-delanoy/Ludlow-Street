(* Yoann Padioleau
 *
 * Copyright (C) 1998-2013 Yoann Padioleau
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
(*###########################################################################*)
(* Prelude *)
(*###########################################################################*)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* The following functions should be in their respective sections but
 * because some functions in some sections use functions in other
 * sections, and because I don't want to take care of the order of
 * those sections, of those dependencies, I put the functions causing
 * dependency problem here. C is better than OCaml on this with the
 * ability to declare prototypes, enabling some form of forward
 * reference.
 *)

 type 'a stack = 'a list (* with sexp *)

let spf = Printf.sprintf

exception Timeout

let push v l =
  l := v :: !l


let debugger = ref false

let unwind_protect f cleanup =
  if !debugger then f () else
    try f ()
    with e -> begin cleanup e; raise e end


let memoized ?(use_cache=true) h k f =
  if not use_cache
  then f ()
  else
    try Hashtbl.find h k
    with Not_found ->
      let v = f () in
      begin
        Hashtbl.add h k v;
        v
      end

exception Todo
exception Impossible


(*###########################################################################*)
(* Basic features *)
(*###########################################################################*)

(*****************************************************************************)
(* Debugging/logging *)
(*****************************************************************************)

let pr s =
  print_string s;
  print_string "\n";
  flush stdout

let pr2 s =
  prerr_string s;
  prerr_string "\n";
  flush stderr


let _already_printed = Hashtbl.create 101
let disable_pr2_once = ref false

let xxx_once f s =
  if !disable_pr2_once then pr2 s
  else
    if not (Hashtbl.mem _already_printed s)
    then begin
      Hashtbl.add _already_printed s true;
      f ("(ONCE) " ^ s);
    end

let pr2_once s = xxx_once pr2 s


(* start of dumper.ml *)

(* Dump an OCaml value into a printable string.
 * By Richard W.M. Jones (rich@annexia.org).
 * dumper.ml 1.2 2005/02/06 12:38:21 rich Exp
 *)
open Printf
open Obj

let rec dump2 r =
  if is_int r then
    string_of_int (magic r : int)
  else (				(* Block. *)
    let rec get_fields acc = function
      | 0 -> acc
      | n -> let n = n-1 in get_fields (field r n :: acc) n
    in
    let rec is_list r =
      if is_int r then (
        if (magic r : int) = 0 then true (* [] *)
        else false
      ) else (
        let s = size r and t = tag r in
        if t = 0 && s = 2 then is_list (field r 1) (* h :: t *)
        else false
      )
    in
    let rec get_list r =
      if is_int r then []
      else let h = field r 0 and t = get_list (field r 1) in h :: t
    in
    let opaque name =
      (* XXX In future, print the address of value 'r'.  Not possible in
       * pure OCaml at the moment.
       *)
      "<" ^ name ^ ">"
    in

    let s = size r and t = tag r in

    (* From the tag, determine the type of block. *)
    if is_list r then ( (* List. *)
      let fields = get_list r in
      "[" ^ String.concat "; " (List.map dump2 fields) ^ "]"
    )
    else if t = 0 then (		(* Tuple, array, record. *)
      let fields = get_fields [] s in
      "(" ^ String.concat ", " (List.map dump2 fields) ^ ")"
    )

    (* Note that [lazy_tag .. forward_tag] are < no_scan_tag.  Not
     * clear if very large constructed values could have the same
     * tag. XXX *)
    else if t = lazy_tag then opaque "lazy"
    else if t = closure_tag then opaque "closure"
    else if t = object_tag then (	(* Object. *)
      let fields = get_fields [] s in
      let clasz, id, slots =
        match fields with h::h'::t -> h, h', t | _ -> assert false in
      (* No information on decoding the class (first field).  So just print
       * out the ID and the slots.
       *)
      "Object #" ^ dump2 id ^
        " (" ^ String.concat ", " (List.map dump2 slots) ^ ")"
    )
    else if t = infix_tag then opaque "infix"
    else if t = forward_tag then opaque "forward"

    else if t < no_scan_tag then (	(* Constructed value. *)
      let fields = get_fields [] s in
      "Tag" ^ string_of_int t ^
        " (" ^ String.concat ", " (List.map dump2 fields) ^ ")"
    )
    else if t = string_tag then (
      "\"" ^ String.escaped (magic r : string) ^ "\""
    )
    else if t = double_tag then (
      string_of_float (magic r : float)
    )
    else if t = abstract_tag then opaque "abstract"
    else if t = custom_tag then opaque "custom"
    else if t = custom_tag then opaque "final"
    else failwith ("dump: impossible tag (" ^ string_of_int t ^ ")")
  )

let dump v = dump2 (repr v)

let pr2_gen x = pr2 (dump x)

(*****************************************************************************)
(* Profiling *)
(*****************************************************************************)

type prof = ProfAll | ProfNone | ProfSome of string list
let profile = ref ProfNone
let show_trace_profile = ref false

let check_profile category =
  match !profile with
  | ProfAll -> true
  | ProfNone -> false
  | ProfSome l -> List.mem category l

let _profile_table = ref (Hashtbl.create 100)

let adjust_profile_entry category difftime =
  let (xtime, xcount) =
    (try Hashtbl.find !_profile_table category
    with Not_found ->
      let xtime = ref 0.0 in
      let xcount = ref 0 in
      Hashtbl.add !_profile_table category (xtime, xcount);
      (xtime, xcount)
    ) in
  xtime := !xtime +. difftime;
  xcount := !xcount + 1;
  ()

let profile_start category = failwith "todo"
let profile_end category = failwith "todo"


(* subtil: don't forget to give all argumens to f, otherwise partial app
 * and will profile nothing.
 *
 * todo: try also detect when complexity augment each time, so can
 * detect the situation for a function gets worse and worse ?
 *)
let profile_code category f =
  if not (check_profile category)
  then f ()
  else begin
  if !show_trace_profile then pr2 (spf "> %s" category);
  let t = Unix.gettimeofday () in
  let res, prefix =
    try Some (f ()), ""
    with Timeout -> None, "*"
  in
  let category = prefix ^ category in (* add a '*' to indicate timeout func *)
  let t' = Unix.gettimeofday () in

  if !show_trace_profile then pr2 (spf "< %s" category);

  adjust_profile_entry category (t' -. t);
  (match res with
  | Some res -> res
  | None -> raise Timeout
  );
  end



(*****************************************************************************)
(* Equality *)
(*****************************************************************************)
let (=|=) : int    -> int    -> bool = (=)

type ('a,'b) either = Left of 'a | Right of 'b
  (* with sexp *)
type ('a, 'b, 'c) either3 = Left3 of 'a | Middle3 of 'b | Right3 of 'c
  (* with sexp *)


let rec filter_some = function
  | [] -> []
  | None :: l -> filter_some l
  | Some e :: l -> e :: filter_some l

let map_filter f xs = xs |> List.map f |> filter_some


let _memo_compiled_regexp = Hashtbl.create 101
let candidate_match_func s re =
  (* old: Str.string_match (Str.regexp re) s 0 *)
  let compile_re =
    memoized _memo_compiled_regexp re (fun () -> Str.regexp re)
  in
  Str.string_match compile_re s 0

let match_func s re =
  profile_code "Common.=~" (fun () -> candidate_match_func s re)

let (=~) s re =match_func s re


let join  sep xs = String.concat sep xs

(*****************************************************************************)
(* Strings *)
(*****************************************************************************)

(* ruby *)
let i_to_s = string_of_int

(*****************************************************************************)
(* Filenames *)
(*****************************************************************************)

type filename = string (* TODO could check that exist :) type sux *)
  (* with sexp *)

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


let (with_open_infile: filename -> ((in_channel) -> 'a) -> 'a) = fun file f ->
  let chan = open_in file in
  unwind_protect (fun () ->
    let res = f chan in
    close_in chan;
    res)
    (fun e -> close_in chan)


(*****************************************************************************)
(* List *)
(*****************************************************************************)

let exclude p xs =
  List.filter (fun x -> not (p x)) xs

let rec take_safe n xs =
  match (n,xs) with
  | (0,_) -> []
  | (_,[]) -> []
  | (n,x::xs) -> x::take_safe (n-1) xs

let null xs = match xs with [] -> true | _ -> false

(*****************************************************************************)
(* Hash *)
(*****************************************************************************)

let hash_to_list h =
  Hashtbl.fold (fun k v acc -> (k,v)::acc) h []
  |> List.sort compare

let hash_of_list xs =
  let h = Hashtbl.create 101 in
  xs |> List.iter (fun (k, v) -> Hashtbl.replace h k v);
  h