(*

Subdirectories name, with the trailing slash removed.

#use"subdirectory.ml";;

*)

type t=SD of string;;

let unveil (SD s)=s;;


let of_string s=SD s;;

let depth (SD s)=
 if s="" then 0 else
 (List.length(Substring.occurrences_of_in "/" s))+1;;

let connectable_to_subpath (SD s)=if s="" then "" else s^"/";;


let ocaml_name (SD s)="Subdirectory"^"."^"of_string(\""^s^"\")";;