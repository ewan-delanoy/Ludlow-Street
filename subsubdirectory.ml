(*

String appearing as a subsubdirectory name, at the moment it is used.

#use"subsubdirectory.ml";;

*)

type t=SSD of string;;

let of_string (SSD s)=s;;

let to_string s=SSD s;;

