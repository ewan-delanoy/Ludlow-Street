(*

Operation on substring finding, with indexes starting from 1.

#use"unqsubstr_helper.ml";;

*)

type t=H of string;;

let of_string s=H s;;

let to_string (H s)=s;;

