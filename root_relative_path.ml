(*

#use"root_relative_path.ml";;

*)

type t=RP of string;;

let of_string s=RP(s);;
let to_string (RP s)=s;;

