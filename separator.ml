(*

#use"separator.ml";;

*)

type t=S of string;;

let of_string s=S s;;
let to_string (S s)=s;;

