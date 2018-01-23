(*

#use"naked_module.ml";;

A module name, or a candidate for one. Uncapitalized. Should contain no slashes.

*)

type t=N of string;;

let of_string s=N (String.uncapitalize_ascii s);; 
let to_string (N s)=s;;

