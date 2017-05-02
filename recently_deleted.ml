(*

#use"recently_deleted.ml";;

*)

type recently_deleted=RD of string;;

let of_string s=RD s;;
let to_string (RD s)=s;;
