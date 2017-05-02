(*

#use"recently_deleted.ml";;

*)

type recently_deleted=RD of string list;;

let of_string_list l=RD l;;
let to_string_list (RD l)=l;;
