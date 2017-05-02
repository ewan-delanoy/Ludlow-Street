(*

#use"recently_changed.ml";;

*)

type recently_changed=RC of string list;;

let of_string_list s=RC l;;
let to_string_list (RC l)=l;;
