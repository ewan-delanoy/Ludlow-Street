(*

#use"recently_changed.ml";;

*)

type recently_changed=RC of string;;

let of_string s=RC s;;
let to_string (RC s)=s;;
