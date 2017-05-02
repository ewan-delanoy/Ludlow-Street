(*

#use"recently_created.ml";;

*)

type recently_created=RC of string;;

let of_string s=RC s;;
let to_string (RC s)=s;;
