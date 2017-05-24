(*

#use"Php_analizer/positioned_php_token_list.ml";;

*)

type t={
   contained : Positioned_php_token.t list;
};;

let empty={contained=[]};;
let is_empty x=(x.contained=[]);;
let hd x=List.hd(x.contained);;

let cons a x={contained=a::(x.contained)};;
let rev x={contained=List.rev(x.contained)};;
let length x=List.length(x.contained);;
let big_head d x={contained=Listennou.big_head d (x.contained)};;


exception Ht_exn;;

let ht x=match x.contained with
    []->raise(Ht_exn)
    |a::b->(a,{contained=b});;