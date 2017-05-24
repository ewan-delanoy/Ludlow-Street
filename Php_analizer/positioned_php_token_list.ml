(*

#use"Php_analizer/positioned_php_token_list.ml";;

*)

type t={
   contained : Positioned_php_token.t list;
};;

let empty={contained=[]};;
let cons a x={contained=a::(x.contained)};;

exception Ht_exn;;

let ht x=match x.contained with
    []->raise(Ht_exn)
    |a::b->(a,{contained=b});;