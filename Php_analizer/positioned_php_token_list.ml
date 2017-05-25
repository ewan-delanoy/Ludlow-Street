(*

#use"Php_analizer/positioned_php_token_list.ml";;

*)

type t={
   contained : Positioned_php_token.t list;
};;

let empty={contained=[]};;
let is_empty x=(x.contained=[]);;
let hd x=List.hd(x.contained);;
let tl x={contained=List.tl(x.contained)};;
let concat x y={contained=(x.contained)@(y.contained)};;

let cons a x={contained=a::(x.contained)};;
let singleton x={contained=[x]};;
let rev x={contained=List.rev(x.contained)};;
let length x=List.length(x.contained);;
let big_head d x={contained=Listennou.big_head d (x.contained)};;

exception Ht_exn;;

let ht x=match x.contained with
    []->raise(Ht_exn)
    |a::b->(a,{contained=b});;
    
let print x=
  let temp1=Image.image(fun ptok->
    let tok=Positioned_php_token.fst ptok in
    Php_token.projected_version tok
   ) x.contained in
  "[ "^(String.concat " " temp1)^" ]";;

let print_out (dummy:Format.formatter) x=
   Format.open_box 0;
   Format.print_string(print x);
   Format.close_box();;

    