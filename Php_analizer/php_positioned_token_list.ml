(*

#use"Php_analizer/php_positioned_token_list.ml";;

*)

type t={
   contained : ( Php_token.t * (Lexing.position * Lexing.position)  ) list;
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

let filter f x={contained=List.filter f (x.contained)}

exception Ht_exn;;

let ht x=match x.contained with
    []->raise(Ht_exn)
    |a::b->(a,{contained=b});;
    
exception File_exn;;    
    
let file x=match x.contained with
    []->raise(File_exn)
    |(_,(y1,_))::_->y1.Lexing.pos_fname;;    
    
let print x=
  let temp1=Image.image(fun ptok->
    let tok=fst ptok in
    Php_projected_token.readable(Php_token.form tok)
   ) x.contained in
  "\xe3\x80\x90  "^(String.concat " " temp1)^"  \xe3\x80\x91";;

let print_out (fmt:Format.formatter) x=
   Format.fprintf fmt "@[%s@]" (print x);;

    