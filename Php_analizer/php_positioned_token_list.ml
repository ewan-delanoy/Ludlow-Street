(*

#use"Php_analizer/php_positioned_token_list.ml";;

*)

type t= ( Php_token.t * (Lexing.position * Lexing.position)  ) list;;


let hd (x:t)=List.hd(x);;
let tl (x:t)=List.tl(x);;


let cons a (x:t)=(a::x:t);;
let singleton (x:t)=[x];;
let rev (x:t)=(List.rev(x):t);;
let length (x:t)=List.length(x);;
let big_head d (x:t)=Listennou.big_head d (x);;

let filter f x=List.filter f x;;

exception Ht_exn;;

let ht x=match x with
    []->raise(Ht_exn)
    |a::b->(a,b);;
    
exception File_exn;;    
    
let file x=match x with
    []->raise(File_exn)
    |(_,(y1,_))::_->y1.Lexing.pos_fname;;    
    
let print (x:t)=
  let temp1=Image.image(fun ptok->
    let tok=fst ptok in
    Php_projected_token.readable(Php_token.form tok)
   ) x in
  "\xe3\x80\x90  "^(String.concat " " temp1)^"  \xe3\x80\x91";;

let print_out (fmt:Format.formatter) (x:t)=
   Format.fprintf fmt "@[%s@]" (print x);;

    