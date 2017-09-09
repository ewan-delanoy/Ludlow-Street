(*

#use"Php_analizer/php_punctuator.ml";;

*)



(* 

from https://github.com/php/php-langspec/blob/master/spec/09-lexical-structure.md#operators-and-punctuators 
I do not consider =& as a single token, but as a composite = followed by &.

*)

type t=
    [
     `T_LPARENTHESIS
    |`T_RPARENTHESIS
    |`T_LBRACE
    |`T_RBRACE
    |`T_ARROW
    |`T_SEMICOLON
    |`T_COMMA
    |`T_COLON_COLON
    ];;

    let t_lparenthesis = (`T_LPARENTHESIS:t);;
    let t_rparenthesis = (`T_RPARENTHESIS:t);;
    let t_comma = (`T_COMMA:t);;
    let t_arrow = (`T_ARROW:t);;
    let t_colon_colon = (`T_COLON_COLON:t);;
    let t_semicolon = (`T_SEMICOLON:t);;
    let t_lbrace = (`T_LBRACE:t);;
    let t_rbrace = (`T_RBRACE:t);;
 

let data=(([
    (t_lparenthesis,"(","lparenthesis");
    (t_rparenthesis,")","rparenthesis");
    (t_comma,",","comma");
    (t_arrow,"->","arrow");
    (t_colon_colon,"::","colon_colon");
    (t_semicolon,";","semicolon");
    (t_lbrace,"{","lbrace");
    (t_rbrace,"}","rbrace");
 ]) : (t*string*string) list);;
 
 let all =Image.image (fun (pkt,viz,sn)->pkt) data;; 

exception Unknown_visible of string;; 

let make_visible =((
  fun pkt->
  let (_,viz,_)=Option.find_really(
      fun (pkt1,_,_)->pkt1=pkt
  ) data in
  viz): t -> string );;

let from_visible=((
   fun viz->
    match Option.find_it(
        fun (_,viz1,_)->viz1=viz
    ) data with
     None->raise(Unknown_visible(viz))
    |Some(pkt,_,_)->pkt): string -> t );;



let readable=make_visible;;
 



 
  
   
