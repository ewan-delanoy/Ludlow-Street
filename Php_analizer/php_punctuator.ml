(*

#use"Php_analizer/php_punctuator.ml";;

*)



(* 

from https://github.com/php/php-langspec/blob/master/spec/09-lexical-structure.md#operators-and-punctuators 
I do not consider =& as a single token, but as a composite = followed by &.

*)

type t=
     T_LPARENTHESIS
    |T_RPARENTHESIS
    |T_LBRACE
    |T_RBRACE
    |T_ARROW
    |T_SEMICOLON
    |T_COMMA
    |T_COLON_COLON;;

    let t_lparenthesis = (T_LPARENTHESIS);;
    let t_rparenthesis = (T_RPARENTHESIS);;
    let t_comma = (T_COMMA);;
    let t_arrow = (T_ARROW);;
    let t_colon_colon = (T_COLON_COLON);;
    let t_semicolon = (T_SEMICOLON);;
    let t_lbrace = (T_LBRACE);;
    let t_rbrace = (T_RBRACE);;
 

let all_triples=[
    (t_lparenthesis,"(","lparenthesis");
    (t_rparenthesis,")","rparenthesis");
    (t_comma,",","comma");
    (t_arrow,"->","arrow");
    (t_colon_colon,"::","colon_colon");
    (t_semicolon,";","semicolon");
    (t_lbrace,"{","lbrace");
    (t_rbrace,"}","rbrace");
 ];;
 
 let to_string pkt=
  let (_,viz,_)=Option.find_really(
      fun (pkt1,_,_)->pkt1=pkt
  ) all_triples in
  viz;;

let all_pairs =
    Ordered.forget_order( Ordered.diforchan Keyval_ordering.ko 
    (Image.image (fun (pkt,viz,sn)->(viz,pkt)) all_triples));;

 
let all_punctuators =Image.image snd all_pairs;; 

exception Unknown_punctuator_string of string;; 

let of_prudent_string s=
  Option.find_it (fun oprtr->to_string(oprtr)=s) all_punctuators;; 
 
let of_string s=
  match of_prudent_string s with
   None->raise(Unknown_punctuator_string(s))
  |Some(oprtr)->oprtr;;
  
let all_strings=Image.image fst all_pairs;;      
 
  
  
   
