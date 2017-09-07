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
 

let data=[
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
  ) data in
  viz;;

let of_string viz=
    let (pkt,_,_)=Option.find_really(
        fun (_,viz1,_)->viz1=viz
    ) data in
    pkt;;

let all_pairs =
    Ordered.forget_order( Ordered.diforchan Keyval_ordering.ko 
    (Image.image (fun (pkt,viz,sn)->(viz,pkt)) data));;

 
let all =Image.image snd all_pairs;; 


 
  
   
