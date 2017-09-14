(*

#use"Php_analizer/php_blocker_name.ml";;


   
*)

type t=Among of Php_constant_token.t * Php_constant_token.t;; 

let unveil (Among(x,y))=(x,y);;

let parenthesis=Among(`T_LPARENTHESIS, `T_RPARENTHESIS);;
let brace=Among(`T_LBRACE, `T_RBRACE);;
let bracket=Among(`T_LBRACKET, `T_RBRACKET);;
let ternop=Among(`T_QUESTION, `T_COLON);;

let all=
  [parenthesis;brace;bracket;ternop];;  



let make_visible (Among(x,y))=
  (Php_constant_token.make_visible x,
  Php_constant_token.make_visible y)
  ;;  
  
  let token_pair blckr=
    let (x,y)=make_visible blckr in
    (Php_token.from_visible x,Php_token.from_visible y);;
   

let seek_block_beginning tok=
    Option.seek (
      fun blckr->
        fst(token_pair blckr)=tok
    ) all;;      