(*

#use"Php_analizer/php_blocker.ml";;


   
*)

type t=
   Parenthesis
  |Brace
  |Bracket
  |Ternop;;
  
let parenthesis=Parenthesis;;
let brace=Brace;;
let bracket=Bracket;;
let ternop=Ternop;;

let all=
  [parenthesis;brace;bracket;ternop];;  
  
let pair=function
   Parenthesis->("(",")")
  |Brace->("{","}")
  |Bracket->("[","]")
  |Ternop->("?",":");;  
  
let all_pairs=Image.image pair all;;  
  
let token_pair blckr=
   let (x,y)=pair blckr in
   (Php_token.put_lexeme_in_category x,Php_token.put_lexeme_in_category y);;
  