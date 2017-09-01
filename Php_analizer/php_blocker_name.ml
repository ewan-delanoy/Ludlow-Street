(*

#use"Php_analizer/php_blocker_name.ml";;


   
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
  
let pair x=match x with
   Parenthesis->("(",")")
  |Brace->("{","}")
  |Bracket->("[","]")
  |Ternop->("?",":");;  
  
  let token_pair blckr=
    let (x,y)=pair blckr in
    (Php_token.put_lexeme_in_category x,Php_token.put_lexeme_in_category y);;
   
  