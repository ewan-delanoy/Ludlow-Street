(*

#use"Php_analizer/php_blocker.ml";;


   
*)

type bare_t=
   Parenthesis
  |Brace
  |Bracket
  |Ternop;;
  
type t=Bl of bare_t * int;;

let depth (Bl(x,d))=d;;

let parenthesis=Bl(Parenthesis,0);;
let brace=Bl(Brace,0);;
let bracket=Bl(Bracket,0);;
let ternop=Bl(Ternop,0);;

let all=
  [parenthesis;brace;bracket;ternop];;  
  
let pair (Bl(x,d))=match x with
   Parenthesis->("(",")")
  |Brace->("{","}")
  |Bracket->("[","]")
  |Ternop->("?",":");;  
  
let all_pairs=Image.image pair all;;  
  
let token_pair blckr=
   let (x,y)=pair blckr in
   (Php_token.put_lexeme_in_category x,Php_token.put_lexeme_in_category y);;
  