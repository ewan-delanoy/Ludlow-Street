(*

#use"Php_analizer/php_blocker.ml";;


   
*)


  
type t=Bl of Php_blocker_name.t * int;;

let depth (Bl(x,d))=d;;

let make x d=Bl(x,d);;

let parenthesis=Bl(Php_blocker_name.parenthesis,1);;
let brace=Bl(Php_blocker_name.brace,1);;
let bracket=Bl(Php_blocker_name.bracket,1);;
let ternop=Bl(Php_blocker_name.ternop,1);;

  
let pair (Bl(x,d))=Php_blocker_name.pair x;;  
  
let token_pair blckr=
   let (x,y)=pair blckr in
   (Php_token.from_visible x,Php_token.from_visible y);;
  