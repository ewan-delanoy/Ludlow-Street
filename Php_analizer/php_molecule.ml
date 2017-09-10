(*

#use"Php_analizer/php_molecule.ml";;

*)



type t= [Php_token.t|`Block of Php_blocker_name.t*(Php_positioned_token_list.t)];;

let of_token (tok:Php_token.t)=(tok:>t);;

let readable (mole:t)=match mole with
   #Php_token.t as tok->Php_token.readable(tok)
  |`Block(blckr,l)->let (a,b)=Php_blocker_name.pair blckr in a^b;;
