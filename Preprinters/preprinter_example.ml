(*

#use"Preprinters/preprinter_example.ml";;

Returns the list of tokens in a description of the argument object.

*)


let positioned_php_token=
  let tempf=(fun ptok->
   [Php_token.short_content(Positioned_php_token.fst ptok)]) in
  (tempf: Positioned_php_token.t Preprinter.t);;

let positioned_php_token_list=
  Preprinter_homomorphism.list 
   positioned_php_token;;
