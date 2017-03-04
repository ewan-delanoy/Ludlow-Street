(*

#use"GParser/gparser_for_ocaml_language.ml";;

*)

let prsr_for_comment=Gparser_constructor.enclosure ("(*","*)");;


let prsr_for_space=Gparser_constructor.constant " ";;
let prsr_for_tab=Gparser_constructor.constant "\t";;

let prsr_for_space_or_tab=Gparser_homomorphism.disjunction [prsr_for_space;prsr_for_tab];;
let prsr_for_linebreak=Gparser_constructor.constant "\n";;
let prsr_for_newline=Gparser_constructor.constant "\012";;
let prsr_for_individual_white=Gparser_homomorphism.disjunction 
[prsr_for_space;prsr_for_tab;prsr_for_linebreak;prsr_for_newline];;

let prsr_for_inline_white_maybe=Gparser_homomorphism.star prsr_for_space_or_tab;;
let prsr_for_white_maybe=Gparser_homomorphism.star prsr_for_individual_white;;
let prsr_for_white=Gparser_homomorphism.one_or_more prsr_for_individual_white;;


let elt_prsr=
   Gparser_homomorphism.disjunction
     [
       prsr_for_comment;
       prsr_for_white;
     ];;


let main_prsr=
   Gparser_homomorphism.star elt_prsr;;



   
