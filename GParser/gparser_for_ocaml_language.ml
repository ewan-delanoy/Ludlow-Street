(*

#use"GParser/gparser_for_ocaml_language.ml";;

*)

let prsr_for_comment=Gparser_constructor.enclosure ("(*","*)");;
let prsr_for_sharp_comment=Gparser_constructor.enclosure ("\n#","\n");;

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

let prsr_for_special_sharp=Gparser_homomorphism.chain
   [
     Gparser_constructor.constant "#";
     prsr_for_inline_white_maybe;
     Gparser_constructor.sample_star "0123456789";
     prsr_for_inline_white_maybe;
     Gparser_constructor.constant "\"";
     Gparser_constructor.sample_star "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ/.";
     Gparser_constructor.constant "\"";
     prsr_for_inline_white_maybe;
   ];;

let prsr_for_uncapitalized_word=Gparser_homomorphism.chain
   [
     Gparser_constructor.sample_char "abcdefghijklmnopqrstuvwxyz_";
     Gparser_constructor.sample_star "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
   ];;

let prsr_for_capitalized_word=Gparser_homomorphism.chain
   [
     Gparser_constructor.sample_char "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
     Gparser_constructor.sample_star "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ012356789";
   ];;

let prsr_for_pointing_module=Gparser_homomorphism.chain
   [
     prsr_for_capitalized_word;
     Gparser_constructor.constant ".";
   ];;

let prsr_for_exception_ending=
   Gparser_homomorphism.chain
   [
     Gparser_constructor.constant "of";
     prsr_for_white;
     Gparser_homomorphism.optional(prsr_for_pointing_module);
     Gparser_constructor.sample_star "abcdefghijklmnopqrstuvwxyz_*";
   ];;

let prsr_for_parameters_in_type=
   Gparser_homomorphism.chain
   [
     Gparser_constructor.constant "'";
      prsr_for_uncapitalized_word; 
     prsr_for_white_maybe; 
   ];;

let prsr_for_value_making=Gparser_homomorphism.chain
   [
     Gparser_constructor.constant "let";
     prsr_for_white;
     prsr_for_uncapitalized_word;
     prsr_for_white_maybe;
     Gparser_constructor.enclosure ("","=");
     Gparser_constructor.enclosure ("",";;");
   ];;

let prsr_for_type_making=Gparser_homomorphism.chain
   [
     Gparser_constructor.constant "type";
     prsr_for_white;
     Gparser_homomorphism.optional(prsr_for_parameters_in_type);
     prsr_for_uncapitalized_word;
     prsr_for_white_maybe;
     Gparser_constructor.enclosure ("","=");
     Gparser_constructor.enclosure ("",";;");
   ];;

let prsr_for_exception_making=Gparser_homomorphism.chain
   [
     Gparser_constructor.constant "exception";
     prsr_for_white;
     prsr_for_capitalized_word;
     prsr_for_white_maybe;
     Gparser_homomorphism.optional(prsr_for_exception_ending);
     prsr_for_white_maybe;
     Gparser_constructor.constant ";;";
   ];;

let prsr_for_module_opener=
   Gparser_homomorphism.chain
   [
     Gparser_constructor.constant "module";
     prsr_for_white;
     prsr_for_capitalized_word;
     prsr_for_white_maybe;
     Gparser_constructor.constant "=";
     prsr_for_white_maybe;
     Gparser_constructor.constant "struct";
   ];;

let prsr_for_module_ender=
   Gparser_homomorphism.chain
   [
     Gparser_constructor.constant "end";
     prsr_for_white_maybe;
     Gparser_constructor.constant ";;";
   ];;

let prsr_for_special_names=
   Gparser_homomorphism.disjunction
     [
       Gparser_constructor.constant "add_to_vvv ";
       Gparser_constructor.constant "add_data ";
       Gparser_constructor.constant "add_data\n";
       Gparser_constructor.constant "add_shortcut ";
     ];;   
   
let prsr_for_specialities=Gparser_homomorphism.chain
   [
     prsr_for_special_names;
     Gparser_constructor.enclosure ("",";;");
   ];;   

let elt_prsr=
   Gparser_homomorphism.disjunction
     [
       prsr_for_value_making;
       prsr_for_type_making;
       prsr_for_exception_making;
       prsr_for_comment;
       prsr_for_sharp_comment;
       prsr_for_special_sharp;
       prsr_for_module_opener;
       prsr_for_module_ender;
       prsr_for_specialities;
       prsr_for_white;
     ];;


let main_prsr=
   Gparser_homomorphism.star elt_prsr;;



   
