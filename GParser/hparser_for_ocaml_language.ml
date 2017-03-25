(*

#use"GParser/hparser_for_ocaml_language.ml";;

*)

let double_semicolon=";"^";";;

let prsr_for_comment=Hparser.House_with_doors ("(*","*)",["\"","\""]);;


let prsr_for_sharp_comment=Hparser.Enclosure ("\n#","\n");;

let prsr_for_space=Hparser.Constant " ";;
let prsr_for_tab=Hparser.Constant "\t";;


let prsr_for_space_or_tab=Hparser.Disjunction [prsr_for_space;prsr_for_tab];;
let prsr_for_linebreak=Hparser.Constant "\n";;
let prsr_for_newline=Hparser.Constant "\012";;
let prsr_for_windows_newline=Hparser.Constant "\r";;
let prsr_for_individual_white=Hparser.Disjunction 
[prsr_for_space;prsr_for_tab;prsr_for_linebreak;prsr_for_newline;prsr_for_windows_newline];;

let prsr_for_inline_white_maybe=Hparser.Star prsr_for_space_or_tab;;
let prsr_for_white_maybe=Hparser.Star prsr_for_individual_white;;
let prsr_for_white=Hparser.One_or_more prsr_for_individual_white;;

let prsr_for_special_sharp=Hparser.Chain
   [
     Hparser.Constant "#";
     prsr_for_inline_white_maybe;
     Hparser.Sample_star "0123456789";
     prsr_for_inline_white_maybe;
     Hparser.Constant "\"";
     Hparser.Sample_star "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ/.";
     Hparser.Constant "\"";
     prsr_for_inline_white_maybe;
   ];;

let prsr_for_uncapitalized_word=Hparser.Chain
   [
     Hparser.Sample_char "abcdefghijklmnopqrstuvwxyz_";
     Hparser.Sample_star "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
   ];;

let prsr_for_capitalized_word=Hparser.Chain
   [
     Hparser.Sample_char "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
     Hparser.Sample_star "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ012356789";
   ];;

let prsr_for_pointing_module=Hparser.Chain
   [
     prsr_for_capitalized_word;
     Hparser.Constant ".";
   ];;

let prsr_for_wholly_lowercase_name=
   Hparser.Chain
   [
     Hparser.Sample_char "abcdefghijklmnopqrstuvwxyz_";
     Hparser.Sample_star "abcdefghijklmnopqrstuvwxyz_";
   ];;


let prsr_for_element_in_uple_in_typedef=
   Hparser.Chain
   [
     Hparser.Constant "'";
      prsr_for_uncapitalized_word; 
     prsr_for_white_maybe; 
     Hparser.Constant ",";
     prsr_for_white_maybe; 
   ];;

let prsr_for_parameters1_in_type=
   Hparser.Chain
   [
     Hparser.Constant "'";
      prsr_for_uncapitalized_word; 
     prsr_for_white_maybe; 
   ];;

let prsr_for_parameters2_in_type=
   Hparser.Chain
   [
     Hparser.Constant "(";
     prsr_for_white_maybe; 
     Hparser.Star(prsr_for_element_in_uple_in_typedef);
     prsr_for_white_maybe; 
     Hparser.Constant "'";
     prsr_for_uncapitalized_word; 
     prsr_for_white_maybe; 
     Hparser.Constant ")";
     prsr_for_white_maybe; 
   ];;

   

let prsr_for_parameters_in_type=
   Hparser.Disjunction
   [
     prsr_for_parameters1_in_type;
     prsr_for_parameters2_in_type;
   ];;

let prsr_for_value_making=Hparser.Chain
   [
     Hparser.Constant "let";
     prsr_for_white;
     prsr_for_uncapitalized_word;
     prsr_for_white_maybe;
     Hparser.Enclosure ("","=");
     Hparser.Enclosure ("",double_semicolon);
   ];;

let prsr_for_type_making=Hparser.Chain
   [
     Hparser.Constant "type";
     prsr_for_white;
     Hparser.Optional(prsr_for_parameters_in_type);
     prsr_for_uncapitalized_word;
     prsr_for_white_maybe;
     Hparser.Enclosure ("","=");
     Hparser.Enclosure ("",double_semicolon);
   ];;



let prsr_for_exception_making=Hparser.Chain
     [
     Hparser.Constant "exception";
     prsr_for_white;
     prsr_for_capitalized_word;
     Hparser.Enclosure ("",double_semicolon);
   ];;

let prsr_for_module_opener=
   Hparser.Chain
   [
     Hparser.Constant "module";
     prsr_for_white;
     prsr_for_capitalized_word;
     prsr_for_white_maybe;
     Hparser.Constant "=";
     prsr_for_white_maybe;
     Hparser.Constant "struct";
   ];;

let prsr_for_module_ender=
   Hparser.Chain
   [
     Hparser.Constant "end";
     prsr_for_white_maybe;
     Hparser.Constant double_semicolon;
   ];;

let prsr_for_module_inclusion=
   Hparser.Chain
   [
     Hparser.Constant "include ";
     prsr_for_white_maybe;
     prsr_for_capitalized_word;
     prsr_for_white_maybe;
     Hparser.Constant double_semicolon;
   ];;

let prsr_for_special_names=
   Hparser.Disjunction
     [
       Hparser.Constant "add_to_vvv ";
       Hparser.Constant "add_data ";
       Hparser.Constant "add_data\n";
       Hparser.Constant "add_shortcut ";
     ];;   
   
let prsr_for_specialities=Hparser.Chain
   [
     prsr_for_special_names;
     Hparser.Enclosure ("",double_semicolon);
   ];;   

let elt_prsr=
   Hparser.Disjunction
     [
       prsr_for_value_making;
       prsr_for_type_making;
       prsr_for_exception_making;
       prsr_for_comment;
       prsr_for_sharp_comment;
       prsr_for_special_sharp;
       prsr_for_module_opener;
       prsr_for_module_ender;
       prsr_for_module_inclusion;
       prsr_for_specialities;
       prsr_for_white;
     ];;


let main_prsr=
   Hparser.Star elt_prsr;;



   
