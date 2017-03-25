(*

#use"GParser/hparser_for_ocaml_language.ml";;

*)

let double_semicolon=";"^";";;

let prsr_for_comment=Gparser.House_with_doors ("(*","*)",["\"","\""]);;


let prsr_for_sharp_comment=Gparser.Enclosure ("\n#","\n");;

let prsr_for_space=Gparser.Constant " ";;
let prsr_for_tab=Gparser.Constant "\t";;


let prsr_for_space_or_tab=Gparser.Disjunction [prsr_for_space;prsr_for_tab];;
let prsr_for_linebreak=Gparser.Constant "\n";;
let prsr_for_newline=Gparser.Constant "\012";;
let prsr_for_windows_newline=Gparser.Constant "\r";;
let prsr_for_individual_white=Gparser.Disjunction 
[prsr_for_space;prsr_for_tab;prsr_for_linebreak;prsr_for_newline;prsr_for_windows_newline];;

let prsr_for_inline_white_maybe=Gparser.Star prsr_for_space_or_tab;;
let prsr_for_white_maybe=Gparser.Star prsr_for_individual_white;;
let prsr_for_white=Gparser.One_or_more prsr_for_individual_white;;

let prsr_for_special_sharp=Gparser.Chain
   [
     Gparser.Constant "#";
     prsr_for_inline_white_maybe;
     Gparser.Sample_star "0123456789";
     prsr_for_inline_white_maybe;
     Gparser.Constant "\"";
     Hparser.Sample_star "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ/.";
     Hparser.Constant "\"";
     prsr_for_inline_white_maybe;
   ];;

let prsr_for_uncapitalized_word=Gparser.Chain
   [
     Gparser.Sample_char "abcdefghijklmnopqrstuvwxyz_";
     Gparser.Sample_star "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
   ];;

let prsr_for_capitalized_word=Gparser.Chain
   [
     Gparser.Sample_char "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
     Gparser.Sample_star "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ012356789";
   ];;

let prsr_for_pointing_module=Gparser.Chain
   [
     prsr_for_capitalized_word;
     Gparser.Constant ".";
   ];;

let prsr_for_wholly_lowercase_name=
   Gparser.Chain
   [
     Gparser.Sample_char "abcdefghijklmnopqrstuvwxyz_";
     Gparser.Sample_star "abcdefghijklmnopqrstuvwxyz_";
   ];;


let prsr_for_element_in_uple_in_typedef=
   Gparser.Chain
   [
     Gparser.Constant "'";
      prsr_for_uncapitalized_word; 
     prsr_for_white_maybe; 
     Gparser.Constant ",";
     prsr_for_white_maybe; 
   ];;

let prsr_for_parameters1_in_type=
   Gparser.Chain
   [
     Gparser.Constant "'";
      prsr_for_uncapitalized_word; 
     prsr_for_white_maybe; 
   ];;

let prsr_for_parameters2_in_type=
   Gparser.Chain
   [
     Gparser.Constant "(";
     prsr_for_white_maybe; 
     Gparser.Star(prsr_for_element_in_uple_in_typedef);
     prsr_for_white_maybe; 
     Gparser.Constant "'";
     prsr_for_uncapitalized_word; 
     prsr_for_white_maybe; 
     Gparser.Constant ")";
     prsr_for_white_maybe; 
   ];;

   

let prsr_for_parameters_in_type=
   Gparser.Disjunction
   [
     prsr_for_parameters1_in_type;
     prsr_for_parameters2_in_type;
   ];;

let prsr_for_value_making=Gparser.Chain
   [
     Gparser.Constant "let";
     prsr_for_white;
     prsr_for_uncapitalized_word;
     prsr_for_white_maybe;
     Gparser.Enclosure ("","=");
     Gparser.Enclosure ("",double_semicolon);
   ];;

let prsr_for_type_making=Gparser.Chain
   [
     Gparser.Constant "type";
     prsr_for_white;
     Gparser.Optional(prsr_for_parameters_in_type);
     prsr_for_uncapitalized_word;
     prsr_for_white_maybe;
     Gparser.Enclosure ("","=");
     Gparser.Enclosure ("",double_semicolon);
   ];;



let prsr_for_exception_making=Gparser.Chain
     [
     Gparser.Constant "exception";
     prsr_for_white;
     prsr_for_capitalized_word;
     Gparser.Enclosure ("",double_semicolon);
   ];;

let prsr_for_module_opener=
   Gparser.Chain
   [
     Gparser.Constant "module";
     prsr_for_white;
     prsr_for_capitalized_word;
     prsr_for_white_maybe;
     Gparser.Constant "=";
     prsr_for_white_maybe;
     Gparser.Constant "struct";
   ];;

let prsr_for_module_ender=
   Gparser.Chain
   [
     Gparser.Constant "end";
     prsr_for_white_maybe;
     Gparser.Constant double_semicolon;
   ];;

let prsr_for_module_inclusion=
   Gparser.Chain
   [
     Gparser.Constant "include ";
     prsr_for_white_maybe;
     prsr_for_capitalized_word;
     prsr_for_white_maybe;
     Gparser.Constant double_semicolon;
   ];;

let prsr_for_special_names=
   Gparser.Disjunction
     [
       Gparser.Constant "add_to_vvv ";
       Gparser.Constant "add_data ";
       Gparser.Constant "add_data\n";
       Gparser.Constant "add_shortcut ";
     ];;   
   
let prsr_for_specialities=Gparser.Chain
   [
     prsr_for_special_names;
     Gparser.Enclosure ("",double_semicolon);
   ];;   

let elt_prsr=
   Gparser.Disjunction
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
   Gparser.Star elt_prsr;;



   
