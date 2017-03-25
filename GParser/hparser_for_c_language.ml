(*

#use"GParser/hparser_for_c_language.ml";;

*)

let prsr_for_comment=Hparser.Enclosure ("/*","*/");;


let prsr_for_space=Hparser.Constant " ";;
let prsr_for_tab=Hparser.Constant "\t";;

let prsr_for_space_or_tab=Hparser.Disjunction [prsr_for_space;prsr_for_tab];;
let prsr_for_linebreak=Hparser.Constant "\n";;
let prsr_for_newline=Hparser.Constant "\012";;
let prsr_for_individual_white=Hparser.Disjunction [prsr_for_space;prsr_for_tab;prsr_for_linebreak;prsr_for_newline];;

let prsr_for_inline_white_maybe=Hparser.Star prsr_for_space_or_tab;;
let prsr_for_white_maybe=Hparser.Star prsr_for_individual_white;;
let prsr_for_white=Hparser.One_or_more prsr_for_individual_white;;

let prsr1=Hparser.Enclosure ("\"","\"");;
let prsr2=Hparser.Enclosure ("<",">");;
let prsr3=Hparser.Disjunction [prsr1;prsr2];;


let prsr_for_inclusion=Hparser.Chain
      [
        Hparser.Constant "\n#";
        prsr_for_inline_white_maybe;
        Hparser.Constant "include";
        prsr_for_inline_white_maybe;
        prsr3
      ];;


let prsr_for_typeword_not_starting_with_u=Hparser.Chain
   [
     Hparser.Sample_neg "u";
     Hparser.Sample_star "abcdefghijklmnopqrstuvwxyz_";
   ];;

let prsr_for_typeword_starting_with_u_but_not_with_un=Hparser.Chain
   [
     Hparser.Constant "u";
     Hparser.Sample_neg "n";
     Hparser.Sample_star "abcdefghijklmnopqrstuvwxyz_";
   ];;

let prsr_for_typeword=Hparser.Disjunction
   [
     prsr_for_typeword_not_starting_with_u;
     prsr_for_typeword_starting_with_u_but_not_with_un;
     
   ];;



let prsr_for_braced=Hparser.Chain
   [
      Hparser.Constant "{";
      Hparser.House_with_doors ("{","}",["/*","*/";"//","\n";"\"","\"";"'","'"]);
   ];;

let prsr_for_typename1=Hparser.Chain
   [
      Hparser.Constant "struct";
      prsr_for_inline_white_maybe;
      prsr_for_typeword;
      prsr_for_white;
      Hparser.Optional(prsr_for_braced);
   ];;
   
let prsr_for_typename2=Hparser.Chain
   [
      Hparser.Constant "unsigned";
      prsr_for_inline_white_maybe;
      Hparser.Constant "int";
   ];;    


let prsr_for_typename3=Hparser.Chain
   [
      Hparser.Constant "long";
      prsr_for_inline_white_maybe;
      Hparser.Constant "int";
   ];;    

let prsr_for_typename4=Hparser.Chain
   [
      Hparser.Constant "unsigned";
      prsr_for_inline_white_maybe;
      Hparser.Constant "long";
   ];;    

let prsr_for_typename=Hparser.Disjunction
   [
     prsr_for_typename1;
     prsr_for_typename2;
     prsr_for_typename3;
     prsr_for_typename4;
     prsr_for_typeword;
     Hparser.Constant "FILE";
   ];;

let prsr_for_possibly_starred_typename=Hparser.Chain
   [
     prsr_for_typename;
     prsr_for_inline_white_maybe;
     Hparser.Optional(Hparser.Constant "*");
   ];;

let prsr_for_variableword=Hparser.Chain
	[
	  Hparser.Sample_char "abcdefghijklmnopqrstuvwxyz_";
	  Hparser.Sample_star "abcdefghijklmnopqrstuvwxyz_0123456789"
	];;

let prsr_for_identword=Hparser.Chain
	[
	  Hparser.Sample_char "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	  Hparser.Sample_star "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
	];;

let prsr_for_uppercase_word=Hparser.Sample_plus "_ABCDEFGHIJKLMNOPQRSTUVWXYZ";; 

let prsr_for_negative_int=Hparser.Chain
	[
	  Hparser.Constant "-";
	  Hparser.Sample_star "0123456789"
	];;


let prsr_for_rvalue=Hparser.Disjunction
   [
     prsr_for_identword;
     prsr_for_negative_int;
     Hparser.Sample_plus "0123456789";
     prsr_for_braced;
   ];;

let prsr_for_vardecl=Hparser.Chain
   [
      
      prsr_for_possibly_starred_typename;
      prsr_for_inline_white_maybe;
      prsr_for_variableword;
      prsr_for_inline_white_maybe;
      Hparser.Constant "=";
      prsr_for_inline_white_maybe;
      prsr_for_rvalue;
      prsr_for_inline_white_maybe;
      Hparser.Constant ";";
   ];;

let prsr_for_def_directive=Hparser.Chain
      [
        Hparser.Constant "\n#";
        prsr_for_inline_white_maybe;
        Hparser.Constant "define";
        prsr_for_space_or_tab;
        Hparser.Race ("\\\n","\n");
      ];;

let prsr_for_lonely_def_directive=Hparser.Chain
      [
        Hparser.Constant "\n#";
        prsr_for_inline_white_maybe;
        Hparser.Constant "define";
        prsr_for_space_or_tab;
        prsr_for_inline_white_maybe;
        prsr_for_uppercase_word;
        Hparser.Footless_constant "\n";
      ];;

let prsr_for_undef_directive=Hparser.Chain
      [
        Hparser.Constant "\n#";
        prsr_for_inline_white_maybe;
        Hparser.Constant "undef";
        prsr_for_space_or_tab;
        prsr_for_inline_white_maybe;
        prsr_for_uppercase_word;
        Hparser.Footless_constant "\n";
      ];;

let prsr_for_typename_inliner1=Hparser.Chain
     [
       Hparser.Constant "__inline";
       prsr_for_white;
       Hparser.Constant "static";
     ];;

let prsr_for_typename_inliner2=Hparser.Chain
     [
       Hparser.Constant "static";
       prsr_for_white;
       Hparser.Constant "inline";
       prsr_for_white;
       Hparser.Constant "const";
     ];;



let prsr_for_typename_inliner=Hparser.Disjunction
     [
       prsr_for_typename_inliner1;
       prsr_for_typename_inliner2;
       Hparser.Constant "__inline";
       Hparser.Constant "static";
       Hparser.Constant "extern";
     ];;

let prsr_for_whitened_typename_inliner=Hparser.Chain
    [
      prsr_for_typename_inliner;
      prsr_for_white;
    ];;

let prsr_for_fundecl1=
     Hparser.Chain
   [
      
      Hparser.Optional(prsr_for_whitened_typename_inliner);
      prsr_for_possibly_starred_typename;
      prsr_for_white_maybe;
      prsr_for_variableword;
      prsr_for_white_maybe;
      Hparser.Optional(prsr_for_uppercase_word);
      prsr_for_white_maybe;
      Hparser.House_with_doors ("(",")",[]);
      prsr_for_white_maybe;
      Hparser.Optional(Hparser.Constant "internal_function");
      Hparser.Constant ";";
   ];;

let prsr_for_fundecl=
   Hparser.Disjunction
    [
      prsr_for_fundecl1;
    ];;


let prsr_for_fundef1=
     Hparser.Chain
   [
      
      Hparser.Optional(prsr_for_whitened_typename_inliner);
      prsr_for_possibly_starred_typename;
      prsr_for_white_maybe;
      Hparser.Optional(Hparser.Constant "internal_function");
      prsr_for_white_maybe;
      prsr_for_variableword;
      prsr_for_white;
      Hparser.Enclosure ("(",")");
      prsr_for_white;
      Hparser.Sample_negstar "{";     
      prsr_for_braced;
   ];;
   
let prsr_for_fundef2=
     Hparser.Chain
   [
      
      Hparser.Constant "RETSIGTYPE";
      prsr_for_white;
      prsr_for_variableword;
      prsr_for_white;
      Hparser.Enclosure ("(",")");
      prsr_for_white;
      Hparser.Sample_negstar "{";     
      prsr_for_braced;
   ];;   
   
let prsr_for_fundef=
   Hparser.Disjunction
    [
      prsr_for_fundef1;
      prsr_for_fundef2;
    ];;   

let prsr_for_structdef=
     Hparser.Chain
   [
      Hparser.Constant "struct";
      prsr_for_white;
      prsr_for_typeword;
      prsr_for_white_maybe;
      prsr_for_braced;
      prsr_for_white_maybe;
      Hparser.Constant ";";
   ];;

let elt_prsr=
   Hparser.Disjunction
     [
       prsr_for_inclusion; 
       prsr_for_lonely_def_directive;
       prsr_for_def_directive;
       prsr_for_undef_directive;
       prsr_for_individual_white;
       prsr_for_comment;
       prsr_for_vardecl;
       prsr_for_fundecl;
       prsr_for_fundef;
       prsr_for_structdef;
     ];;


let main_prsr=
   Hparser.Star elt_prsr;;



   
