(*

#use"GParser/gparser_for_c_language.ml";;

*)

let prsr_for_comment=Gparser_constructor.enclosure ("/*","*/");;


let prsr_for_space=Gparser_constructor.constant " ";;
let prsr_for_tab=Gparser_constructor.constant "\t";;

let prsr_for_space_or_tab=Gparser_homomorphism.disjunction [prsr_for_space;prsr_for_tab];;
let prsr_for_linebreak=Gparser_constructor.constant "\n";;
let prsr_for_newline=Gparser_constructor.constant "\012";;
let prsr_for_individual_white=Gparser_homomorphism.disjunction [prsr_for_space;prsr_for_tab;prsr_for_linebreak;prsr_for_newline];;

let prsr_for_inline_white_maybe=Gparser_homomorphism.star prsr_for_space_or_tab;;
let prsr_for_white_maybe=Gparser_homomorphism.star prsr_for_individual_white;;
let prsr_for_white=Gparser_homomorphism.one_or_more prsr_for_individual_white;;

let prsr1=Gparser_constructor.enclosure ("\"","\"");;
let prsr2=Gparser_constructor.enclosure ("<",">");;
let prsr3=Gparser_homomorphism.disjunction [prsr1;prsr2];;


let prsr_for_inclusion=Gparser_homomorphism.chain
      [
        Gparser_constructor.constant "\n#";
        prsr_for_inline_white_maybe;
        Gparser_constructor.constant "include";
        prsr_for_inline_white_maybe;
        prsr3
      ];;



let prsr_for_typeword=Gparser_constructor.simple_plus "abcdefghijklmnopqrstuvwxyz_";;

let prsr_for_braced=Gparser_homomorphism.chain
   [
      Gparser_constructor.constant "{";
      Gparser_constructor.house_with_doors ("{","}") 
                                           ["/*","*/";"//","\n";"\"","\"";"'","'"];
   ];;

let prsr_for_typename1=Gparser_homomorphism.chain
   [
      Gparser_constructor.constant "struct";
      prsr_for_inline_white_maybe;
      prsr_for_typeword;
      prsr_for_white;
      Gparser_homomorphism.optional(prsr_for_braced);
   ];;
   
let prsr_for_typename2=Gparser_homomorphism.chain
   [
      Gparser_constructor.constant "unsigned";
      prsr_for_inline_white_maybe;
      Gparser_constructor.constant "int";
   ];;    


let prsr_for_typename3=Gparser_homomorphism.chain
   [
      Gparser_constructor.constant "long";
      prsr_for_inline_white_maybe;
      Gparser_constructor.constant "int";
   ];;    

let prsr_for_typename4=Gparser_homomorphism.chain
   [
      Gparser_constructor.constant "unsigned";
      prsr_for_inline_white_maybe;
      Gparser_constructor.constant "long";
   ];;    

let prsr_for_typename=Gparser_homomorphism.disjunction
   [
     prsr_for_typename1;
     prsr_for_typename2;
     prsr_for_typename3;
     prsr_for_typename4;
     prsr_for_typeword;
     Gparser_constructor.constant "FILE";
   ];;

let prsr_for_possibly_starred_typename=Gparser_homomorphism.chain
   [
     prsr_for_typename;
     prsr_for_inline_white_maybe;
     Gparser_homomorphism.optional(Gparser_constructor.constant "*");
   ];;

let prsr_for_variableword=Gparser_homomorphism.chain
	[
	  Gparser_constructor.simple_char "abcdefghijklmnopqrstuvwxyz_";
	  Gparser_constructor.simple_star "abcdefghijklmnopqrstuvwxyz_0123456789"
	];;

let prsr_for_identword=Gparser_homomorphism.chain
	[
	  Gparser_constructor.simple_char "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	  Gparser_constructor.simple_star "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
	];;

let prsr_for_uppercase_word=Gparser_constructor.simple_plus "_ABCDEFGHIJKLMNOPQRSTUVWXYZ";; 

let prsr_for_negative_int=Gparser_homomorphism.chain
	[
	  Gparser_constructor.constant "-";
	  Gparser_constructor.simple_star "0123456789"
	];;


let prsr_for_rvalue=Gparser_homomorphism.disjunction
   [
     prsr_for_identword;
     prsr_for_negative_int;
     Gparser_constructor.simple_plus "0123456789";
     prsr_for_braced;
   ];;

let prsr_for_vardecl=Gparser_homomorphism.chain
   [
      
      prsr_for_possibly_starred_typename;
      prsr_for_inline_white_maybe;
      prsr_for_variableword;
      prsr_for_inline_white_maybe;
      Gparser_constructor.constant "=";
      prsr_for_inline_white_maybe;
      prsr_for_rvalue;
      prsr_for_inline_white_maybe;
      Gparser_constructor.constant ";";
   ];;

let prsr_for_def_directive=Gparser_homomorphism.chain
      [
        Gparser_constructor.constant "\n#";
        prsr_for_inline_white_maybe;
        Gparser_constructor.constant "define";
        prsr_for_space_or_tab;
        Gparser_constructor.race ("\\\n","\n");
      ];;

let prsr_for_lonely_def_directive=Gparser_homomorphism.chain
      [
        Gparser_constructor.constant "\n#";
        prsr_for_inline_white_maybe;
        Gparser_constructor.constant "define";
        prsr_for_space_or_tab;
        prsr_for_inline_white_maybe;
        prsr_for_uppercase_word;
        Gparser_constructor.footless_constant "\n";
      ];;

let prsr_for_undef_directive=Gparser_homomorphism.chain
      [
        Gparser_constructor.constant "\n#";
        prsr_for_inline_white_maybe;
        Gparser_constructor.constant "undef";
        prsr_for_space_or_tab;
        prsr_for_inline_white_maybe;
        prsr_for_uppercase_word;
        Gparser_constructor.footless_constant "\n";
      ];;

let prsr_for_typename_inliner1=Gparser_homomorphism.chain
     [
       Gparser_constructor.constant "__inline";
       prsr_for_white;
       Gparser_constructor.constant "static";
     ];;

let prsr_for_typename_inliner2=Gparser_homomorphism.chain
     [
       Gparser_constructor.constant "static";
       prsr_for_white;
       Gparser_constructor.constant "inline";
       prsr_for_white;
       Gparser_constructor.constant "const";
     ];;



let prsr_for_typename_inliner=Gparser_homomorphism.disjunction
     [
       prsr_for_typename_inliner1;
       prsr_for_typename_inliner2;
       Gparser_constructor.constant "__inline";
       Gparser_constructor.constant "static";
       Gparser_constructor.constant "extern";
     ];;

let prsr_for_whitened_typename_inliner=Gparser_homomorphism.chain
    [
      prsr_for_typename_inliner;
      prsr_for_white;
    ];;

let prsr_for_fundecl1=
     Gparser_homomorphism.chain
   [
      
      Gparser_homomorphism.optional(prsr_for_whitened_typename_inliner);
      prsr_for_possibly_starred_typename;
      prsr_for_white_maybe;
      prsr_for_variableword;
      prsr_for_white;
      Gparser_homomorphism.optional(prsr_for_uppercase_word);
      prsr_for_white_maybe;
      Gparser_constructor.constant "(";
      Gparser_constructor.house_with_doors ("(",")") [];
      prsr_for_white_maybe;
      Gparser_homomorphism.optional(Gparser_constructor.constant "internal_function");
      Gparser_constructor.constant ";";
   ];;

let prsr_for_fundecl=
   Gparser_homomorphism.disjunction
    [
      prsr_for_fundecl1;
    ];;

let prsr_for_fundef=
     Gparser_homomorphism.chain
   [
      
      Gparser_homomorphism.optional(prsr_for_whitened_typename_inliner);
      prsr_for_possibly_starred_typename;
      prsr_for_white_maybe;
      Gparser_homomorphism.optional(Gparser_constructor.constant "internal_function");
      prsr_for_white_maybe;
      prsr_for_variableword;
      prsr_for_white;
      Gparser_constructor.enclosure ("(",")");
      prsr_for_white;
      Gparser_constructor.simple_negstar "{";     
      prsr_for_braced;
   ];;

let prsr_for_structdef=
     Gparser_homomorphism.chain
   [
      Gparser_constructor.constant "struct";
      prsr_for_white;
      prsr_for_typeword;
      prsr_for_white_maybe;
      prsr_for_braced;
      prsr_for_white_maybe;
      Gparser_constructor.constant ";";
   ];;

let elt_prsr=
   Gparser_homomorphism.disjunction
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
   Gparser_homomorphism.star elt_prsr;;



   
