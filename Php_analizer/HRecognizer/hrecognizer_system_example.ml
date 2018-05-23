(*

#use"Php_analizer/HRecognizer/hrecognizer_system_example.ml";;

Temporary copy of hrecognize module.


*)

(*
let walker=ref(Hrecognizer_system.empty_one );;

let ak avdbl name l=(walker:=Hrecognizer_system.add_avoidable_item (!walker) avdbl name l);;
let aa v=(walker:=Hrecognizer_system.add_atom (!walker) v);;
let au v=(walker:=Hrecognizer_system.add_unlabelled (!walker) v);;
let al v=(walker:=Hrecognizer_system.add_labelled (!walker) v);;

let c x y=aa (x,Atomic_hrecognizer.constant y);;
let lc x y=aa (x,Atomic_hrecognizer.later_constant y);;
let st x y=aa (x,Atomic_hrecognizer.star y);;
let sto x y=aa (x,Atomic_hrecognizer.star_outside y);;
let enc x y=aa (x,Atomic_hrecognizer.enclosed y);;

let sq=aa ("sq",Atomic_hrecognizer.simple_quoted);;
let dq=aa ("dq",Atomic_hrecognizer.double_quoted);;           
let ch x l=al (Abstractified_nonatomic_hrecognizer.Chain(x,l));;  
let uch x l=au (Abstractified_nonatomic_hrecognizer.Chain(x,l));;   
let dis x ll=al (Abstractified_nonatomic_hrecognizer.Disjunction_of_chains(x,ll));;
let udis x ll=au (Abstractified_nonatomic_hrecognizer.Disjunction_of_chains(x,ll));;  


let star x y=au (Abstractified_nonatomic_hrecognizer.Star(x,y));;
let maybe x y =au (Abstractified_nonatomic_hrecognizer.Maybe(x,y));;
let keyword_avoider lbl x y=au (Abstractified_nonatomic_hrecognizer.Avoider(x,y,lbl));;


(* Particular parser elements *)

c "ampersand" "&";;
c "backslash" "\\";;
c "colon" ":";;
c "comma" ",";;
c "dollar" "$";;
c "equals" "=";;
c "exclaim" "!";;
c "larger" ">";;
c "linebreak" "\n";;
c "minus" "-";;
c "plus" "+";;
c "point" ".";;
c "question_mark" "?";;
c "rounded_at_symbol" "@";;
c "semicolon" ";";;    
c "slash" "/";;
c "space" " ";;
c "smaller" "<";;
c "tab" "\t";;
c "times" "*";;
c "tilda" "~";;
c "vline" "|";;
c "windows_linebreak" "\r";;


let avoider_label_for_keywords=Avoider_label.of_string "keyword";;

ak avoider_label_for_keywords "abstract_kwd" ["abstract"];;
ak avoider_label_for_keywords "array_kwd" ["array"];;
ak avoider_label_for_keywords "catch_kwd" ["catch"];;
ak avoider_label_for_keywords "const_kwd" ["const"];;
ak avoider_label_for_keywords "define_kwd" ["define"];;
ak avoider_label_for_keywords "do_kwd" ["do"];;
ak avoider_label_for_keywords "echo_kwd" ["echo"];;
ak avoider_label_for_keywords "extends_kwd" ["extends"];;
ak avoider_label_for_keywords "false_kwd" ["false"];;
ak avoider_label_for_keywords "for_kwd" ["for"];;
ak avoider_label_for_keywords "foreach_kwd" ["for";"each"];;
ak avoider_label_for_keywords "final_kwd" ["final"];;
ak avoider_label_for_keywords "finally_kwd" ["finally"];;
ak avoider_label_for_keywords "function_kwd" ["function"];;
ak avoider_label_for_keywords "global_kwd" ["global"];;
ak avoider_label_for_keywords "class_kwd" ["class"];;
ak avoider_label_for_keywords "include_kwd" ["include"];;
ak avoider_label_for_keywords "include_once_kwd" ["include";"_once"];;
ak avoider_label_for_keywords "interface_kwd" ["interface"];;
ak avoider_label_for_keywords "new_kwd" ["new"];;
ak avoider_label_for_keywords "null_kwd" ["null"];;
ak avoider_label_for_keywords "NULL_kwd" ["NULL"];;
ak avoider_label_for_keywords "namespace_kwd" ["namespace"];;
ak avoider_label_for_keywords "private_kwd" ["private"];;
ak avoider_label_for_keywords "protected_kwd" ["protected"];;
ak avoider_label_for_keywords "public_kwd" ["public"];;
ak avoider_label_for_keywords "require_kwd" ["require"];;
ak avoider_label_for_keywords "require_once_kwd" ["require";"_once"];;
ak avoider_label_for_keywords "return_kwd" ["return"];;
ak avoider_label_for_keywords "static_kwd" ["static"];;
ak avoider_label_for_keywords "switch_kwd" ["switch"];;
ak avoider_label_for_keywords "throw_kwd" ["throw"];;
ak avoider_label_for_keywords "true_kwd" ["true"];;
ak avoider_label_for_keywords "try_kwd" ["try"];;
ak avoider_label_for_keywords "var_kwd" ["var"];;
ak avoider_label_for_keywords "while_kwd" ["while"];;
ak avoider_label_for_keywords "use_kwd" ["use"];;


udis "falsy" [["backslash";"false_kwd"];["false_kwd"]];;
udis "truthy" [["backslash";"true_kwd"];["true_kwd"]];;

enc  "naive_paren_block" ('(',')') ;;
enc  "brace_block" ('{','}') ;;
enc  "bracket_block" ('[',']') ;;
udis "one_white" [["space"];["linebreak"];["windows_linebreak"];["tab"]];;
st "whites"    [' ';'\n';'\r';'\t'];;
ch "white_spot" ["one_white";"whites"];;

maybe "possible_bracket_block" "bracket_block";;

c "zero" "0";;

Image.image(
   fun cha->let s=String.make 1 cha in
    c ("just_the_letter_"^s) s
) Charset.php_label_first_letters;;

udis "first_letter_in_php_name"
  (Image.image(
   fun cha->let s=String.make 1 cha in
    ["just_the_letter_"^s]
) Charset.php_label_first_letters);;

let avoider_label_for_coercers=Avoider_label.of_string "coercer";;

ak avoider_label_for_coercers "coerce_to_array"  ["(array)"];;
ak avoider_label_for_coercers "coerce_to_bool"   ["(bool)"];;
ak avoider_label_for_coercers "coerce_to_int"    ["(int)"];;
ak avoider_label_for_coercers "coerce_to_string" ["(string)"];;

keyword_avoider avoider_label_for_coercers "paren_block" "naive_paren_block" ;;

st "alphanumeric_word" Charset.php_label_nonfirst_letters;;
ch "naive_php_name"
    [
      "first_letter_in_php_name";
      "alphanumeric_word";
    ];;
keyword_avoider avoider_label_for_keywords "php_name" "naive_php_name";;
 

uch "php_vname" ["dollar";"naive_php_name"];;
sto "no_semicolon" [';'];;
sto "no_lbrace" ['{'];;
sto "no_linebreak" ['{'];;

uch "double_slash_comment" [

     "slash";
     "slash";
     "no_linebreak";
     "linebreak";
];;


uch "beginning_of_starred_comment" [

     "slash";
     "times";

];;

lc "end_of_starred_comment" "*/";;

uch "starred_comment" [

     "beginning_of_starred_comment";
     "end_of_starred_comment";
];;



udis "ornament" [

     ["starred_comment"];
     ["double_slash_comment"];
     ["white_spot"];

];;

star "possible_ornaments" "ornament";;

maybe "possible_paren_block" "paren_block";;

uch "snake_start" [

     "dollar";
     "naive_php_name";
     "possible_bracket_block";
     "whites";

];;

uch "snippet_in_snake" [

     "arrow";
     "whites";
     "php_name";
     "whites";
     "possible_paren_block";
     "whites";

];;

star "starred_snippet_in_snake" "snippet_in_snake";;

uch "snake" [

     "snake_start";
     "starred_snippet_in_snake";

];;

uch "final_protected" [

     "final_kwd";
     "white_spot";
     "protected_kwd";

];;

uch "final_public" [

     "final_kwd";
     "white_spot";
     "public_kwd";

];;

uch "private_static" [

     "private_kwd";
     "white_spot";
     "static_kwd";

];;

uch "protected_static" [

     "protected_kwd";
     "white_spot";
     "static_kwd";

];;

uch "public_static" [

     "public_kwd";
     "white_spot";
     "static_kwd";

];;

uch "static_protected" [

     "static_kwd";
     "white_spot";
     "protected_kwd";

];;

udis "nonclass_qualifier" [

     ["const_kwd"];
     ["final_protected"];
     ["final_public"];
     ["global_kwd"];
     ["private_static"];
     ["private_kwd"];
     ["protected_static"];
     ["protected_kwd"];
     ["public_static"];
     ["public_kwd"];
     ["static_protected"];
     ["static_public"];
     ["static_kwd"];
     ["var_kwd"];

];;

udis "echoable" [

     ["dq"];
     ["sq"];
     ["snake"];
     ["paren_block"]

];;

uch "snippet_in_namespaced_name" [

     "backslash";
     "php_name";

];;

star "starred_snippet_in_namespaced_name" "snippet_in_namespaced_name";;

uch "snippet_in_naked_vars_list" [

     "comma";
     "whites";
     "php_vname";
     "whites";

];;

star "starred_snippet_in_naked_vars_list" "snippet_in_naked_vars_list";;

uch "naked_vars_list" [

     "php_vname";
     "whites";
     "snippet_in_naked_vars_list";
     "starred_snippet_in_naked_vars_list";

];;

uch "namespaced_name_one" [

     "php_name";
     "starred_snippet_in_namespaced_name";

];;

uch "namespaced_name_two" [

     "backslash";
     "php_name";
     "starred_snippet_in_namespaced_name";

];;

udis "namespaced_name" [

     ["namespaced_name_one"];
     ["namespaced_name_two"];
];;

maybe "possible_namespaced_name" "namespaced_name";;

uch "snippet_in_extension_list" [

     "whites";
     "comma";
     "whites";
     "namespaced_name";

];;

star "extension_list" "snippet_in_extension_list";;

uch "extension_statement" [

     "extends_kwd";
     "white_spot";
     "namespaced_name";
     "extension_list";

];;

maybe "possible_extension_statement" "extension_statement";;

uch "myriam_elt1" [

     "backslash";
     "php_name";
     "paren_block]";

];;

udis "myriam_element" [

     ["myriam_elt1"];
     ["dq"];
     ["php_name"];
     ["paren_block"];
     ["sq"];
     ["snake"];

];;

uch "myriam_snippet" [

     "whites";
     "point";
     "whites";
     "myriam_element";

];;

star "starred_myriam_snippet" "myriam_snippet";;

uch "myriam" [

     "myriam_element";
     "starred_myriam_snippet";

];;

uch "elisabeth_elt1" [

     "null_kwd";
     "whites";
     "exclaim";
     "equals";
     "equals";
     "whites";
     "php_name";
     "paren_block";

];;

udis "elisabeth_element" [

     ["elisabeth_elt1"];
     ["paren_block"];

];;

uch "elisabeth_snippet" [

     "whites";
     "vline";
     "vline";
     "whites";
     "elisabeth_element";

];;

star "starred_elisabeth_snippet" "elisabeth_snippet";;

uch "elisabeth" [

     "elisabeth_element";
     "starred_elisabeth_snippet";

];;

uch "catchlist_snippet" [

     "catch_kwd";
     "whites";
     "paren_block";
     "whites";
     "brace_block";
     "whites";

];;

star "catchlist" "catchlist_snippet";;

uch "finally_block" [

     "finally_kwd";
     "whites";
     "paren_block";
     "whites";
     "brace_block";
     "whites";

];;

maybe "possible_finally_block" "finally_block";;

c  "just_the_digit_0" "0";;

c  "just_the_digit_1" "1";;

c  "just_the_digit_2" "2";;

c  "just_the_digit_3" "3";;

c  "just_the_digit_4" "4";;

c  "just_the_digit_5" "5";;

c  "just_the_digit_6" "6";;

c  "just_the_digit_7" "7";;

c  "just_the_digit_8" "8";;

c  "just_the_digit_9" "9";;

udis "nonzero_digit" [

     ["just_the_digit_1"];
     ["just_the_digit_2"];
     ["just_the_digit_3"];
     ["just_the_digit_4"];
     ["just_the_digit_5"];
     ["just_the_digit_6"];
     ["just_the_digit_7"];
     ["just_the_digit_8"];
     ["just_the_digit_9"];

];;

udis "digit" [

     ["just_the_digit_0"];
     ["just_the_digit_1"];
     ["just_the_digit_2"];
     ["just_the_digit_3"];
     ["just_the_digit_4"];
     ["just_the_digit_5"];
     ["just_the_digit_6"];
     ["just_the_digit_7"];
     ["just_the_digit_8"];
     ["just_the_digit_9"];
    

];;

star "digits" "digit";;

uch "positive_integer" [

     "nonzero_digit";
     "digits";

];;

uch "negative_integer" [

     "minus";
     "positive_integer";

];;

udis "integer" [

     ["positive_integer"];
     ["negative_integer"];

];;

c  "lowercase_x" "x";;

c  "uppercase_x" "X";;

udis "caseless_x" [

     ["lowercase_x"];
     ["uppercase_x"];

];;

st  "hexadecimal_range" ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9';
'a'; 'b'; 'c'; 'd'; 'e'; 'f';
'A'; 'B'; 'C'; 'D'; 'E'; 'F';
];;

uch "hexadecimal_number" [

     "zero";
     "caseless_x";
     "hexadecimal_range";

];;

maybe "possible_ampersand" "ampersand";;

uch "ampersandable1" [

     "tilda";
     "php_name]";

];;

udis "ampersandable" [

     ["ampersandable1"];
     ["php_name"];

];;

uch "snippet_in_ampersanded" [

     "whites";
     "ampersand";
     "whites";
     "ampersandable";

];;

star "starred_snippet_in_ampersanded" "snippet_in_ampersanded";;

ch "ampersanded"
[
  "ampersandable";
  "starred_snippet_in_ampersanded"
];;

uch "center_of_tripod1" [

     "array_kwd";
     "paren_block";

];;

uch "center_of_tripod2" [

     "php_name";
     "paren_block";

];;

uch "center_of_tripod3" [

     "php_vname";
     "arrow";
     "php_name";
     "bracket_block";

];;

uch "center_of_tripod4" [

     "php_vname";
     "arrow";
     "php_name";
     "paren_block";

];;

uch "center_of_tripod5" [

     "php_vname";
     "arrow";
     "php_name";

];;

uch "center_of_tripod6" [

     "php_vname";
     "white_spot";
     "minus";
     "whites";
     "integer";

];;

udis "center_of_tripod" [

     ["center_of_tripod1"];
     ["null_kwd"];
     ["center_of_tripod2"];
     ["php_name"];
     ["center_of_tripod3"];
     ["center_of_tripod4"];
     ["center_of_tripod5"];
     ["center_of_tripod6"];
     ["php_vname"];
     ["sq"];
     ["true_kwd"];

];;

uch "left_of_tripod1" [

     "array_kwd";
     "paren_block]";

];;

uch "left_of_tripod2" [

     "php_name";
     "paren_block";
     "whites";
     "point";
     "whites";
     "sq]";

];;

uch "left_of_tripod3" [

     "php_name";
     "paren_block]";

];;

uch "left_of_tripod4" [

     "php_vname";
     "bracket_block";
     "bracket_block";

];;

uch "left_of_tripod5" [

     "php_vname";
     "bracket_block";

];;

uch "left_of_tripod6" [

     "php_vname";
     "white_spot";
     "point";
     "whites";
     "php_vname";

];;

udis "left_of_tripod" [

     "left_of_tripod1";
     "false_kwd";
     "integer";
     "paren_block";
     "left_of_tripod2";
     "left_of_tripod3";
     "left_of_tripod4";
     "left_of_tripod5";
     "left_of_tripod6";
     "php_vname";
     "null_kwd";
     "sq";
     "]";

];;

uch "wap" [

     "whites";
     "arrow";
     "php_name]";

];;

udis "declarable" [

     "php_vname";
     "]";

];;

uch "assignee1" [

     "php_vname";
     "arrow";
     "naive_php_name";
     "bracket_block";
     "bracket_block]";

];;

uch "assignee2" [

     "php_vname";
     "arrow";
     "naive_php_name";
     "bracket_block]";

];;

uch "assignee3" [

     "php_vname";
     "arrow";
     "naive_php_name";
     "]";

];;

uch "assignee4" [

     "php_vname";
     "bracket_block";
     "bracket_block]";

];;

uch "assignee5" [

     "php_vname";
     "bracket_block]";

];;

uch "assignee6" [

     "php_vname]";

];;

udis "assignee" [

     "assignee1";
     "assignee2";
     "assignee3";
     "assignee4";
     "assignee5";
     "assignee6
]";

];;

uch "handler" [

     "assignee";
     "whites";
     "equals";
     "whites";
     "]";

];;

star "several_handlers" "handler";;

uch "semicoloned_assignment" [

     "assignee";
     "whites";
     "equals";
     "whites";
     "several_handlers";
     "whites";
     "assignable";
     "whites";
     "semicolon";
     "]";

];;

uch "receiver1" [

     "php_vname";
     "bracket_block]";

];;

udis "receiver" [

     "receiver1
]";

];;

uch "append_assignment" [

     "receiver";
     "whites";
     "point";
     "equals";
     "whites";
     "assignable";
     "whites";
     "semicolon
]";

];;

uch "initialization" [

     "equals";
     "whites";
     "assignable";
     "whites
]";

];;

maybe "possible_initialization" "initialization";;

uch "throwable1" [

     "new_kwd";
     "whites";
     "namespaced_name";
     "paren_block]";

];;

udis "throwable" [

     "throwable1
]";

];;

c  "beginning_of_php_open_tag" "<?php";;

udis "end_of_php_open_tag" [

     "linebreak";
     "space
]";

];;

uch "php_open_tag" [

     "beginning_of_php_open_tag";
     "end_of_php_open_tag";
     "]";

];;

uch "nonrepeatable_inclusion" [

     "include_once_kwd";
     "whites";
     "paren_block";
     "semicolon
]";

];;

uch "repeatable_inclusion" [

     "include_kwd";
     "whites";
     "paren_block";
     "semicolon
]";

];;

uch "nonrepeatable_requirement" [

     "require_once_kwd";
     "whites";
     "paren_block";
     "semicolon
]";

];;

uch "repeatable_requirement" [

     "require_kwd";
     "whites";
     "paren_block";
     "semicolon
]";

];;

let label_for_php_open_tag="php_open_tag";;

add_label label_for_php_open_tag;;

add_recognizer (label_for_php_open_tag,php_open_tag);;

let label_for_starred_comment="starred_comment";;

add_label label_for_starred_comment;;

add_recognizer (label_for_starred_comment,starred_comment);;

let label_for_white_spot="white_spot";;

add_label label_for_white_spot;;

add_recognizer (label_for_white_spot,white_spot);;

let label_for_difyne_constant="difyne_constant";;

add_label label_for_difyne_constant;;

uch "difyne_constant" [

     "define_kwd";
     "whites";
     "paren_block";
     "whites";
     "semicolon";
     "]";

];;

add_recognizer (label_for_difyne_constant,difyne_constant);;

let label_for_nonrepeatable_inclusion="nonrepeatable_inclusion";;

add_label label_for_nonrepeatable_inclusion;;

add_recognizer (label_for_nonrepeatable_inclusion,nonrepeatable_inclusion);;

let label_for_repeatable_inclusion="repeatable_inclusion";;

add_label label_for_repeatable_inclusion;;

add_recognizer (label_for_repeatable_inclusion,repeatable_inclusion);;

let label_for_nonrepeatable_requirement="nonrepeatable_requirement";;

add_label label_for_nonrepeatable_requirement;;

add_recognizer (label_for_nonrepeatable_requirement,nonrepeatable_requirement);;

let label_for_repeatable_requirement="repeatable_requirement";;

add_label label_for_repeatable_requirement;;

add_recognizer (label_for_repeatable_requirement,repeatable_requirement);;

let label_for_double_slash_comment="double_slash_comment";;

add_label label_for_double_slash_comment;;

add_recognizer (label_for_double_slash_comment,double_slash_comment);;

let label_for_semicoloned_fnctn_call="semicoloned_fnctn_call";;

add_label label_for_semicoloned_fnctn_call;;

uch "semicoloned_fnctn_call" [

     "namespaced_name";
     "whites";
     "paren_block";
     "whites";
     "semicolon
]";

];;

add_recognizer (label_for_semicoloned_fnctn_call,semicoloned_fnctn_call);;

let label_for_wiley="wiley";;

add_label label_for_wiley;;

uch "wiley" [

     "while_kwd";
     "whites";
     "paren_block";
     "whites";
     "brace_block";
     "]";

];;

add_recognizer (label_for_wiley,wiley_recognizer);;

uch "snake_with_semicolon" [

     "snake";
     "semicolon
]";

];;

let label_for_snake_with_semicolon="snake_with_semicolon";;

add_label label_for_snake_with_semicolon;;

add_recognizer (label_for_snake_with_semicolon,snake_with_semicolon);;

let label_for_phor_loop="phor_loop";;

add_label label_for_phor_loop;;

uch "phor_loop" [

     "for_kwd";
     "whites";
     "paren_block";
     "whites";
     "brace_block";
     "]";

];;

add_recognizer (label_for_phor_loop,phor_loop);;

let label_for_phoreech_loop="phoreech_loop";;

add_label label_for_phoreech_loop;;

uch "phoreech_loop" [

     "foreach_kwd";
     "whites";
     "paren_block";
     "whites";
     "brace_block";
     "]";

];;

add_recognizer (label_for_phoreech_loop,phoreech_loop);;

sto  "no_semicolon_or_lbrace" [';';'{'];;

let label_for_semicoloned_nspc="semicoloned_nspc";;

add_label label_for_semicoloned_nspc;;

uch "semicoloned_nspc" [

     "nspc_kwd";
     "no_semicolon_or_lbrace";
     "semicolon";
     "]";

];;

add_recognizer (label_for_semicoloned_nspc,semicoloned_nspc);;

let label_for_braced_nspc="braced_nspc";;

add_label label_for_braced_nspc;;

uch "braced_nspc" [

     "nspc_kwd";
     "whites";
     "possible_namespaced_name";
     "whites";
     "brace_block";
     "]";

];;

add_recognizer (label_for_braced_nspc,braced_nspc);;

let label_for_yuze="yuze";;

add_label label_for_yuze;;

uch "yuze" [

     "yuze_kwd";
     "white_spot";
     "no_semicolon";
     "semicolon";
     "]";

];;

add_recognizer (label_for_yuze,yuze);;

let label_for_glass="glass";;

add_label label_for_glass;;

uch "glass" [

     "glass_kwd";
     "white_spot";
     "no_lbrace";
     "brace_block";
     "]";

];;

add_recognizer (label_for_glass,glass_recognizer);;

let label_for_comeback="comeback";;

add_label label_for_comeback;;

uch "comeback" [

     "return_kwd";
     "white_spot";
     "whites";
     "semicolon";
     "]";

];;

add_recognizer (label_for_comeback,comeback);;

let label_for_hurl_exception="hurl_exception";;

add_label label_for_hurl_exception;;

uch "hurl_exception" [

     "throw_kwd";
     "white_spot";
     "throwable";
     "whites";
     "semicolon";
     "]";

];;

add_recognizer (label_for_hurl_exception,hurl_exception);;

let label_for_fnctn="fnctn";;

add_label label_for_fnctn;;

uch "fnctn" [

     "fnctn_kwd";
     "white_spot";
     "php_name";
     "whites";
     "paren_block";
     "whites";
     "brace_block";
     "]";

];;

add_recognizer (label_for_fnctn,fnctn);;

let label_for_itrfc="itrfc";;

add_label label_for_itrfc;;

uch "itrfc" [

     "itrfc_kwd";
     "white_spot";
     "php_name";
     "whites";
     "possible_extension_statement";
     "whites";
     "brace_block";
     "]";

];;

add_recognizer (label_for_itrfc,itrfc);;

let label_for_abstract_glass="abstract_glass";;

add_label label_for_abstract_glass;;

uch "abstract_glass" [

     "abstract_kwd";
     "white_spot";
     "glass_kwd";
     "white_spot";
     "no_lbrace";
     "brace_block";
     "]";

];;

add_recognizer (label_for_abstract_glass,abstract_glass);;

let label_for_final_glass="final_glass";;

add_label label_for_final_glass;;

uch "final_glass" [

     "final_kwd";
     "white_spot";
     "glass_kwd";
     "white_spot";
     "no_lbrace";
     "brace_block";
     "]";

];;

add_recognizer (label_for_final_glass,final_glass);;

let label_for_difyne_carelessly="difyne_carelessly";;

add_label label_for_difyne_carelessly;;

uch "difyne_carelessly" [

     "rounded_at_symbol";
     "define_kwd";
     "whites";
     "paren_block";
     "whites";
     "semicolon";
     "]";

];;

add_recognizer (label_for_difyne_carelessly,difyne_carelessly);;

let label_for_echo="echo";;

add_label label_for_echo;;

uch "echo" [

     "echo_kwd";
     "white_spot";
     "echoable";
     "semicolon";
     "]";

];;

add_recognizer (label_for_echo,echo);;

let label_for_add_array="add_array";;

add_label label_for_add_array;;

uch "add_array" [

     "dollar";
     "naive_php_name";
     "whites";
     "plus";
     "equals";
     "whites";
     "array_kwd";
     "whites";
     "paren_block";
     "whites";
     "semicolon";
     "]";

];;

add_recognizer (label_for_add_array,add_array_recognizer);;

let label_for_trycatch="trycatch";;

add_label label_for_trycatch;;

uch "trycatch" [

     "try_kwd";
     "whites";
     "brace_block";
     "whites";
     "catchlist";
     "whites";
     "possible_finally_block";
     "]";

];;

add_recognizer (label_for_trycatch,trycatch);;

let label_for_dowhile="dowhile";;

add_label label_for_dowhile;;

uch "dowhile" [

     "do_kwd";
     "whites";
     "brace_block";
     "whites";
     "while_kwd";
     "whites";
     "paren_block";
     "whites";
     "semicolon";
     "]";

];;

add_recognizer (label_for_dowhile,dowhile);;

let label_for_sweatch="sweatch";;

add_label label_for_sweatch;;

uch "sweatch" [

     "switch_kwd";
     "whites";
     "paren_block";
     "whites";
     "brace_block";
     "]";

];;

add_recognizer (label_for_sweatch,sweatch);;

let label_for_semicoloned_paamayim_call="semicoloned_paamayim_call";;

add_label label_for_semicoloned_paamayim_call;;

uch "semicoloned_paamayim_call" [

     "namespaced_name";
     "colon";
     "colon";
     "php_name";
     "whites";
     "paren_block";
     "whites";
     "semicolon
]";

];;

add_recognizer (label_for_semicoloned_paamayim_call,semicoloned_paamayim_call);;

let label_for_append_assignment="append_assignment";;

add_label label_for_append_assignment;;

add_recognizer (label_for_append_assignment,append_assignment);;

let label_for_semicoloned_assignment="semicoloned_assignment";;

add_label label_for_semicoloned_assignment;;

add_recognizer (label_for_semicoloned_assignment,semicoloned_assignment);;

let label_for_backslashed_fnctn_call="backslashed_fnctn_call";;

add_label label_for_backslashed_fnctn_call;;

uch "backslashed_fnctn_call" [

     "backslash";
     "php_name";
     "whites";
     "paren_block";
     "whites";
     "semicolon
]";

];;

add_recognizer (label_for_backslashed_fnctn_call,backslashed_fnctn_call);;

let label_for_qualified_declaration="qualified_declaration";;

add_label label_for_qualified_declaration;;

uch "qualified_declaration" [

     "nonclass_qualifier";
     "white_spot";
     "php_vname";
     "whites";
     "possible_initialization";
     "semicolon
]";

];;

add_recognizer (label_for_qualified_declaration,qualified_declaration);;

let label_for_multi_declaration="multi_declaration";;

add_label label_for_multi_declaration;;

uch "multi_declaration" [

     "nonclass_qualifier";
     "white_spot";
     "naked_vars_list";
     "semicolon
]";

];;

add_recognizer (label_for_multi_declaration,multi_declaration);;

let label_for_big_qualified_declaration="big_qualified_declaration";;

add_label label_for_big_qualified_declaration;;

uch "big_qualified_declaration" [

     "nonclass_qualifier";
     "white_spot";
     "php_name";
     "whites";
     "possible_initialization";
     "semicolon
]";

];;

add_recognizer (label_for_big_qualified_declaration,big_qualified_declaration);;

let label_for_qualified_fnctn="qualified_fnctn";;

add_label label_for_qualified_fnctn;;

uch "qualified_fnctn" [

     "nonclass_qualifier";
     "white_spot";
     "fnctn_kwd";
     "white_spot";
     "possible_ampersand";
     "php_name";
     "whites";
     "paren_block";
     "whites";
     "brace_block";
     "]";

];;

add_recognizer (label_for_qualified_fnctn,qualified_fnctn);;

let label_for_abstract_qualified_fnctn="abstract_qualified_fnctn";;

add_label label_for_abstract_qualified_fnctn;;

uch "abstract_qualified_fnctn" [

     "abstract_kwd";
     "white_spot";
     "nonclass_qualifier";
     "white_spot";
     "fnctn_kwd";
     "whites";
     "possible_ampersand";
     "php_name";
     "whites";
     "paren_block";
     "whites";
     "semicolon";
     "]";

];;

add_recognizer (label_for_abstract_qualified_fnctn,abstract_qualified_fnctn);;

let label_for_abstracted_qualified_fnctn="abstracted_qualified_fnctn";;

add_label label_for_abstracted_qualified_fnctn;;

uch "abstracted_qualified_fnctn" [

     "nonclass_qualifier";
     "white_spot";
     "fnctn_kwd";
     "whites";
     "possible_ampersand";
     "php_name";
     "whites";
     "paren_block";
     "whites";
     "semicolon";
     "]";

];;

add_recognizer (label_for_abstracted_qualified_fnctn,abstracted_qualified_fnctn);;








let main_recognizer s i=
  Option.find_and_stop (
     fun (lbl,rcgzr)->Nonatomic_hrecognize.recgz_and_add_label lbl rcgzr s i
  ) (!list_of_recognizers) ;;

let rec main_helper (graet,s,i)=
   match main_recognizer s i with
   None->(i,List.rev graet)
   |Some(lbl,idxs,j)->
        main_helper((lbl,idxs)::graet,s,j);;  

let main_exhauster s i=
   main_helper ([],s,i);;         

exception Parse_failure of string;;

let parse_all s=
   let (j,l)=main_exhauster s 1 in
   let n=String.length s in
   if j<=n
   then let m=min(j+1000)(n) in
        let t=Cull_string.interval s j m in
        raise(Parse_failure(t))
   else l;; 
*)