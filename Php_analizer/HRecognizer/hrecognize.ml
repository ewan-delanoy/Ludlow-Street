(*

#use"Php_analizer/HRecognizer/hrecognize.ml";;

*)

(* Label related generic definitions *)

let old_list_of_labels=ref [];;
(*
A newer list of labels is to be found in the h pacify_namespaces module
*)

exception Duplicate_label of string;;

let add_label lbl =
   let temp1=(!old_list_of_labels) in
   if List.mem lbl temp1
   then raise(Duplicate_label(lbl))
   else 
   old_list_of_labels:=lbl::temp1;;

let list_of_recognizers=ref[];;
let add_recognizer (lbl,f)=
   (
    list_of_recognizers:=(lbl,f)::(!list_of_recognizers)
   );;

(* end of label related generic definitions *)

let c x y=
    Hregistrar.leaf x (Atomic_hrecognizer.constant y);;
let cli x y=
    Hregistrar.leaf x (Atomic_hrecognizer.constant_list y);;
let lc x y=
    Hregistrar.leaf x (Atomic_hrecognizer.later_constant y);;
let st x y=
    Hregistrar.leaf x (Atomic_hrecognizer.star y);;
let ne_st x y=
    Hregistrar.leaf x (Atomic_hrecognizer.nonempty_star y);;
let sto x y=
  Hregistrar.leaf x (Atomic_hrecognizer.star_outside y);;
let enc x y=
    Hregistrar.leaf x (Atomic_hrecognizer.enclosed y);;  
let eo x y=
      Hregistrar.leaf x (Atomic_hrecognizer.exactly_one y);; 
let sq=Hregistrar.leaf "squote" Atomic_hrecognizer.simple_quoted;;
let dq=Hregistrar.leaf "dquote" Atomic_hrecognizer.double_quoted;;           
let ch x l=Hregistrar.chain x l;;   
let dis x l=Hregistrar.ordered_disjunction x l;;   

let star=Hregistrar.star;;
let maybe=Hregistrar.maybe;;
let keyword_avoider=Hregistrar.keyword_avoider;;


let rlab=Nonatomic_hrecognize.recgz_and_add_label ;;
let rlabch lbl l=rlab lbl
  (ch lbl l);; 

(* Particular parser elements *)

let whites=st "whites"  [' '; '\n'; '\r'; '\t'];;
let naive_paren_block=enc  "naive_paren_block" ('(',')') ;;
let brace_block=enc  "brace_block" ('{','}') ;;
let bracket_block=enc  "bracket_block" ('[',']') ;;
let white_spot=ne_st "white_spot" [' '; '\n'; '\r'; '\t'];;

let possible_bracket_block=maybe "possible_bracket_block" bracket_block;;



let ampersand=c "ampersand" "&";;
let arrow=c "arrow" "->";;
let backslash=c "backslash" "\\";;
let colon=c "colon" ":";;
let dollar=c "dollar" "$";;
let equals=c "equals" "=";;
let linebreak=c "linebreak" "\n";;
let minus=c "minus" "-";;
let plus=c "plus" "+";;
let point=c "point" ".";;
let question_mark=c "question_mark" "?";;
let rounded_at_symbol=c "rounded_at_symbol" "@";;
let semicolon=c "semicolon" ";";;    
let slash=c "slash" "/";;
let tilda=c "tilda" "~";;

let list_of_keywords =ref [];;
let kc x y=
   let _=(list_of_keywords:=y::(!list_of_keywords)) in
   c x y;;

let abstract_kwd=kc "abstract_kwd" "abstract";;
let array_kwd=kc "array_kwd" "array";;
let backslashed_false_kwd=kc "backslashed_false_kwd" "\\false";;
let catch_kwd=kc "catch_kwd" "catch";;
let const_kwd=kc "const_kwd" "const";;
let define_kwd=kc "define_kwd" "define";;
let echo_kwd=kc "echo_kwd" "echo";;
let final_kwd=kc "final_kwd" "final";;
let fnctn_kwd=kc "fnctn_kwd" "function";;
let global_kwd=kc "global_kwd" "global";;
let glass_kwd=kc "glass_kwd" "class";;
let itrfc_kwd=kc "itrfc_kwd" "interface";;
let lowercase_null_kwd=kc "lowercase_null_kwd" "null";;
let new_kwd=kc "new_kwd" "new";;
let nonbackslashed_false_kwd=kc "nonbackslashed_false_kwd" "false";;
let nspc_kwd=kc "nspc_kwd" "namespace";;
let private_kwd=kc "private_kwd" "private";;
let protected_kwd=kc "protected_kwd" "protected";;
let public_kwd=kc "public_kwd" "public";;
let static_kwd=kc "static_kwd" "static";;
let true_kwd=kc "true_kwd" "true";;
let try_kwd=kc "try_kwd" "try";;
let uppercase_null_kwd=kc "uppercase_null_kwd" "NULL";;
let var_kwd=kc "var_kwd" "var";;
let yuze_kwd=kc "yuze_kwd" "use";;

let false_kwd=dis "false_kwd" [backslashed_false_kwd;nonbackslashed_false_kwd];;
let null_kwd=dis "null_kwd" [lowercase_null_kwd;uppercase_null_kwd];;

let naive_php_name=ch "naive_php_name"
    [
      eo "first_letter" Charset.php_label_first_letters;
     st "" Charset.strictly_alphanumeric_characters;
    ];;
let php_name=keyword_avoider 
    "php_name" (naive_php_name,!list_of_keywords);;



let list_of_coercers =ref [];;
let cc x y=
   let _=(list_of_coercers:=y::(!list_of_coercers)) in
   c x y;;

let coerce_to_array=cc "coerce_to_array" "(array)";;   
let coerce_to_int=cc "coerce_to_int" "(int)";;   

let paren_block=keyword_avoider 
    "paren_block" (naive_paren_block,!list_of_coercers);;






let php_vname=ch "php_vname"
   [dollar;naive_php_name];;

let no_semicolon=sto "no_semicolon" [';'];;
let no_lbrace=sto "no_lbrace" ['{'];;

let double_slash_comment=
  ch "double_slash_comment"
  [
    slash;
    slash;
    sto "" ['\n'];
    linebreak
  ];;

let starred_comment=
  ch "starred_comment"
  [
    c "beginning_of_starred_comment" "/*";
    lc "end_of_starred_comment" "*/"
  ];;


let ornament=
  dis "ornament"
   [
     starred_comment;
     double_slash_comment;
     white_spot;
   ];;



let possible_ornaments=
   star "possible_ornaments" ornament;;

let possible_paren_block=
   maybe "possible_paren_block" paren_block;;   

let snake_start=
  ch "snake_start"
  [
     dollar;
     naive_php_name;
     possible_bracket_block;
     whites;
  ];;

let snippet_in_snake=
  ch "snippet_in_snake"
  [
     arrow;
     whites;
     php_name;
     whites;
     possible_paren_block;
     whites;
  ];;

let starred_snippet_in_snake=star "starred_snippet_in_snake" snippet_in_snake;;

let snake=
  ch "snake"
   [
     snake_start;
     starred_snippet_in_snake;
   ];;

let echoable=
    dis "echoable"
    [
      dq;
      sq;
      snake;
      paren_block
    ] ;;

    let snippet_in_namespaced_name=
      ch "snippet_in_namespaced_name"
      [
        backslash;
        php_name;
      ];;
  
  let starred_snippet_in_namespaced_name=
    star "starred_snippet_in_namespaced_name" snippet_in_namespaced_name;;
  
  let namespaced_name_one=
    ch "namespaced_name_one"
    [
       php_name ;
       starred_snippet_in_namespaced_name;
    ];;    
  
  let namespaced_name_two=
      ch "namespaced_name_two"
      [
         backslash;
         php_name ;
         starred_snippet_in_namespaced_name;
      ];;   
  
  let namespaced_name=
      dis "namespaced_name"
      [
        namespaced_name_one;
        namespaced_name_two;
      ];;        


let myriam_element=dis
    "myriam_element"
    [
      ch "myriam_elt1" [backslash;php_name;paren_block];
      dq;
      php_name;
      paren_block;
      sq;
      snake;

    ];;

let myriam_snippet=
    ch "myriam_snippet"
    [
       whites;
       point;
       whites;
       myriam_element;
    ];;

let myriam=
   ch "myriam"
   [
      myriam_element ;
      star "" myriam_snippet;
   ];;    




let positive_integer=
     ne_st "positive_integer" ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'];;

let negative_integer=
    ch "negative_integer" [minus;positive_integer];;
    
let integer=dis "integer" [positive_integer;negative_integer];;    

let ampersandable=dis "ampersandable"
    [
      ch "ampersandable1" [tilda;php_name];
                           php_name;
    ];;

let snippet_in_ampersanded=ch "snippet_in_ampersanded"
    [
       whites;
       ampersand;
       whites;
       ampersandable; 
       
    ] ;;   

let ampersanded=ch "ampersanded"
    [
      ampersandable;
      star "starred_snippet_in_ampersanded" snippet_in_ampersanded;
    ];;

let center_of_tripod=dis "center_of_tripod"
   [
     (ch "center_of_tripod1") [array_kwd;paren_block];
     (ch "center_of_tripod2") [php_name;paren_block];
                               php_name;
     (ch "center_of_tripod3") [php_vname;arrow;php_name;bracket_block];
     (ch "center_of_tripod4") [php_vname;arrow;php_name;paren_block];
     (ch "center_of_tripod5") [php_vname;arrow;php_name];
     (ch "center_of_tripod6") [php_vname;white_spot;minus;whites;integer];
                               php_vname;
                               sq;
                               true_kwd;
   ];;

let left_of_tripod=dis "left_of_tripod"
   [
                             false_kwd;
                             integer;
     
     (ch "left_of_tripod1") [php_name;paren_block;whites;point;whites;sq];
     (ch "left_of_tripod2") [php_name;paren_block];
     (ch "left_of_tripod3") [php_vname;bracket_block;bracket_block];
     (ch "left_of_tripod4") [php_vname;white_spot;point;whites;php_vname];
                             php_vname; 
                             null_kwd;
                             sq;
   ];;   


let wap=ch "wap" [whites;arrow;php_name];;


let assignable=
   dis "assignable"
    [ 
      (ch "one_array"            [array_kwd;whites;paren_block]);   
      (ch "assignable1"          [coerce_to_array;whites;php_vname]);   
      (ch "assignable2"          [coerce_to_int;whites;php_vname;arrow;php_name;paren_block]); 
      (ch "assignable3"          [coerce_to_int;whites;php_vname;bracket_block]); 
                                  dq; 
                                  false_kwd;
      (ch "floater"              [integer;point;positive_integer]);
                                  integer;
      (ch "paamayim_call"        [namespaced_name;colon;colon;php_name;paren_block]);
      (ch "paamayim_simple_call" [namespaced_name;colon;colon;php_name]); 
      (ch "tripod1"              [namespaced_name;paren_block;whites;equals;equals;whites;sq;whites;question_mark;whites;center_of_tripod;whites;colon;whites;left_of_tripod]); 
      (ch "tripod2"              [namespaced_name;paren_block;whites;question_mark;whites;center_of_tripod;whites;colon;whites;left_of_tripod]); 
      (ch "fnctn_call_minus_int" [namespaced_name;paren_block;whites;minus;whites;integer;]);
      (ch "fnctn_call_dot_sq"    [namespaced_name;paren_block;whites;point;whites;sq;]);
      (ch "fnctn_call_plus_sth"  [namespaced_name;paren_block;whites;plus;whites;php_vname;]);
      (ch "fnctn_call"           [namespaced_name;paren_block]);
      (ch "ampersanded_item"     [namespaced_name;white_spot;ampersand;whites;ampersanded]);
      (ch "new_fnctn_call"       [new_kwd;white_spot;namespaced_name;whites;paren_block]);
      (ch "new_vfnctn_call"      [new_kwd;white_spot;php_vname;whites;paren_block]);   
      (ch "new_meth_call"        [new_kwd;white_spot;php_vname;whites;arrow;php_name]); 
                                  null_kwd;                              
      (ch "tripod3"              [paren_block;whites;question_mark;whites;center_of_tripod;whites;colon;whites;left_of_tripod]); 
      (ch "assignable4"          [php_vname;bracket_block;white_spot;point;whites;myriam]); 
      (ch "assignable5"          [php_vname;bracket_block]); 
      (ch "assignable6"          [php_vname;wap;arrow;php_name;paren_block]);
      (ch "assignable7"          [php_vname;wap;bracket_block]);
      (ch "vnctn_call_minus_int" [php_vname;wap;paren_block;whites;minus;whites;integer;]);
      (ch "assignable8"          [php_vname;wap;paren_block;wap;paren_block;white_spot;arrow;php_name;whites;possible_paren_block;whites;starred_snippet_in_snake]);
      (ch "assignable9"          [php_vname;wap;paren_block;wap;paren_block;white_spot;point;whites;php_vname]);
      (ch "assignable10"         [php_vname;wap;paren_block;wap;paren_block]);
      (ch "assignable11"         [php_vname;wap;paren_block]);
      (ch "assignable12"         [php_vname;wap;white_spot;point;whites;myriam]);
      (ch "assignable13"         [php_vname;wap]);
      (ch "assignable14"         [php_vname;whites;point;whites;myriam]);
                                  php_vname; 
      (ch "dotted_line"          [sq;whites;point;whites;myriam]);
                                  sq;
                                  true_kwd;
    ] ;;   

let arrowing=ch "arrowing" [arrow;php_name];;
let possible_arrowing=maybe "possible_arrowing" arrowing;;
      
let handler=
  ch "handler"
  [
    dollar;
    naive_php_name;
    possible_arrowing;
    possible_bracket_block;
    possible_bracket_block;
    whites;
    equals;
    whites;
  ];;

let several_handlers =
   star "several_handlers" handler;;
  
let semicoloned_assignment=
    ch "semicoloned_assignment"
    [
       handler;
       several_handlers;
       assignable;
       whites;
       semicolon;
    ];;

let receiver=
      dis "receiver"
      [
         ch "receiver1" [php_vname;bracket_block]
      ] ;;   

let append_assignment=
    ch "append_assignment"    
    [
      
      receiver;
      whites;
      point;
      equals;
      whites;
      assignable;
      whites;
      semicolon
   ];;

let initialization=
  ch "initialization"
  [
    equals;
    whites;
    assignable;
    whites
  ];;

let possible_initialization = maybe "possible_initialization" initialization;;  

(* End of particular parser elements *)

let label_for_one_liner_with_variable="one_liner_with_variable";;
add_label label_for_one_liner_with_variable;;

let one_liner_with_variable_recognizer=rlabch
  label_for_one_liner_with_variable
  [
     dollar;
     naive_php_name;
     sto "" ['\n';'\r';';'];
     semicolon
  ];;

add_recognizer (label_for_one_liner_with_variable,one_liner_with_variable_recognizer);; 


let label_for_php_open_tag="php_open_tag";;
add_label label_for_php_open_tag;;

let php_open_tag_recognizer=rlab
  label_for_php_open_tag 
  (cli "php_open_tag"
  ["<?php\n";"<?php "]);;

add_recognizer (label_for_php_open_tag,php_open_tag_recognizer);;  

let label_for_starred_comment="starred_comment";;
add_label label_for_starred_comment;;

let starred_comment_recognizer=rlab
  label_for_starred_comment
  starred_comment;;

add_recognizer (label_for_starred_comment,starred_comment_recognizer);; 

let label_for_white_spot="white_spot";;
add_label label_for_white_spot;;

let white_spot_recognizer=rlab 
  label_for_white_spot
  white_spot;;

add_recognizer (label_for_white_spot,white_spot_recognizer);; 

let label_for_difyne_constant="difyne_constant";;
add_label label_for_difyne_constant;;

let difyne_constant_recognizer=rlabch
  label_for_difyne_constant
  [
     define_kwd;
     whites;
     paren_block;
     whites;
     semicolon;
  ];;

add_recognizer (label_for_difyne_constant,difyne_constant_recognizer);; 



let label_for_inclusion_with_parenthesis="inclusion_with_parenthesis";;
add_label label_for_inclusion_with_parenthesis;;

let inclusion_with_parenthesis_recognizer=rlabch
  label_for_inclusion_with_parenthesis
  [
    cli "" ["include_once";"require_once";"include";"require"];
    whites;
    paren_block;
    semicolon
  ];;

add_recognizer (label_for_inclusion_with_parenthesis,inclusion_with_parenthesis_recognizer);; 

let label_for_double_slash_comment="double_slash_comment";;
add_label label_for_double_slash_comment;;

let double_slash_comment_recognizer=rlab
  label_for_double_slash_comment
  double_slash_comment;;

add_recognizer (label_for_double_slash_comment,double_slash_comment_recognizer);; 


(* the fst_kwd parameter is either if or elseif or else+if *)

let ivy_start_partial_recognizer fst_kwd=
  Nonatomic_hrecognize.recgz
  (ch ""
  [
    c "" fst_kwd;
    whites;
    paren_block;
    whites;
    brace_block;
    possible_ornaments
  ]);;

(*

ivy_start_partial_recognizer "if" "if (567) {123} 678" 1;;
ivy_start_partial_recognizer "elseif" "elseif (abc) {def} ghi" 1;;

*)

let elsie_partial_recognizer=
  Nonatomic_hrecognize.recgz
  (ch ""
  [
    c "" "else";
    whites;
    brace_block;
    whites
  ]);;

let label_for_ive="ive";;
add_label label_for_ive;;

let label_for_ivy="ivy";;
add_label label_for_ivy;;



let rec ivy_iterator_partial_recognizer (s,i0,i)=
   let opt1=elsie_partial_recognizer s i in
   if opt1<>None
   then let next_i=Option.unpack opt1 in
        Some(label_for_ive,(i0,next_i-1),next_i)
   else 
   let opt2=ivy_start_partial_recognizer "elseif" s i in
   let opt3=(
      if opt2=None
      then ivy_start_partial_recognizer "else if" s i
      else opt2
   ) in
   if opt3=None
   then Some(label_for_ivy,(i0,i-1),i)
   else 
   let j6=Option.unpack opt3 in
   ivy_iterator_partial_recognizer (s,i0,j6);;

let label_for_ive_or_ivy="ive_or_ivy";;
   add_label label_for_ive_or_ivy;;
  
let ive_or_ivy_recognizer s i=
    let opt1=ivy_start_partial_recognizer "if" s i in
    if opt1=None then None else
    let i6=Option.unpack opt1 in
    ivy_iterator_partial_recognizer (s,i,i6);;

add_recognizer (label_for_ive_or_ivy,ive_or_ivy_recognizer);; 

(*

ive_or_ivy_recognizer "if (ab) {c} elseif (de) {fg} elseif(h){ij} else{kl} m" 1;;
ive_or_ivy_recognizer "if (ab) {c} elseif (de) {fg} elseif(h){ij}m" 1;;

ive_or_ivy_recognizer "if (!$topic_id && !$post_id)\n{\n\ttrigger_error('NO_TOPIC');\n}m" 1;;

let example=
  "if (!$post_id)\n{\n\t$sql_array['WHERE'] = \"t.topic_id = "^
  "$topic_id\";\n}\nelse\n{\n\t$sql_array['WHERE'] = \"p.post_id "^
  "= $post_id AND t.topic_id = p.topic_id\";\n}\n\n";;



*)



let label_for_semicoloned_fnctn_call="semicoloned_fnctn_call";;
add_label label_for_semicoloned_fnctn_call;;

let semicoloned_fnctn_call_recognizer=rlabch 
  label_for_semicoloned_fnctn_call
  [
     namespaced_name;
     whites;
     paren_block;
     whites;
     semicolon
  ];;

add_recognizer (label_for_semicoloned_fnctn_call,semicoloned_fnctn_call_recognizer);; 


let label_for_wiley="wiley";;
add_label label_for_wiley;;


let wiley_recognizer=rlabch 
  label_for_wiley
  [
     c "" "while";
     whites;
     paren_block;
     whites;
     brace_block;
  ];;

add_recognizer (label_for_wiley,wiley_recognizer);; 

let snake_with_semicolon=
  ch "snake_with_semicolon"
   [
     snake;
     semicolon
   ];;

let label_for_snake_with_semicolon="snake_with_semicolon";;
add_label label_for_snake_with_semicolon;;  


let snake__with_semicolon_recognizer=rlab 
 label_for_snake_with_semicolon 
  snake_with_semicolon;;

add_recognizer (label_for_snake_with_semicolon,snake__with_semicolon_recognizer);; 

let label_for_phor="phor";;
add_label label_for_phor;;

let phor_recognizer=rlabch
  label_for_phor
  [
     c "" "for";
     whites;
     paren_block;
     whites;
     brace_block;
  ];;


add_recognizer (label_for_phor,phor_recognizer);; 

let label_for_phoreech="phoreech";;
add_label label_for_phoreech;;


let phoreech_recognizer=rlabch
  label_for_phoreech
  [
     c "" "foreach";
     whites;
     paren_block;
     whites;
     brace_block;
  ];;

add_recognizer (label_for_phoreech,phoreech_recognizer);; 

let label_for_semicoloned_nspc="semicoloned_nspc";;
add_label label_for_semicoloned_nspc;;



let semicoloned_nspc_recognizer=rlabch
  label_for_semicoloned_nspc
  [
     nspc_kwd;
     sto "no_semicolon_or_lbrace" [';';'{'];
     semicolon;
  ];;

add_recognizer (label_for_semicoloned_nspc,semicoloned_nspc_recognizer);; 


let label_for_braced_nspc="braced_nspc";;
add_label label_for_braced_nspc;;


let braced_nspc_recognizer=rlabch
  label_for_braced_nspc
  [
     nspc_kwd;
     no_lbrace;
     brace_block;
  ];;

add_recognizer (label_for_braced_nspc,braced_nspc_recognizer);; 

let label_for_yuze="yuze";;
add_label label_for_yuze;;

let yuze_recognizer=rlabch
  label_for_yuze
  [
     yuze_kwd;
     white_spot;
     no_semicolon;
     semicolon;
  ];;


add_recognizer (label_for_yuze,yuze_recognizer);; 

let label_for_glass="glass";;
add_label label_for_glass;;

let glass_recognizer=rlabch
  label_for_glass
  [
     glass_kwd;
     white_spot;
     no_lbrace;
     brace_block;
  ];;


add_recognizer (label_for_glass,glass_recognizer);; 

let label_for_fnctn="fnctn";;
add_label label_for_fnctn;;

let fnctn_recognizer=rlabch
  label_for_fnctn
  [
     fnctn_kwd;
     white_spot;
     no_lbrace;
     brace_block;
  ];;


add_recognizer (label_for_fnctn,fnctn_recognizer);; 

let label_for_itrfc="itrfc";;
add_label label_for_itrfc;;

let itrfc_recognizer=rlabch
  label_for_itrfc
  [
     itrfc_kwd;
     white_spot;
     no_lbrace;
     brace_block;
  ];;


add_recognizer (label_for_itrfc,itrfc_recognizer);; 

let label_for_abstract_glass="abstract_glass";;
add_label label_for_abstract_glass;;

let abstract_glass_recognizer=rlabch
  label_for_abstract_glass
  [
     abstract_kwd;
     white_spot;
     glass_kwd;
     white_spot;
     no_lbrace;
     brace_block;
  ];;


add_recognizer (label_for_abstract_glass,abstract_glass_recognizer);; 

let label_for_final_glass="final_glass";;
add_label label_for_final_glass;;

let final_glass_recognizer=rlabch
  label_for_final_glass
  [
     final_kwd;
     white_spot;
     glass_kwd;
     white_spot;
     no_lbrace;
     brace_block;
  ];;


add_recognizer (label_for_final_glass,final_glass_recognizer);; 


let label_for_difyne_carelessly="difyne_carelessly";;
add_label label_for_difyne_carelessly;;

let difyne_carelessly_recognizer=rlabch
  label_for_difyne_carelessly
  [
     rounded_at_symbol;
     define_kwd;
     whites;
     paren_block;
     whites;
     semicolon;
  ];;

add_recognizer (label_for_difyne_carelessly,difyne_carelessly_recognizer);; 

let label_for_global_decl="global_decl";;
add_label label_for_global_decl;;

let global_decl_recognizer=rlabch
  label_for_global_decl
  [
     global_kwd;
     white_spot;
     no_semicolon;
     semicolon;
  ];;


add_recognizer (label_for_global_decl,global_decl_recognizer);; 

let label_for_static_decl="static_decl";;
add_label label_for_static_decl;;

let static_decl_recognizer=rlabch
  label_for_static_decl
  [
     static_kwd;
     white_spot;
     no_semicolon;
     semicolon;
  ];;


add_recognizer (label_for_static_decl,static_decl_recognizer);; 

let label_for_echo="echo";;
add_label label_for_echo;;

let echo_recognizer=rlabch
  label_for_echo
  [
     echo_kwd;
     white_spot;
     echoable;
     semicolon;
  ];;

add_recognizer (label_for_echo,echo_recognizer);; 

let label_for_add_array="add_array";;
add_label label_for_add_array;;

let add_array_recognizer=rlabch
  label_for_add_array
  [
     dollar;
     naive_php_name;
     whites;
     plus;
     equals;
     whites;
     array_kwd;
     whites;
     paren_block;
     whites;
     semicolon;
  ];;


add_recognizer (label_for_add_array,add_array_recognizer);; 

let label_for_trycatch="trycatch";;
add_label label_for_trycatch;;

let trycatch_recognizer=rlabch
  label_for_trycatch
  [
     try_kwd;
     whites;
     brace_block;
     whites;
     catch_kwd;
     whites;
     paren_block;
     whites;
     brace_block;
  ];;


add_recognizer (label_for_trycatch,trycatch_recognizer);; 

let label_for_semicoloned_paamayim_call="semicoloned_paamayim_call";;
add_label label_for_semicoloned_paamayim_call;;

let semicoloned_paamayim_call_recognizer=rlabch 
  label_for_semicoloned_paamayim_call
  [
     namespaced_name;
     colon;
     colon;
     php_name;
     whites;
     paren_block;
     whites;
     semicolon
  ];;

add_recognizer (label_for_semicoloned_paamayim_call,semicoloned_paamayim_call_recognizer);; 

let label_for_append_assignment="append_assignment";;

add_label label_for_append_assignment;;

let append_assignment_recognizer=rlab 
  label_for_append_assignment
  append_assignment;;

add_recognizer (label_for_append_assignment,append_assignment_recognizer);; 

let label_for_semicoloned_assignment="semicoloned_assignment";;
add_label label_for_semicoloned_assignment;;


let semicoloned_assignment_recognizer=rlab
  label_for_semicoloned_assignment
  semicoloned_assignment;;

add_recognizer (label_for_semicoloned_assignment,semicoloned_assignment_recognizer);; 


let label_for_backslashed_fnctn_call="backslashed_fnctn_call";;
add_label label_for_backslashed_fnctn_call;;

let backslashed_fnctn_call_recognizer=rlabch 
  label_for_backslashed_fnctn_call
  [
     backslash;
     php_name;
     whites;
     paren_block;
     whites;
     semicolon
  ];;

add_recognizer (label_for_backslashed_fnctn_call,backslashed_fnctn_call_recognizer);; 

let label_for_const_declaration="const_declaration";;
add_label label_for_const_declaration;;

let const_declaration_recognizer=rlabch 
  label_for_const_declaration
  [
     const_kwd;
     white_spot;
     php_vname;
     whites;
     possible_initialization;
     semicolon
  ];;

add_recognizer (label_for_const_declaration,const_declaration_recognizer);; 

let label_for_big_const_declaration="big_const_declaration";;
add_label label_for_big_const_declaration;;

let big_const_declaration_recognizer=rlabch 
  label_for_big_const_declaration
  [
     const_kwd;
     white_spot;
     php_name;
     whites;
     possible_initialization;
     semicolon
  ];;

add_recognizer (label_for_big_const_declaration,big_const_declaration_recognizer);; 

let label_for_private_declaration="private_declaration";;
add_label label_for_private_declaration;;

let private_declaration_recognizer=rlabch 
  label_for_private_declaration
  [
     private_kwd;
     white_spot;
     php_vname;
     whites;
     possible_initialization;
     semicolon
  ];;

add_recognizer (label_for_private_declaration,private_declaration_recognizer);; 

let label_for_protected_declaration="protected_declaration";;
add_label label_for_protected_declaration;;

let protected_declaration_recognizer=rlabch 
  label_for_protected_declaration
  [
     protected_kwd;
     white_spot;
     php_vname;
     whites;
     possible_initialization;
     semicolon
  ];;

add_recognizer (label_for_protected_declaration,protected_declaration_recognizer);; 

let label_for_public_declaration="public_declaration";;
add_label label_for_public_declaration;;

let public_declaration_recognizer=rlabch 
  label_for_public_declaration
  [
     public_kwd;
     white_spot;
     php_vname;
     whites;
     possible_initialization;
     semicolon
  ];;

add_recognizer (label_for_public_declaration,public_declaration_recognizer);; 

let label_for_var_declaration="var_declaration";;
add_label label_for_var_declaration;;

let var_declaration_recognizer=rlabch 
  label_for_var_declaration
  [
     public_kwd;
     white_spot;
     php_vname;
     whites;
     possible_initialization;
     semicolon
  ];;

add_recognizer (label_for_var_declaration,var_declaration_recognizer);; 

let label_for_private_fnctn="private_fnctn";;
add_label label_for_private_fnctn;;

let private_fnctn_recognizer=rlabch
  label_for_private_fnctn
  [
     private_kwd;
     white_spot;
     fnctn_kwd;
     white_spot;
     no_lbrace;
     brace_block;
  ];;


add_recognizer (label_for_private_fnctn,private_fnctn_recognizer);; 

let label_for_protected_fnctn="protected_fnctn";;
add_label label_for_protected_fnctn;;

let protected_fnctn_recognizer=rlabch
  label_for_protected_fnctn
  [
     protected_kwd;
     white_spot;
     fnctn_kwd;
     white_spot;
     no_lbrace;
     brace_block;
  ];;


add_recognizer (label_for_protected_fnctn,protected_fnctn_recognizer);; 

let label_for_public_fnctn="public_fnctn";;
add_label label_for_public_fnctn;;

let public_fnctn_recognizer=rlabch
  label_for_public_fnctn
  [
     public_kwd;
     white_spot;
     fnctn_kwd;
     white_spot;
     no_lbrace;
     brace_block;
  ];;


add_recognizer (label_for_public_fnctn,public_fnctn_recognizer);; 

let main_recognizer s i=
  Option.find_and_stop (
     fun (lbl,f)->f s i
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


