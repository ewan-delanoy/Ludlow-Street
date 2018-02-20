(*

#use"Php_analizer/HRecognizer/hrecognize.ml";;

*)

(* Label related generic definitions *)

let list_of_labels=ref [];;
exception Duplicate_label of string;;

let add_label lbl =
   let temp1=(!list_of_labels) in
   if List.mem lbl temp1
   then raise(Duplicate_label(lbl))
   else 
   list_of_labels:=lbl::temp1;;

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
  Hregistrar.leaf x (Atomic_hrecognizer.nonempty_star_outside y);;
let enc x y=
    Hregistrar.leaf x (Atomic_hrecognizer.enclosed y);;  
let eo x y=
      Hregistrar.leaf x (Atomic_hrecognizer.exactly_one y);; 
let sq=Hregistrar.leaf "squote" Atomic_hrecognizer.simple_quoted;;
let dq=Hregistrar.leaf "dquote" Atomic_hrecognizer.double_quoted;;           
let ch x l=Hregistrar.chain x l;;   

let star=Hregistrar.star;;

let rlab=Nonatomic_hrecognize.recgz_and_add_label ;;
let rlabch lbl l=rlab lbl
  (ch lbl l);; 

(* Particular parser elements *)

let whites=st "whites"  [' '; '\n'; '\r'; '\t'];;
let paren_block=enc  "paren_block" ('(',')') ;;
let brace_block=enc  "brace_block" ('{','}') ;;

let first_letter=eo "first_letter" Charset.php_label_first_letters;;
let php_name=ch "php_name"
    [
     first_letter;
     st "" Charset.strictly_alphanumeric_characters;
    ];;
let semicolon=c "semicolon" ";";;    
let question_mark=c "question_mark" "?";;
let colon=c "colon" ":";;
let point=c "point" ".";;

let snake_start=
  ch "snake_start"
  [
     c "" "$";
     php_name;
     whites;
  ];;

let snippet_in_snake=
  ch "snippet_in_snake"
  [
     c "" "->";
     whites;
     php_name;
     whites;
     paren_block;
     whites;
  ];;

let snake=
  ch "snake"
   [
     snake_start;
     star "" snippet_in_snake;
   ];;
(*
let ternary_returning_squote=
  ch "ternary_returning_squote"
    [
      paren_block;
      whites;
      question_mark;
      whites;
      sq;
      whites;
      colon;
      whites;
      sq;
    ] ;; 
*)

let myriam_element=Hregistrar.ordered_disjunction
    "myriam_element"
    [
      sq;
      dq;
      php_name;
      paren_block;
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

let assign_to_myriam=
   ch "assign_to_myriam"
   [
      c "" "$";
      php_name;
      c "" "=";
      whites;
      myriam;
      whites;
      semicolon
   ];;

(* End of particular parser elements *)

let label_for_php_open_tag="php_open_tag";;
add_label label_for_php_open_tag;;

let php_open_tag_recognizer=rlab
  label_for_php_open_tag 
  (cli "php_open_tag"
  ["<?php\n";"<?php "]);;

add_recognizer (label_for_php_open_tag,php_open_tag_recognizer);;  

let label_for_comment="comment";;
add_label label_for_comment;;

let comment_recognizer=rlabch
  label_for_comment
  [
    c "" "/*";
    lc "" "*/"
  ];;

add_recognizer (label_for_comment,comment_recognizer);; 

let label_for_white_spot="white_spot";;
add_label label_for_white_spot;;

let white_spot_recognizer=rlab 
  label_for_white_spot
  (
    ne_st "" [' '; '\n'; '\r'; '\t']
  );;

add_recognizer (label_for_white_spot,white_spot_recognizer);; 

let label_for_difyne_constant="difyne_constant";;
add_label label_for_difyne_constant;;

let difyne_constant_recognizer=rlabch
  label_for_difyne_constant
  [
     c "" "define";
     whites;
     paren_block;
     whites;
     c "" ";"
  ];;

add_recognizer (label_for_difyne_constant,difyne_constant_recognizer);; 

let label_for_one_liner_with_variable="one_liner_with_variable";;
add_label label_for_one_liner_with_variable;;

let one_liner_with_variable_recognizer=rlabch
  label_for_one_liner_with_variable
  [
     c "" "$";
     first_letter;
     st "" Charset.strictly_alphanumeric_characters;
     sto "" ['\n';'\r';';'];
     semicolon
  ];;

add_recognizer (label_for_one_liner_with_variable,one_liner_with_variable_recognizer);; 

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

let double_slash_comment_recognizer=rlabch
  label_for_double_slash_comment
  [
    c "" "//";
    sto "" ['\n'];
    c "" "\n"
  ];;

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
    whites
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

let label_for_assign_to_array="assign_to_array";;
add_label label_for_assign_to_array;;


let assign_to_array_recognizer=rlabch
  label_for_assign_to_array
  [
     c "" "$";
     php_name;
     c "" "=";
     whites;
     c "" "array";
     paren_block;
     whites;
     semicolon
  ];;

add_recognizer (label_for_assign_to_array,assign_to_array_recognizer);; 

let label_for_fnctn_call="fnctn_call";;
add_label label_for_fnctn_call;;

let fnctn_call_recognizer=rlabch 
  label_for_fnctn_call
  [
     php_name;
     whites;
     paren_block;
     whites;
     semicolon
  ];;

add_recognizer (label_for_fnctn_call,fnctn_call_recognizer);; 

let label_for_assign_to_fnctn_call="assign_to_fnctn_call";;
add_label label_for_assign_to_fnctn_call;;

let assign_to_fnctn_call_recognizer=rlabch 
  label_for_assign_to_fnctn_call
  [
     c "" "$"; 
     php_name;
     whites;
     c "" "=";
     whites;
     php_name;
     whites;
     paren_block;
     whites;
     semicolon
  ];;

add_recognizer (label_for_assign_to_fnctn_call,assign_to_fnctn_call_recognizer);; 

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

let label_for_assign_to_myriam="assign_to_myriam";;
add_label label_for_assign_to_myriam;;


let assign_to_myriam_recognizer=rlab
  label_for_assign_to_myriam
  assign_to_myriam;;

add_recognizer (label_for_assign_to_myriam,assign_to_myriam_recognizer);; 

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

(*

open Hrecognize;;

let reference_for_loaf = ref "hum";;
let viz f=
   let see=(!reference_for_loaf) in
   let opt1=f see 1 in
   let (_,_,j)=Option.unpack opt1 in
   Cull_string.cobeginning (j-1) see;;

let s_ap="~/Documents/Sites/Rachel/public_html/viewtopic.php";;
let ap=Absolute_path.of_string s_ap;;
let text=Io.read_whole_file ap;;
let (i1,_)=main_exhauster text 1;;
let m1=min (String.length text) (i1+400);; 
reference_for_loaf:=Cull_string.interval text i1 m1;;
let see=(!reference_for_loaf);;

let i1=Substring.leftmost_index_of_in see text;;
let li1=Strung.number_of_lines_before text i1;;
let s2=Lines_in_string.interval text 1140 1146;;

let ioi x=Substring.leftmost_index_of_in x s2;;
let ioj x=(ioi x,(ioi x)+(String.length x)-1);;

let w1=itv s2 8 32;;
let w2=itv s2 36 46;;
let w3=itv s2 50 53;;
let w4=itv s2 57 117;;
let w5=itv s2 121 159;;
let w6=itv s2 163 232;;

















*)