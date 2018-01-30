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

let c=Atomic_hrecognizer.constant;;
let cli=Atomic_hrecognizer.constant_list;;
let lc=Atomic_hrecognizer.later_constant;;
let st=Atomic_hrecognizer.star;;
let sto=Atomic_hrecognizer.star_outside;;



let whites= st [' '; '\n'; '\r'; '\t'];;
let paren_block=Atomic_hrecognizer.enclosed ('(',')');;
let brace_block=Atomic_hrecognizer.enclosed ('{','}');;
let first_letter=Atomic_hrecognizer.exactly_one 
                   Charset.php_label_first_letters;;

let label_for_php_open_tag="php_open_tag";;
add_label label_for_php_open_tag;;

let php_open_tag_recognizer=
  Parametric_hrecognize.chain 
  label_for_php_open_tag 
  [
    cli ["<?php\n";"<?php "]
  ];;

add_recognizer (label_for_php_open_tag,php_open_tag_recognizer);;  

let label_for_comment="comment";;
add_label label_for_comment;;

let comment_recognizer=
  Parametric_hrecognize.chain 
  label_for_comment
  [
    c "/*";
    lc "*/"
  ];;

add_recognizer (label_for_comment,comment_recognizer);; 

let label_for_white_spot="white_spot";;
add_label label_for_white_spot;;



let white_spot_recognizer=
  Parametric_hrecognize.chain 
  label_for_white_spot
  [
    whites
  ];;

add_recognizer (label_for_white_spot,white_spot_recognizer);; 

let label_for_difyne_constant="difyne_constant";;
add_label label_for_difyne_constant;;

let difyne_constant_recognizer=
  Parametric_hrecognize.chain 
  label_for_difyne_constant
  [
     c "define";
     whites;
     paren_block;
     whites;
     c ";"
  ];;

add_recognizer (label_for_difyne_constant,difyne_constant_recognizer);; 

let label_for_one_liner_with_variable="one_liner_with_variable";;
add_label label_for_one_liner_with_variable;;

let one_liner_with_variable_recognizer=
  Parametric_hrecognize.chain 
  label_for_one_liner_with_variable
  [
     c "$";
     first_letter;
     st Charset.strictly_alphanumeric_characters;
     sto ['\n';'\r';';'];
     c ";"
  ];;

add_recognizer (label_for_one_liner_with_variable,one_liner_with_variable_recognizer);; 

let label_for_inclusion_with_parenthesis="inclusion_with_parenthesis";;
add_label label_for_inclusion_with_parenthesis;;

let inclusion_with_parenthesis_recognizer=
  Parametric_hrecognize.chain
  label_for_inclusion_with_parenthesis
  [
    cli ["include_once";"require_once";"include";"require"];
    whites;
    paren_block;
    c ";"
  ];;

add_recognizer (label_for_inclusion_with_parenthesis,inclusion_with_parenthesis_recognizer);; 

let label_for_double_slash_comment="double_slash_comment";;
add_label label_for_double_slash_comment;;

let double_slash_comment_recognizer=
  Parametric_hrecognize.chain
  label_for_double_slash_comment
  [
    c "//";
    sto ['\n'];
    c "\n"
  ];;

add_recognizer (label_for_double_slash_comment,double_slash_comment_recognizer);; 



let label_for_ivy_start_partial="ivy_start_partial";;
add_label label_for_ivy_start_partial;;

(* the fst_kwd parameter is either if or elseif or else+if *)

let ivy_start_partial_recognizer fst_kwd=
  Parametric_hrecognize.chain
  label_for_ivy_start_partial
  [
    c fst_kwd;
    whites;
    paren_block;
    whites;
    brace_block
  ];;

(*

ivy_start_partial_recognizer "if" "if (567) {123} 678" 1;;
ivy_start_partial_recognizer "elseif" "elseif (abc) {def} ghi" 1;;

*)

let label_for_elsie_partial="elsie_partial";;
add_label label_for_elsie_partial;;

let elsie_partial_recognizer=
  Parametric_hrecognize.chain
  label_for_ivy_start_partial
  [
    c "else";
    whites;
    brace_block
  ];;

let label_for_ive="ive";;
add_label label_for_ive;;

let label_for_ivy="ivy";;
add_label label_for_ivy;;



let rec ivy_iterator_partial_recognizer (graet,s,i)=
   let opt1=elsie_partial_recognizer s i in
   if opt1<>None
   then let (_,l,next_i)=Option.unpack opt1 in
        Some(label_for_ive,graet@l,next_i)
   else 
   let opt2=ivy_start_partial_recognizer "elseif" s i in
   let opt3=(
      if opt2=None
      then ivy_start_partial_recognizer "else if" s i
      else opt2
   ) in
   if opt3=None
   then Some(label_for_ivy,graet,i)
   else 
   let (_,l2,j6)=Option.unpack opt3 in
   ivy_iterator_partial_recognizer (graet@l2,s,j6);;

let label_for_ive_or_ivy="ive_or_ivy";;
   add_label label_for_ive_or_ivy;;
  
let ive_or_ivy_recognizer s i=
    let opt1=ivy_start_partial_recognizer "if" s i in
    if opt1=None then None else
    let (_,l0,i6)=Option.unpack opt1 in
    ivy_iterator_partial_recognizer (l0,s,i6);;

add_recognizer (label_for_ive_or_ivy,ive_or_ivy_recognizer);; 

(*

ive_or_ivy_recognizer "if (ab) {c} elseif (de) {fg} elseif(h){ij} else{kl} m" 1;;
ive_or_ivy_recognizer "if (ab) {c} elseif (de) {fg} elseif(h){ij}m" 1;;

ive_or_ivy_recognizer "if (!$topic_id && !$post_id)\n{\n\ttrigger_error('NO_TOPIC');\n}m" 1;;

*)

let label_for_assign_to_array="assign_to_array";;
add_label label_for_assign_to_array;;

let assign_to_array_recognizer=
  Parametric_hrecognize.chain
  label_for_assign_to_array
  [
     c "$";
     first_letter;
     st Charset.strictly_alphanumeric_characters;
     whites;
     c "=";
     whites;
     c "array";
     paren_block;
     whites;
     c ";"
  ];;

add_recognizer (label_for_assign_to_array,assign_to_array_recognizer);; 

let label_for_fnctn_call="fnctn_call";;
add_label label_for_fnctn_call;;

let fnctn_call_recognizer= 
  Parametric_hrecognize.chain
  label_for_fnctn_call
  [
     first_letter;
     st Charset.strictly_alphanumeric_characters;
     whites;
     paren_block;
     whites;
     c ";"
  ];;

add_recognizer (label_for_fnctn_call,fnctn_call_recognizer);; 

let label_for_assign_to_fnctn_call="assign_to_fnctn_call";;
add_label label_for_assign_to_fnctn_call;;

let assign_to_fnctn_call_recognizer= 
  Parametric_hrecognize.chain
  label_for_assign_to_fnctn_call
  [
     c "$"; 
     first_letter;
     st Charset.strictly_alphanumeric_characters;
     whites;
     c "=";
     whites;
     first_letter;
     st Charset.strictly_alphanumeric_characters;
     whites;
     paren_block;
     whites;
     c ";"
  ];;

add_recognizer (label_for_assign_to_fnctn_call,assign_to_fnctn_call_recognizer);; 

let label_for_wiley="wiley";;
add_label label_for_wiley;;


let wiley_recognizer= 
  Parametric_hrecognize.chain
  label_for_wiley
  [
     c "while";
     whites;
     paren_block;
     whites;
     brace_block;
  ];;

add_recognizer (label_for_wiley,wiley_recognizer);; 


let label_for_snippet_in_snake_partial="snippet_in_snake_partial";;
add_label label_for_snippet_in_snake_partial;;

let snippet_in_snake_partial_recognizer=
  Parametric_hrecognize.chain
  label_for_snippet_in_snake_partial
  [
     c "->";
     whites;
     first_letter;
     st Charset.strictly_alphanumeric_characters;
     whites;
     paren_block;
     whites;
  ];;

let label_for_snake_start_partial="snake_start_partial";;
add_label label_for_snake_start_partial;;
  
let snake_start_partial_recognizer=
    Parametric_hrecognize.chain
    label_for_snake_start_partial
    [
       c "$";
       first_letter;
       st Charset.strictly_alphanumeric_characters;
       whites;
    ];;


let label_for_snake="snake";;
add_label label_for_snake;;  

let rec snake_iterator_partial_recognizer (graet,s,i)=
  let opt1=snippet_in_snake_partial_recognizer s i in
  if opt1<>None
  then let (_,interm_results,next_i)=Option.unpack opt1 in
      snake_iterator_partial_recognizer(graet@interm_results,s,next_i)
  else 
  if not(Substring.is_a_substring_located_at ";" s i)
  then None
  else Some(label_for_snake,graet@[i],i+1);;


let snake_recognizer s i=
  let opt1=snake_start_partial_recognizer s i in
  if opt1=None
  then None
  else 
  let (_,first_results,next_i)=Option.unpack opt1 in
  snake_iterator_partial_recognizer (first_results,s,next_i);;

add_recognizer (label_for_snake,snake_recognizer);; 

let label_for_phor="phor";;
add_label label_for_phor;;

let phor_recognizer=
  Parametric_hrecognize.chain
  label_for_phor
  [
     c "for";
     whites;
     paren_block;
     whites;
     brace_block;
  ];;


add_recognizer (label_for_phor,phor_recognizer);; 


let label_for_phoreech="phoreech";;
add_label label_for_phoreech;;


let phoreech_recognizer=
  Parametric_hrecognize.chain
  label_for_phor
  [
     c "foreach";
     whites;
     paren_block;
     whites;
     brace_block;
  ];;

add_recognizer (label_for_phoreech,phoreech_recognizer);; 



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


let reference_for_loaf = ref "";;
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




*)