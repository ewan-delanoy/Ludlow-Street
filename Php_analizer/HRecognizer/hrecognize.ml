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

let one_liner_with_variable_recognizer s i=
  Parametric_hrecognize.chain 
  label_for_one_liner_with_variable
  [
     c "$";
     first_letter;
     st Charset.strictly_alphanumeric_characters;
     whites;
     paren_block;
     whites;
     c ";"
  ];;

  if not(Substring.is_a_substring_located_at "$" s i)
  then None
  else 
  let opt2=After.after_php_label  s (i+1) in
  if opt2=None then None else
  let i2=Option.unpack opt2 in
  let opt3=After.next_in_list ['\n';'\r';';'] s i2 in
  if opt3=None then None else
  let i3=Option.unpack opt3 in 
  if Strung.get s i3=';'
  then Some(label_for_one_liner_with_variable,[i;i2;i3],i3+1)
  else None;;

add_recognizer (label_for_one_liner_with_variable,one_liner_with_variable_recognizer);; 

let label_for_inclusion_with_parenthesis="inclusion_with_parenthesis";;
add_label label_for_inclusion_with_parenthesis;;

let inclusion_with_parenthesis_recognizer s i=
  let opt1=Option.seek(
     fun kwd->Substring.is_a_substring_located_at kwd s i
  ) ["include_once";"require_once";"include";"require"] in
  if opt1=None then None else
  let kwd1=Option.unpack opt1 in
  let i2=i+(String.length kwd1) in
  let opt3=After.after_whites s i2 in
  if opt3=None then None else
  let i3=Option.unpack opt3 in
  if not(Substring.is_a_substring_located_at "(" s i3)
  then None
  else 
  let i4=After.after_closing_character ('(',')') s (i3+1,1) in
  let opt5=After.after_whites s i4 in
  if opt5=None then None else
  let i5=Option.unpack opt5 in
  if not(Substring.is_a_substring_located_at ";" s i5)
  then None
  else Some(label_for_inclusion_with_parenthesis,[i;i2;i3;i4;i5],i5+1);;

add_recognizer (label_for_inclusion_with_parenthesis,inclusion_with_parenthesis_recognizer);; 

let label_for_double_slash_comment="double_slash_comment";;
add_label label_for_double_slash_comment;;

let double_slash_comment_recognizer s i=
  if not(Substring.is_a_substring_located_at "//" s i)
  then None
  else 
  let j=Substring.leftmost_index_of_in_from "\n" s (i+2) in
  if j<1 
  then None
  else Some(label_for_double_slash_comment,[i;j],j+1);;

add_recognizer (label_for_double_slash_comment,double_slash_comment_recognizer);; 

(* the fst_kwd parameter is either if or elseif *)

let ivy_start_partial_recognizer fst_kwd s i=
  if not(Substring.is_a_substring_located_at fst_kwd s i)
  then None
  else 
  let opt2=After.after_whites s (i+String.length fst_kwd) in
  if opt2=None then None else
  let i2=Option.unpack opt2 in
  if not(Substring.is_a_substring_located_at "(" s i2)
  then None
  else 
  let i3=After.after_closing_character ('(',')') s (i2+1,1) in
  let opt4=After.after_whites s i3 in
  if opt4=None then None else
  let i4=Option.unpack opt4 in
  if not(Substring.is_a_substring_located_at "{" s i4)
  then None
  else 
  let i5=After.after_closing_character ('{','}') s (i4+1,1) in
  let opt6=After.after_whites s i5 in
  if opt6=None then None else   
  let i6=Option.unpack opt6 in
  Some(i,i2,i3,i4,i5,i6);;

(*

ivy_start_partial_recognizer "if" "if (abc) {def} ghi" 1;;
ivy_start_partial_recognizer "elseif" "elseif (abc) {def} ghi" 1;;

*)


let tag_for_elsie_partial_recognizer_jmp=1;;

let elsie_partial_recognizer s i=
  if not(Substring.is_a_substring_located_at "else" s i)
  then (None,0)
  else 
  let opt2=After.after_whites s (i+4) in
  if opt2=None then (None,0) else
  let i2=Option.unpack opt2 in
  if not(Substring.is_a_substring_located_at "{" s i2)
  then (None,tag_for_elsie_partial_recognizer_jmp)
  else 
  let i3=After.after_closing_character ('{','}') s (i2+1,1) in
  (Some(i,i2,i3),0);;

let label_for_ive="ive";;
add_label label_for_ive;;

let label_for_ivy="ivy";;
add_label label_for_ivy;;

let label_for_ive_or_ivy="ive_or_ivy";;
add_label label_for_ive_or_ivy;;


let rec ivy_iterator_partial_recognizer (graet,s,i)=
   let (opt1,tag)=elsie_partial_recognizer s i in
   if tag=tag_for_elsie_partial_recognizer_jmp
   then None
   else
   if opt1<>None
   then let (j,j2,j3)=Option.unpack opt1 in
        Some(label_for_ive,graet@[j2;j3],j3)
   else 
   let opt2=ivy_start_partial_recognizer "elseif" s i in
   if opt2=None
   then Some(label_for_ivy,graet,i)
   else 
   let (_,j2,j3,j4,j5,j6)=Option.unpack opt2 in
   ivy_iterator_partial_recognizer (graet@[j2;j3;j4;j5;j6],s,j6);;

let ive_or_ivy_recognizer s i=
    let opt1=ivy_start_partial_recognizer "if" s i in
    if opt1=None then None else
    let (_,i2,i3,i4,i5,i6)=Option.unpack opt1 in
    ivy_iterator_partial_recognizer ([i;i2;i3;i4;i5;i6],s,i6);;

add_recognizer (label_for_ive_or_ivy,ive_or_ivy_recognizer);; 

(*

ive_or_ivy_recognizer "if (ab) {c} elseif (de) {fg} elseif(h){ij} else{kl} m" 1;;
ive_or_ivy_recognizer "if (ab) {c} elseif (de) {fg} elseif(h){ij}m" 1;;

ive_or_ivy_recognizer "if (!$topic_id && !$post_id)\n{\n\ttrigger_error('NO_TOPIC');\n}m" 1;;

*)

let label_for_assign_to_array="assign_to_array";;
add_label label_for_assign_to_array;;

let assign_to_array_recognizer s i=
  if not(Substring.is_a_substring_located_at "$" s i)
  then None
  else 
  let opt2=After.after_php_label  s (i+1) in
  if opt2=None then None else
  let i2=Option.unpack opt2 in
  let opt3=After.after_whites s i2 in
  if opt3=None then None else
  let i3=Option.unpack opt3 in 
  if not(Substring.is_a_substring_located_at "=" s i3)
  then None
  else 
  let opt4=After.after_whites s (i3+1) in
  if opt4=None then None else
  let i4=Option.unpack opt4 in
  if not(Substring.is_a_substring_located_at "array(" s i4)
  then None
  else 
  let i5=After.after_closing_character ('(',')') s (i4+6,1) in
  let opt6=After.after_whites s i5 in
  if opt6=None then None else   
  let i6=Option.unpack opt6 in
  if not(Substring.is_a_substring_located_at ";" s i6)
  then None
  else 
   Some(label_for_assign_to_array,[i;i2;i3;i4;i5;i6],i6+1);;

add_recognizer (label_for_assign_to_array,assign_to_array_recognizer);; 

let label_for_fnctn_call="fnctn_call";;
add_label label_for_fnctn_call;;

let fnctn_call_recognizer s i= 
  let opt2=After.after_php_label  s (i+1) in
  if opt2=None then None else
  let i2=Option.unpack opt2 in
  let opt3=After.after_whites s i2 in
  if opt3=None then None else
  let i3=Option.unpack opt3 in 
  if not(Substring.is_a_substring_located_at "(" s i3)
  then None
  else 
  let i4=After.after_closing_character ('(',')') s (i3+1,1) in
  let opt5=After.after_whites s i4 in
  if opt5=None then None else   
  let i5=Option.unpack opt5 in
  if not(Substring.is_a_substring_located_at ";" s i5)
  then None
  else 
  Some(label_for_fnctn_call,[i;i2;i3;i4;i5],i5+1);;

add_recognizer (label_for_fnctn_call,fnctn_call_recognizer);; 

let label_for_assign_to_fnctn_call="assign_to_fnctn_call";;
add_label label_for_assign_to_fnctn_call;;

let assign_to_fnctn_call_recognizer s i=
  if not(Substring.is_a_substring_located_at "$" s i)
  then None
  else 
  let opt2=After.after_php_label  s (i+1) in
  if opt2=None then None else
  let i2=Option.unpack opt2 in
  let opt3=After.after_whites s i2 in
  if opt3=None then None else
  let i3=Option.unpack opt3 in 
  if not(Substring.is_a_substring_located_at "=" s i3)
  then None
  else 
  let opt4=After.after_whites s (i3+1) in
  if opt4=None then None else
  let i4=Option.unpack opt4 in
  let opt5=After.after_php_label  s i4 in
  if opt5=None then None else
  let i5=Option.unpack opt5 in
  let opt6=After.after_whites s i5 in
  if opt6=None then None else
  let i6=Option.unpack opt6 in 
  if not(Substring.is_a_substring_located_at "(" s i6)
  then None
  else 
  let i7=After.after_closing_character ('(',')') s (i6+1,1) in
  let opt8=After.after_whites s i7 in
  if opt8=None then None else   
  let i8=Option.unpack opt8 in
  if not(Substring.is_a_substring_located_at ";" s i8)
  then None
  else Some(label_for_assign_to_fnctn_call,[i;i2;i3;i4;i5;i6;i7;i8],i8+1);;

add_recognizer (label_for_assign_to_fnctn_call,assign_to_fnctn_call_recognizer);; 

let label_for_wiley="wiley";;
add_label label_for_wiley;;

let wiley_recognizer s i=
  if not(Substring.is_a_substring_located_at "while" s i)
  then None
  else 
  let opt2=After.after_whites s (i+5) in
  if opt2=None then None else
  let i2=Option.unpack opt2 in
  if not(Substring.is_a_substring_located_at "(" s i2)
  then None
  else 
  let i3=After.after_closing_character ('(',')') s (i2+1,1) in
  let opt4=After.after_whites s i3 in
  if opt4=None then None else
  let i4=Option.unpack opt4 in
  if not(Substring.is_a_substring_located_at "{" s i4)
  then None
  else 
  let i5=After.after_closing_character ('{','}') s (i4+1,1) in
  let opt6=After.after_whites s i5 in
  if opt6=None then None else   
  let i6=Option.unpack opt6 in
  Some(label_for_wiley,[i;i2;i3;i4;i5;i6],i6);;

add_recognizer (label_for_wiley,wiley_recognizer);; 


let label_for_snake="snake";;
add_label label_for_snake;;

let snake_pusher_partial_recognizer s i=
  if not(Substring.is_a_substring_located_at "->" s i)
  then None
  else 
  let opt2=After.after_whites s (i+2) in
  if opt2=None
  then None
  else
  let i2=Option.unpack opt2 in
  let opt3=After.after_php_label  s i2 in
  if opt3=None then None else
  let i3=Option.unpack opt3 in
  let opt4=After.after_whites s i3 in
  if opt4=None
  then None
  else 
  let i4=Option.unpack opt4 in
  if not(Substring.is_a_substring_located_at "(" s i4)
  then None
  else 
  let i5=After.after_closing_character ('(',')') s (i4+1,1) in
  let opt6=After.after_whites s i5 in
  if opt6=None then None else   
  let i6=Option.unpack opt6 in
  Some([i;i+2;i3;i4;i5],i6);;


let rec snake_iterator_partial_recognizer (graet,s,i)=
  let opt1=snake_pusher_partial_recognizer s i in
  if opt1<>None
  then let (interm_results,next_i)=Option.unpack opt1 in
      snake_iterator_partial_recognizer(graet@interm_results,s,next_i)
  else 
  if not(Substring.is_a_substring_located_at ";" s i)
  then None
  else Some(label_for_snake,graet@[i],i+1);;


let snake_recognizer s i=
  if not(Substring.is_a_substring_located_at "$" s i)
  then None
  else 
  let opt2=After.after_php_label  s (i+1) in
  if opt2=None then None else
  let i2=Option.unpack opt2 in
  let opt3=After.after_whites s i2 in
  if opt3=None then None else   
  let i3=Option.unpack opt3 in
  snake_iterator_partial_recognizer ([i;i2],s,i3);;

add_recognizer (label_for_snake,snake_recognizer);; 

let label_for_phor="phor";;
add_label label_for_phor;;

let phor_recognizer s i=
  if not(Substring.is_a_substring_located_at "for" s i)
  then None
  else 
  let opt2=After.after_whites s (i+3) in
  if opt2=None then None else
  let i2=Option.unpack opt2 in
  if not(Substring.is_a_substring_located_at "(" s i2)
  then None
  else 
  let i3=After.after_closing_character ('(',')') s (i2+1,1) in
  let opt4=After.after_whites s i3 in
  if opt4=None then None else
  let i4=Option.unpack opt4 in
  if not(Substring.is_a_substring_located_at "{" s i4)
  then None
  else 
  let i5=After.after_closing_character ('{','}') s (i4+1,1) in
  let opt6=After.after_whites s i5 in
  if opt6=None then None else   
  let i6=Option.unpack opt6 in
  Some(label_for_phor,[i;i2;i3;i4;i5;i6],i6);;

add_recognizer (label_for_phor,phor_recognizer);; 


let label_for_phoreech="phoreech";;
add_label label_for_phoreech;;


let phoreech_recognizer s i=
  if not(Substring.is_a_substring_located_at "foreach" s i)
  then None
  else 
  let opt2=After.after_whites s (i+7) in
  if opt2=None then None else
  let i2=Option.unpack opt2 in
  if not(Substring.is_a_substring_located_at "(" s i2)
  then None
  else 
  let i3=After.after_closing_character ('(',')') s (i2+1,1) in
  let opt4=After.after_whites s i3 in
  if opt4=None then None else
  let i4=Option.unpack opt4 in
  if not(Substring.is_a_substring_located_at "{" s i4)
  then None
  else 
  let i5=After.after_closing_character ('{','}') s (i4+1,1) in
  let opt6=After.after_whites s i5 in
  if opt6=None then None else   
  let i6=Option.unpack opt6 in
  Some(label_for_phoreech,[i;i2;i3;i4;i5;i6],i6);;

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