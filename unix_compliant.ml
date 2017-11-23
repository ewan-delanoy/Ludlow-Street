(*

#use"unix_compliant.ml";;

*)

let list_for_unix_usual=
   Image.image (fun c->let s=String.make 1 c in (s,s) ) 
  Charset.unix_filename_admissible_characters;;

let list_for_unix_rewriting=
   [
                 " ","_";
                 "-","_";
                 "'","_single_quote_";
                "\"","_double_quote_";
                 "&","_and_";
                 "(","_left_parenthesis_";
                 ")","_right_parenthesis_";
                 "?","_question_mark_";
                 "|","_vertical_bar_";
                 "<","_lower_than_";
                 ">","_greater_than_";
                 "=","_equals_";
                 ",","_comma_";
                 ";","_semicolon_";
          "\xc2\xa0","_";
          "\xcc\x80","_grave_";
          "\xcc\x81","_acute_";
          "\xcc\x83","_tilde_";
          "\xcc\xa7","_cedilla_";
      "\xe2\x80\x93","_";
      "\xe2\x80\x94","_";
      "\xe2\x80\x98","_lquote_";
      "\xe2\x80\x99","_rquote_";
      "\xe2\x80\xa6","_etc_";
    ];;
  
let unix_rewrite_char t=List.assoc t
  ((list_for_unix_usual)@(list_for_unix_rewriting));;
  
exception Unix_unknown of string;;  
  
let make_unix_compliant s=
   try String.concat "" 
   (Image.image unix_rewrite_char (Utf_eight.decompose s)) with
   _->raise(Unix_unknown(s));;  
  

     
  

  
  