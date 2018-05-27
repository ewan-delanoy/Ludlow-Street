(*

Concrete values of type My_str.regexp.

#use"my_str_example.ml";;

*)

let capital_letter=Alternative_str.veil "[A-Z]";;
let letters=Alternative_str.veil "[A-Za-z1-9_']*";;
let nonletter=Alternative_str.veil "[^A-Za-z1-9_']";;
let white=Alternative_str.veil "[ \n\r\t]";;
let maybe_whites=Alternative_str.star white;;
let some_whites=Alternative_str.plus white;;
 

let delimited_module_name=Alternative_str.big_concat
  [
    nonletter;capital_letter;letters;nonletter
  ];;

let bare_module_name=Alternative_str.big_concat
  [
    capital_letter;letters
  ];;

let include_case=
  let left_part=Alternative_str.veil "[ \n\r\t]+include[ \n\r\t(]+"
  and center_part=bare_module_name 
  and right_part=nonletter in
  Alternative_str.create_centered_regexp left_part center_part right_part;; 

let open_case=
  let left_part=Alternative_str.veil "[ \n\r\t]+open[ \n\r\t(]+"
  and center_part=bare_module_name 
  and right_part=nonletter in
  Alternative_str.create_centered_regexp left_part center_part right_part;; 

let moodle_case=
  let left_part=Alternative_str.big_concat 
  [some_whites;Alternative_str.veil"module";some_whites;
   bare_module_name;Alternative_str.veil"=";maybe_whites]
  and center_part=bare_module_name 
  and right_part=nonletter in
  Alternative_str.create_centered_regexp left_part center_part right_part;; 

let pointed_case=
  let left_part=nonletter
  and center_part=bare_module_name 
  and right_part=Alternative_str.regexp_string "." in
  Alternative_str.create_centered_regexp left_part center_part right_part;; 

let moodle_cases=[include_case;open_case;moodle_case;pointed_case];;
let index_for_include_case=1;;
let index_for_open_case=2;;
let index_for_moodle_case=3;;
let index_for_pointed_case=4;;


(*
My_str.centered_regexp_match include_case " include Peggy;; " 1;;
My_str.centered_regexp_match include_case " include_once;; " 1;;
My_str.centered_regexp_match moodle_case " module Amy=Lawson " 1;;
My_str.centered_regexp_match pointed_case " 57+Everybody.talking-78 " 4;;
*)

 let capital_letter=Alternative_str.veil "[A-Z]";;
 
 let alphanumeric=
    Alternative_str.big_or
      [ 
     	Alternative_str.veil "[a-z]";
     	Alternative_str.veil "[A-Z]";
     	Alternative_str.veil "[0-9]";
     	Alternative_str.regexp_string "_";
      ];;
 
 let alphanumerics=Alternative_str.plus alphanumeric;;
 
 let beginning_of_module_definition=
    Alternative_str.set_backtrack 1
    (Alternative_str.big_concat
      [
         white;
         Alternative_str.regexp_string "module";
         some_whites;
         capital_letter;
         alphanumerics;
         some_whites;
         Alternative_str.regexp_string "=";
         some_whites;
         Alternative_str.regexp_string "struct";
         white;
          
      ]);;
      
 let beginning_of_module_reminder=
    Alternative_str.set_backtrack 1
    (Alternative_str.big_concat
      [
         white;
         Alternative_str.regexp_string "module";
         some_whites;
         capital_letter;
         alphanumerics;
         some_whites;
         Alternative_str.regexp_string ":";
         some_whites;
         Alternative_str.regexp_string "sig";
         white;
          
      ]);;
      
 let beginning_of_module_type_definition=
    Alternative_str.set_backtrack 1
    (Alternative_str.big_concat
      [
         white;
         Alternative_str.regexp_string "module";
         some_whites;
         Alternative_str.regexp_string "type";
         some_whites;
         capital_letter;
         alphanumerics;
         some_whites;
         Alternative_str.regexp_string "=";
         some_whites;
         Alternative_str.regexp_string "sig";
         white;
          
      ]);;           
      
      
 let the_end=
    Alternative_str.set_backtrack 1
    (Alternative_str.big_concat
      [
         white;
         Alternative_str.regexp_string "end";
         white;
          
      ]);;       
      