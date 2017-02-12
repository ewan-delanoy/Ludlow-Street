(*

#use"Kernighan/gparser_special_character.ml";;

*)



let separator_for_chain='.';;
let separator_for_disjunction='|';;
let marker_for_star='*';;
let marker_for_one_or_more='+';;
let marker_for_optional='?';;

let all=[
  separator_for_chain;
  separator_for_disjunction;
  marker_for_star;
  marker_for_one_or_more;
  marker_for_optional;
];;

let appears_in_string s=
   List.exists(
     fun c->String.contains s c
   );;
   