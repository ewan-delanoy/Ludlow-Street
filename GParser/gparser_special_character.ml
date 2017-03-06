(*

#use"GParser/gparser_special_character.ml";;

*)



let separator_for_chain='.';;
let separator_for_disjunction='|';;
let marker_for_star='*';;
let marker_for_one_or_more='+';;
let marker_for_optional='?';;
let marker_for_recoiling_ending='<';;

let all=[
  separator_for_chain;
  separator_for_disjunction;
  marker_for_star;
  marker_for_one_or_more;
  marker_for_optional;
  marker_for_recoiling_ending;
];;


let wrap s=
   if   List.exists(
     fun c->String.contains s c
   ) all
   then "("^s^")"
   else s;; 