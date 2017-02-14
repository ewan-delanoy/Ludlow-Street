(*

#use"GParser/gparser_result.ml";;

*)


type t={
   description : Gparser_description.t;
   whole_range : int*int ;
   important_ranges : (int*int) list;
   final_cursor_position : int; 
   disjunction_index : int option;
};;

let description x=x.description;;
let whole_range x=x.whole_range;;
let important_ranges x=x.important_ranges;;
let final_cursor_position x=x.final_cursor_position;;
let disjunction_index x=x.disjunction_index;;

let veil a b c d e={
   description =a;
   whole_range =b;
   important_ranges =c;
   final_cursor_position =d; 
   disjunction_index=e;
};;


