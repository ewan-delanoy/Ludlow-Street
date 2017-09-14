(*

#use"Php_analizer/php_check_recognizer_rules.ml";;

If everything is OK, all the components of they_should_be_empty are []

*)

type t=
     Selector of Php_short_selector.t
    |New_symbol of string;;

let part1=Image.image (fun (s,sel)->(s,Selector sel)) Php_short_selector.readables_and_selectors;;
let part2=Image.image (fun t->(t,New_symbol t)) 
  Php_symbols_for_recognizer_description.new_symbols;;
let whole=part1@part2;;


let should_be_empty1=
   List.filter(
     fun (x,y)->fst(x)=fst(y)
   ) (Uple.list_of_pairs whole);;

assert(should_be_empty1=[]);;

