(*

#use"Php_analizer/php_symbols_for_recognizer_description.ml";;

Generic parser for php code.


*)



let pair_for_disjunction=("_l_","_rd_");;
let associator_for_disjunction="_u_";;  


let all_pairs=pair_for_disjunction::Generalizer.all_pairs;;
let new_symbols=
    Ordered.forget_order(Tidel.diforchan(associator_for_disjunction::
    
   (Image.image fst all_pairs)@(Image.image snd all_pairs)));;

let all_symbols_so_far=
    Ordered_string.diforchan(
    (Image.image fst Php_projected_token_set.readables_and_toksets)
    @new_symbols );;
    
let  dependencies l=
   let temp0=Ordered_string.insert ""  all_symbols_so_far in 
   let temp1=Str.split (Str.regexp_string " ") l in   
   let temp2=Ordered_string.diforchan temp1 in
   let temp3=Ordered_string.lemel temp2 temp0 in
   temp3;;


