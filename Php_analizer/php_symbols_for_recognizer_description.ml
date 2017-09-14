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



