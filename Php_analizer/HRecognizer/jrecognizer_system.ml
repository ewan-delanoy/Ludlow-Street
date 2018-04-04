(*

#use"Php_analizer/HRecognizer/jrecognizer_system.ml";;

*)

type t={
   atoms : (string*Atomic_hrecognizer.t) list;
   nonatoms : (string*Nonatomic_jrecognizer.t*Standard_jdisjunction.t) list;
   coatom_dependencies : (string*(Ordered_string.set)) list;
};;

module Private =struct

let expanded_version l=


let pusher_for_update (_,(already_updated,treated,to_be_treated))=match to_be_treated with
 []->(true,(already_updated,treated,to_be_treated))
|old_item::other_ones->
   let (name,old_val,_)=old_item in
   let (novelty_present,new_val)=Nonatomic_jrecognizer.update already_updated old_val in
   if novelty_present
   then let new_item=(name,new_val,)
        (false,(already_updated,treated,to_be_treated))
   else (false,(already_updated,old_item::treated,to_be_treated));;
  
  

end;;



