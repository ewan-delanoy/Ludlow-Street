(*

#use"Php_analizer/HRecognizer/concretize_hrecognizer.ml";;


*)

exception Unused_name of string;;

let get_recognizer_with_name previous_recgzrs name=
  match Option.seek (
    fun rcgzr->Nonatomic_hrecognizer.name rcgzr=name
  ) previous_recgzrs with
  None->raise(Unused_name(name))
  |Some(rcgzr)->rcgzr;;



 let concretize (avoidables,previous_recgzrs) abstract_summary=
    match abstract_summary with 
    Abstractified_nonatomic_hrecognizer.Leaf(name_for_whole,atm)->
    Nonatomic_hrecognizer.leaf name_for_whole atm
 |Chain(name_for_whole,l)->
      let temp1=Image.image (get_recognizer_with_name previous_recgzrs) l in
      Nonatomic_hrecognizer.chain name_for_whole temp1
 |Ordered_disjunction(name_for_whole,l)->
      let temp1=Image.image (get_recognizer_with_name previous_recgzrs) l in
      Nonatomic_hrecognizer.chain name_for_whole temp1     
 |Star(name_for_whole,x)->
      Nonatomic_hrecognizer.star name_for_whole  (get_recognizer_with_name previous_recgzrs x)
 |Maybe(name_for_whole,x)->
      Nonatomic_hrecognizer.maybe name_for_whole  (get_recognizer_with_name previous_recgzrs x)
 |Avoider(name_for_whole,x,avdbl)->
      let avoided_ones=List_of_avoidables.avoided_words avoidables avdbl in
      Nonatomic_hrecognizer.avoider name_for_whole 
        (get_recognizer_with_name previous_recgzrs x,avoided_ones)
 |Motionless(name_for_whole,x)->
       Nonatomic_hrecognizer.maybe name_for_whole  
         (get_recognizer_with_name previous_recgzrs x)
 |Disjunction_of_chains(name_for_whole,ll)->
   let temp1=Image.image (Image.image (get_recognizer_with_name previous_recgzrs)) ll in
   Nonatomic_hrecognizer.disjunction_of_chains name_for_whole  
         temp1;;

  