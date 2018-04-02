(*

#use"Php_analizer/HRecognizer/nonatomic_jrecognize.ml";;

*)

let recgz_chain old_f l s i=
   let rec tempf=(fun (idx,da_ober)->
    match da_ober with
    []->Some(idx)
    |atm::peurrest->
      (
        match old_f atm s idx with
         None->None
        |Some(new_idx)->tempf(new_idx,peurrest)
      )
   ) in
   tempf (i,l);;


let recgz_ordered_disjunction old_f l s i=
    Option.find_and_stop (
      fun atm->old_f atm s i
    ) l;;
          

let recgz_avoiding_keywords old_f (x,l) s i=
          match old_f x s i with
          None->None
          |Some(new_idx)->
            if new_idx<=i then Some(new_idx) else
            let t=Cull_string.interval s i (new_idx-1) in
            if List.mem t l 
            then None 
            else Some(new_idx);;         

let rec recgz natm s i=
  match natm with
   Nonatomic_jrecognizer.Leaf(_,atm)->Atomic_hrecognize.recgnz atm s i
  |Nonatomic_jrecognizer.Chain(_,l)->
      recgz_chain recgz l s i 
  |Nonatomic_jrecognizer.Ordered_disjunction(_,l)->
      recgz_ordered_disjunction recgz l s i         
  |Nonatomic_jrecognizer.Keyword_avoider(_,(x,l))->
      recgz_avoiding_keywords recgz (x,l) s i        ;;




let recgz_and_add_label lbl natm s i=
   match recgz natm s i with
   None->None
   |Some(next_i)->Some(lbl,(i,next_i-1),next_i);;

exception Debug_chain_exn;;


let debug_chain  l s i=
         let rec tempf=(fun (idx,graet,da_ober)->
         match da_ober with
         []->(List.rev graet,None,(String.length s)+1)
         |atm::peurrest->
           (
             match recgz atm s idx with
              None->(List.rev graet,Some(atm),idx)
             |Some(new_idx)->
               let t=Cull_string.interval s idx (new_idx-1) in
               tempf(new_idx,(atm,t,(idx,new_idx-1))::graet,peurrest)
           )
        ) in
        tempf (i,[],l);;     

exception Debug_disjunction_exn;;        

let debug_disjunction  l s i=
  match Option.find_and_stop (
    fun atm->match recgz atm s i with
    None->None
    |Some(res)->Some(atm,res)
  ) l with
  None->raise(Debug_disjunction_exn)
  |Some(atm0,res0)->([atm0,"",(0,0)],None,-1);;     
            
     
        
exception Debug_not_implemented_yet;;
        
let  debug natm s i=
          match natm with
           Nonatomic_jrecognizer.Chain(_,l)->debug_chain  l s i 
          |Nonatomic_jrecognizer.Ordered_disjunction (_,l)->debug_disjunction  l s i       
          |_->raise(Debug_not_implemented_yet);;     
           
     
      

      

 