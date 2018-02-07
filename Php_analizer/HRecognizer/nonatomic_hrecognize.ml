(*

#use"Php_analizer/HRecognizer/nonatomic_hrecognize.ml";;

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
    
let recgz_star old_f atm s i=
     let rec tempf=(fun idx->
       match old_f atm s idx with
       None->Some(idx)
       |Some(new_idx)->tempf idx
     )  in
     tempf i;;   

let rec recgz natm s i=
  match natm with
  Nonatomic_hrecognizer.Leaf(_,atm)->Atomic_hrecognize.recgnz atm s i
  |Nonatomic_hrecognizer.Chain(_,l)->
      recgz_chain recgz l s i 
  |Nonatomic_hrecognizer.Ordered_disjunction(_,l)->
      recgz_ordered_disjunction recgz l s i         
  |Nonatomic_hrecognizer.Star(_,natm2)->
      recgz_star recgz natm2 s i;;

exception Debug_chain_exn;;

let debug_chain old_f l s i=
        let rec tempf=(fun (idx,da_ober)->
         match da_ober with
         []->raise(Debug_chain_exn)
         |atm::peurrest->
           (
             match recgz atm s idx with
              None->old_f atm s idx 
             |Some(new_idx)->tempf(new_idx,peurrest)
           )
        ) in
        tempf (i,l);;
         
exception Debug_star_exn;;
     
let rec debug natm s i=
       match natm with
       Nonatomic_hrecognizer.Leaf(_,_)
       |Nonatomic_hrecognizer.Ordered_disjunction(_,_)->(natm,i)  
       |Nonatomic_hrecognizer.Chain(_,l)->debug_chain debug l s i      
       |Nonatomic_hrecognizer.Star(_,natm2)->
           raise(Debug_star_exn);;
     
     
     
     
           
     
      

      

 