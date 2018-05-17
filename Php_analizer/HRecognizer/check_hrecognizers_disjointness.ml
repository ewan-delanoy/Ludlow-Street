(*

#use"Php_analizer/HRecognizer/check_hrecognizers_disjointness.ml";;

*)

type rcgzr = Nonatomic_hrecognizer.t;;

type obstruction_data = rcgzr list * rcgzr * rcgzr list * rcgzr * rcgzr list;;

exception Cannot_repair of obstruction_data;;

type  low_level_analizer_result=
      Disjointness_confirmed
     |Obstruction_found of obstruction_data
     |Expansion_found of rcgzr list * rcgzr list list * rcgzr list list;;

module Private=struct

let constant_aspect_for_atomic_hrecognizer =function
   Atomic_hrecognizer.Constant(s)->Some(s)
  |_->None;;

let rec constant_aspect=function
 Nonatomic_hrecognizer.Leaf(_,atm)->constant_aspect_for_atomic_hrecognizer atm 
|Nonatomic_hrecognizer.Chain(_,l)->
      if List.length(l)<>1 then None else constant_aspect(List.hd l)    
|Nonatomic_hrecognizer.Ordered_disjunction(_,l)->
      if List.length(l)<>1 then None else constant_aspect(List.hd l)
|Nonatomic_hrecognizer.Disjunction_of_chains (_,ll)->
     if List.length(ll)<>1 then None else let l=List.hd ll in   
     if List.length(l)<>1 then None else constant_aspect(List.hd l)        
|_->None;;

let rec extract_left_constants (graet,da_ober)=
    match da_ober with
     []->(graet,[])
    |a::peurrest->
       let opt=constant_aspect a in
       if opt=None then (graet,da_ober) else 
       extract_left_constants (graet^(Option.unpack opt),peurrest);;

let common_prefix_for_atomic_hrecognizer =function
 Atomic_hrecognizer.Constant(s)->s
|Atomic_hrecognizer.Later_constant(s)->""
|Atomic_hrecognizer.Star(l_chr)->""
|Atomic_hrecognizer.Star_outside(l_chr)->""
|Atomic_hrecognizer.Enclosed(opener,closer)->String.make 1 opener
|Atomic_hrecognizer.Simple_quoted->"'"
|Atomic_hrecognizer.Double_quoted->"\"";;

let common_prefix_for_chain old_f l=
  let (graet1,da_ober1)=extract_left_constants ("",l) in
  (match da_ober1 with
   []->graet1
   |a::peurrest->(graet1)^(old_f a)
  );;

let rec common_prefix=function
        Nonatomic_hrecognizer.Leaf(_,atm)->common_prefix_for_atomic_hrecognizer atm
        |Nonatomic_hrecognizer.Chain(_,l)->common_prefix_for_chain common_prefix l
        |Nonatomic_hrecognizer.Ordered_disjunction(_,l)->
                  Strung.largest_common_prefix(Image.image common_prefix l)
        |Nonatomic_hrecognizer.Star(_,_)->""
        |Nonatomic_hrecognizer.Maybe(_,_)->""          
        |Nonatomic_hrecognizer.Avoider(_,(x,_))->common_prefix x
        |Nonatomic_hrecognizer.Motionless(_,x)->common_prefix x
        |Nonatomic_hrecognizer.Disjunction_of_chains(_,ll)->
                  Strung.largest_common_prefix(Image.image 
                  (common_prefix_for_chain common_prefix) ll)
        ;;


let first_char_for_atomic_hrecognizer x=match x with
        Atomic_hrecognizer.Constant(s)->Some(Tidel.singleton(String.get s 0))
       |Atomic_hrecognizer.Later_constant(s)->None
       |Atomic_hrecognizer.Star(l_chr)->None
       |Atomic_hrecognizer.Star_outside(l_chr)->None
       |Atomic_hrecognizer.Enclosed(opener,closer)->Some(Tidel.singleton opener)
       |Atomic_hrecognizer.Simple_quoted->Some(Tidel.singleton('\''))
       |Atomic_hrecognizer.Double_quoted->Some(Tidel.singleton('"'))
       ;;

exception First_char_for_nonatomic_exn of Nonatomic_hrecognizer.t;;       

let atomic_star_aspect x=match x with
Nonatomic_hrecognizer.Leaf(_,atm)->(
          match atm with
          Atomic_hrecognizer.Star(l_chr)->Some(l_chr)
          |_->None
       )
|_->None;;

let first_char_in_chain_case old_f l=
   let x=List.hd l in
   let opt=atomic_star_aspect x in
   if (opt=None)||((List.length l)<2)
   then old_f x
   else 
   let opt2=old_f (List.nth l 1) in
   if opt2=None
   then old_f x
   else
   Some(Tidel.teuzin (Tidel.safe_set(Option.unpack opt)) (Option.unpack opt2) );;

let rec first_char_for_nonatomic_hrecognizer x=match x with
        Nonatomic_hrecognizer.Leaf(_,atm)->first_char_for_atomic_hrecognizer atm
        |Nonatomic_hrecognizer.Chain(_,l)->
           first_char_in_chain_case first_char_for_nonatomic_hrecognizer l
        |Nonatomic_hrecognizer.Ordered_disjunction(_,l)->
                  let temp1=Image.image first_char_for_nonatomic_hrecognizer l in
                  if List.mem None temp1 then None else
                  Some(Tidel.big_teuzin(Image.image Option.unpack temp1))
        |Nonatomic_hrecognizer.Star(_,_)->None
        |Nonatomic_hrecognizer.Maybe(_,_)->None           
        |Nonatomic_hrecognizer.Avoider(_,(x,_))->
                   first_char_for_nonatomic_hrecognizer x
        |Nonatomic_hrecognizer.Motionless(_,l)->
                   first_char_for_nonatomic_hrecognizer x
        |Nonatomic_hrecognizer.Disjunction_of_chains(_,ll)->
                   let temp1=Image.image 
                    (first_char_in_chain_case first_char_for_nonatomic_hrecognizer) ll in
                   if List.mem None temp1 then None else
                   Some(Tidel.big_teuzin(Image.image Option.unpack temp1))           ;;
 
let first_char_for_chain l=
  first_char_in_chain_case first_char_for_nonatomic_hrecognizer l;;

let avoider_aspect=function
  Nonatomic_hrecognizer.Avoider(_,data)->Some(data)
  |_->None;;

let check_nonsymmetric_avoider_case x y=
   let opt1=constant_aspect x
   and opt2=avoider_aspect y in
   if (opt1=None)||(opt2=None)
   then false
   else 
   let word=Option.unpack opt1
   and (_,l)=Option.unpack opt2 in
   List.mem word l;;

let check_avoider_case x y=
    (check_nonsymmetric_avoider_case x y)
    ||
    (check_nonsymmetric_avoider_case y x);;

let test_for_string_strict_disjointness s1 s2=
    (*
      this test is applied to left shadows, so it must
      be more strict to mean something about the initial recognizers.
    *)
    let n1=String.length s1 
    and n2=String.length s2 in
    let m=(min n1 n2)-1 in
    List.exists(fun k->
       (String.get s1 k)<>(String.get s2 k)
    )(Ennig.ennig 0 m);;

let test_for_disjointness_via_first_chars lx ly=
  let opt1=first_char_for_chain lx
  and opt2=first_char_for_chain ly in
  if (opt1=None)||(opt2=None)
  then false
  else Tidel.kengeij_goullo (Option.unpack opt1) (Option.unpack opt2);; 

let main_test_for_disjointness lx ly=
   let x=List.hd lx and y=List.hd ly in
   if test_for_string_strict_disjointness (common_prefix x) (common_prefix y)
   then true
   else
   if test_for_disjointness_via_first_chars lx ly
   then true
   else  
   check_avoider_case x y
   ;;

let expand_at_cut_point (a,b)=
  match a with
  |Nonatomic_hrecognizer.Chain(_,l)->Some [l@b]
  |Nonatomic_hrecognizer.Ordered_disjunction(_,l)->
     Some(
       Image.image (fun elt->elt::b) l
     )
  |Nonatomic_hrecognizer.Maybe(_,sa)->
     Some(
       [b;sa::b]
     )  
  |_->None;;

let expand_pair (a1,b1,a2,b2)=
    (expand_at_cut_point (a1,b1),expand_at_cut_point (a2,b2));;



let rec compute_common_left_factor x=
  let (graet,l1,l2)=x in
  if (l1=[])||(l2=[]) then x else
  let (a1,b1)=Listennou.ht l1
  and (a2,b2)=Listennou.ht l2 in
  if Nonatomic_hrecognizer.name a1=Nonatomic_hrecognizer.name a2
  then compute_common_left_factor(a1::graet,b1,b2)
  else x;;

let low_level_analizer (old_graet,old_l1,old_l2)=
   let (graet,l1,l2)=compute_common_left_factor (old_graet,old_l1,old_l2) in
   if (l1=[])||(l2=[])
   then Disjointness_confirmed
   else
   let (a1,b1)=Listennou.ht l1
   and (a2,b2)=Listennou.ht l2 in
   if main_test_for_disjointness l1 l2
   then Disjointness_confirmed
   else 
   let (opt1,opt2)=expand_pair (a1,b1,a2,b2) in
   if (opt1,opt2)=(None,None) then Obstruction_found(graet,a1,b1,a2,b2) else
   let new_l1=(match opt1 with None->[a1::b1] |Some(l)->l )
   and new_l2=(match opt2 with None->[a2::b2] |Some(l)->l ) in
   Expansion_found(graet,new_l1,new_l2);;

module Find_Fault=struct    

let pusher (x,y,graet,da_ober)=
  match da_ober with
  []->(x,y,graet,[])
  |wlkr::peurrest->
     (
       match low_level_analizer wlkr with
       Disjointness_confirmed->(x,y,graet,peurrest)
      |Obstruction_found(obstr_data)->(x,y,obstr_data::graet,peurrest)
      |Expansion_found(graet2,new_l1,new_l2)->
        let temp1=Cartesian.product new_l1 new_l2 in
        let temp2=Image.image (fun (nl1,nl2)->(graet2,nl1,nl2)) temp1 in
        (x,y,graet,temp2@peurrest)
     ) ;;  

let rec iterator walker=
    let (x,y,graet,da_ober)=walker in
    if da_ober=[] then List.rev_map(fun (graet,a1,b1,a2,b2)->(x,y,graet,a1,b1,a2,b2)) (graet) else
    iterator(pusher walker);;

let in_pair x y=iterator (x,y,[],[[],[x],[y]]);;

end;;    

module Repair=struct

let pusher ll1=
   let indexed_ll1=Ennig.index_everything ll1 in
   let temp1=Uple.list_of_pairs indexed_ll1 in
   match Option.find_and_stop (
      fun ((i1,l1),(i2,l2))->match low_level_analizer([],l1,l2) with
      Disjointness_confirmed->None
     |Obstruction_found(obstr_data)->raise(Cannot_repair(obstr_data))
     |Expansion_found(graet,new_sl1,new_sl2)->
        let full_new_sl1=Image.image (fun l->List.rev_append graet l) new_sl1
        and full_new_sl2=Image.image (fun l->List.rev_append graet l) new_sl2 in
        Some(List.flatten(Image.image (fun (i,l)->
           if i=i1 then full_new_sl1 else 
           if i=i2 then full_new_sl2 else [l]
        ) indexed_ll1))
   ) temp1 with
   None->(true,ll1)
   |Some(new_ll1)->(false,new_ll1);;  
   
let rec iterator  (end_reached,x)=
   if end_reached then x else
   iterator(pusher x);;

end;;  

module Repair_Labelled=struct

let pusher ll1=
    let indexed_ll1=Ennig.index_everything ll1 in
    let temp1=Uple.list_of_pairs indexed_ll1 in
    match Option.find_and_stop (
       fun ((i1,(x1,l1)),(i2,(x2,l2)))->match low_level_analizer([],l1,l2) with
       Disjointness_confirmed->None
      |Obstruction_found(obstr_data)->raise(Cannot_repair(obstr_data))
      |Expansion_found(graet,new_sl1,new_sl2)->
         let full_new_sl1=Image.image (fun l->List.rev_append graet l) new_sl1
         and full_new_sl2=Image.image (fun l->List.rev_append graet l) new_sl2 in
         let temp2=List.flatten(Image.image (fun (i,(x,l))->
            if i=i1 then Image.image (fun l2->(x,l2)) full_new_sl1 else 
            if i=i2 then Image.image (fun l2->(x,l2)) full_new_sl2 else [x,l]
          ) indexed_ll1) in
         Some(temp2)
    ) temp1 with
    None->(true,ll1)
    |Some(new_ll1)->(false,new_ll1);; 
  
(*
takes as argument a list where each name corresponds to a unique chain,
and returns  a list where one name may correspond to several chains.

*)

let rec iterator  (end_reached,x)=
    if end_reached then x else
    iterator (pusher x);;   

end;;  

end;;


let quick_check_on_disjunction_of_chains ll1=
  let temp1=Uple.list_of_pairs ll1 in
  let opt1=Option.find_and_stop (
     fun (l1,l2)->
     let temp2=Private.low_level_analizer([],l1,l2) in
     if temp2=Disjointness_confirmed then None else
     Some(temp2)
  ) temp1 in
  let temp2=Listennou.universal_delta_list(ll1) in
  let opt2=Option.find_and_stop (
    fun (x,y)->
    if Hrecognizer_related_order.for_lists x y=Total_ordering.Greater 
    then Some(x,y)
    else None
 ) temp2 in
 (opt1,opt2);;  

let repair_disjunction_of_chains ll=
    if quick_check_on_disjunction_of_chains ll =(None,None) then ll else
    let temp1=Private.Repair.iterator (false,ll) in
    Ordered.diforchan_plaen Hrecognizer_related_order.for_lists temp1;;

let quick_check_on_recognizer x=match x with
    Nonatomic_hrecognizer.Disjunction_of_chains(name,ll)->
    quick_check_on_disjunction_of_chains ll
   |Ordered_disjunction(name,l)->
      let ll=Image.image (fun z->[z]) l in
      quick_check_on_disjunction_of_chains ll
   |_->(None,None);;    

let repair_recognizer x=match x with
 Nonatomic_hrecognizer.Disjunction_of_chains(name,ll)->
   Nonatomic_hrecognizer.Disjunction_of_chains(name,repair_disjunction_of_chains ll)
|Ordered_disjunction(name,l)->
   let ll=Image.image (fun z->[z]) l in
   Nonatomic_hrecognizer.Disjunction_of_chains(name,repair_disjunction_of_chains ll)
|_->x;;
  
let rec repair_system (completed,changed_ones,to_be_completed)=
   match to_be_completed with
   []->List.rev completed
   |a::other_ones->
     let temp_a=Nonatomic_hrecognizer.replace_inside changed_ones a in
     let repaired_a=repair_recognizer temp_a in
     if repaired_a=a
     then repair_system (repaired_a::completed,changed_ones,other_ones)
     else repair_system (repaired_a::completed,repaired_a::changed_ones,other_ones);;


let quick_check_on_disjunction_list_of_recognizers  old_l=
  let l=Image.image (fun rcgzr->
  ( Nonatomic_hrecognizer.name rcgzr,rcgzr)) old_l in
  let temp1=Uple.list_of_pairs l in
  let opt1=Option.find_and_stop (
     fun ((x1,rcgzr1),(x2,rcgzr2))->
     let temp2=Private.low_level_analizer([],[rcgzr1],[rcgzr2]) in
     if temp2=Disjointness_confirmed then None else
     Some(temp2)
  ) temp1 in
  let temp2=Listennou.universal_delta_list(l) in
  let opt2=Option.find_and_stop (
    fun ((x1,rcgzr1),(x2,rcgzr2))->
    if Hrecognizer_related_order.for_lists [rcgzr1] [rcgzr2]=Total_ordering.Greater 
    then Some(x1,x2)
    else None
 ) temp2 in
 (opt1,opt2);;  


let repair_disjunction_list_of_recognizers 
  old_counter_value main_l=
  if quick_check_on_disjunction_list_of_recognizers main_l =(None,None) then None else
  let ll=Image.image (fun rcgzr->
     ( Nonatomic_hrecognizer.name rcgzr,
     Nonatomic_hrecognizer.write_as_chain_list rcgzr)  
  ) main_l in
  let temp1=Private.Repair_Labelled.iterator  (false,ll) in
  let local_counter=ref(old_counter_value) 
  and local_accu=ref[] in 
  let temp2=Prepared.partition_according_to_fst temp1 in
  let outers_with_their_suites=Image.image (fun (x,l)->
     if List.length(l)=1
     then (Nonatomic_hrecognizer.chain x (List.hd l),[])
     else  
     (* if we get here, new preliminary definitions will be necessary *)
     let ttemp4=Ennig.index_everything l in
     let j0=(!local_counter) in
     let ttemp5=Image.image(
         fun (t,chain_for_t)->
           let name="anon_"^(string_of_int (j0+t) ) in
           let abstract_version=
           Abstractified_nonatomic_hrecognizer.Chain(name,
           Image.image Nonatomic_hrecognizer.name chain_for_t) in
           let concrete_version=Nonatomic_hrecognizer.chain name chain_for_t in
           (abstract_version,concrete_version)
     ) ttemp4 in
     let new_recgzr=Nonatomic_hrecognizer.ordered_disjunction x 
        (Image.image snd ttemp5) in
     let _=(
       local_accu := new_recgzr::(!local_accu);
       local_counter:=j0+(List.length l)
      ) in
     (new_recgzr,ttemp5)
    )  temp2 in
  let outers_without_their_suites=List.rev(!local_accu) in  
  Some(!local_counter,outers_with_their_suites,outers_without_their_suites);;


