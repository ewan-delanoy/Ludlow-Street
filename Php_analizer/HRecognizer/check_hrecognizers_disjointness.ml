(*

#use"Php_analizer/HRecognizer/check_hrecognizers_disjointness.ml";;

*)



let constant_aspect_for_atomic_hrecognizer =function
   Atomic_hrecognizer.Constant(s)->Some(s)
  |_->None;;

let rec constant_aspect=function
 Nonatomic_hrecognizer.Leaf(_,atm)->constant_aspect_for_atomic_hrecognizer atm 
|Nonatomic_hrecognizer.Chain(_,l)->
      if List.length(l)>1 then None else constant_aspect(List.hd l)    
|Nonatomic_hrecognizer.Ordered_disjunction(_,l)->
      if List.length(l)>1 then None else constant_aspect(List.hd l)  
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


let rec common_prefix=function
        Nonatomic_hrecognizer.Leaf(_,atm)->common_prefix_for_atomic_hrecognizer atm
        |Nonatomic_hrecognizer.Chain(_,l)->
            let (graet1,da_ober1)=extract_left_constants ("",l) in
            (match da_ober1 with
             []->graet1
             |a::peurrest->(graet1)^(common_prefix a)
            )
        |Nonatomic_hrecognizer.Ordered_disjunction(_,l)->
                  Strung.largest_common_prefix(Image.image common_prefix l)
        |Nonatomic_hrecognizer.Star(_,_)->""
        |Nonatomic_hrecognizer.Maybe(_,_)->""          
        |Nonatomic_hrecognizer.Keyword_avoider(_,(x,_))->common_prefix x;;


let first_char_for_atomic_hrecognizer x=match x with
        Atomic_hrecognizer.Constant(s)->Some(Tidel.singleton(String.get s 0))
       |Atomic_hrecognizer.Later_constant(s)->None
       |Atomic_hrecognizer.Star(l_chr)->None
       |Atomic_hrecognizer.Star_outside(l_chr)->None
       |Atomic_hrecognizer.Enclosed(opener,closer)->Some(Tidel.singleton opener)
       |Atomic_hrecognizer.Simple_quoted->Some(Tidel.singleton('\''))
       |Atomic_hrecognizer.Double_quoted->Some(Tidel.singleton('"'));;

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
        |Nonatomic_hrecognizer.Keyword_avoider(_,(x,_))->
                   first_char_for_nonatomic_hrecognizer x ;;



let keyword_avoider_aspect=function
  Nonatomic_hrecognizer.Keyword_avoider(_,data)->Some(data)
  |_->None;;

let check_nonsymmetric_avoider_case x y=
   let opt1=constant_aspect x
   and opt2=keyword_avoider_aspect y in
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

let test_for_disjointness_via_first_chars x y=
  let opt1=first_char_for_nonatomic_hrecognizer x
  and opt2=first_char_for_nonatomic_hrecognizer y in
  if (opt1=None)||(opt2=None)
  then false
  else Tidel.kengeij_goullo (Option.unpack opt1) (Option.unpack opt2);; 

let main_test_for_disjointness x y=
   if test_for_string_strict_disjointness (common_prefix x) (common_prefix y)
   then true
   else
   if test_for_disjointness_via_first_chars x y
   then true
   else  
   check_avoider_case x y
   ;;

let expand_pair (x,y,graet,a1,b1,a2,b2)=
   match a1 with
   |Nonatomic_hrecognizer.Chain(_,l)->Some([x,y,graet,l@b1,a2::b2])
   |Nonatomic_hrecognizer.Ordered_disjunction(_,l)->
      Some(
        Image.image (fun elt->(x,y,graet,elt::b1,a2::b2)) l
      )
   |Nonatomic_hrecognizer.Maybe(_,sa1)->
      Some(
        [(x,y,graet,b1,a2::b2);(x,y,graet,sa1::b1,a2::b2)]
      )   
   |_->
    (
       match a2 with
     |Nonatomic_hrecognizer.Chain(_,l)->Some([x,y,graet,a1::b1,l@b2])
     |Nonatomic_hrecognizer.Ordered_disjunction(_,l)->
      Some(
        Image.image (fun elt->(x,y,graet,a1::b1,elt::b2)) l
      )
      |Nonatomic_hrecognizer.Maybe(_,sa2)->
      Some(
        [(x,y,graet,a1::b1,b2);(x,y,graet,a1::b1,sa2::b2)]
      )  
      |_->None
    )
   ;;

type walker =  
  ( Nonatomic_hrecognizer.t * 
    Nonatomic_hrecognizer.t * 
    Nonatomic_hrecognizer.t list * 
    Nonatomic_hrecognizer.t list *
    Nonatomic_hrecognizer.t list) ;;

type  pusher_result=
      Disjointness_confirmed
     |Pushes_found of walker list
     |No_push_found of walker;;

let low_level_pusher ((x,y,graet,l1,l2):walker)=
   if (l1=[])||(l2=[])
   then Disjointness_confirmed
   else
   let (a1,b1)=Listennou.ht l1
   and (a2,b2)=Listennou.ht l2 in
   if Nonatomic_hrecognizer.name a1=Nonatomic_hrecognizer.name a2
   then Pushes_found [x,y,a1::graet,b1,b2]
   else 
   if main_test_for_disjointness a1 a2
   then Disjointness_confirmed
   else 
   match expand_pair (x,y,graet,a1,b1,a2,b2) with
   None->No_push_found(x,y,graet,a1::b1,a2::b2)
   |Some(l)->Pushes_found(l);;

let pusher_for_fault_finding (graet,da_ober)=
  match da_ober with
  []->(graet,[])
  |wlkr::peurrest->
     (
       match low_level_pusher wlkr with
       Disjointness_confirmed->(graet,peurrest)
      |Pushes_found(l)->(graet,l@peurrest)
      |No_push_found(wlkr2)->(wlkr2::graet,peurrest)
     ) ;;  

let rec iterator_for_fault_finding p=
    if snd(p)=[] then List.rev(fst p) else
    iterator_for_fault_finding(pusher_for_fault_finding p);;

let find_fault x y=
   iterator_for_fault_finding ([],[x,y,[],[x],[y]]);;





