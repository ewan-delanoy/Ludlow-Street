(*

#use"Php_analizer/HRecognizer/check_jrecognizers_disjointness.ml";;

*)

exception Repair_exn of 
(Nonatomic_jrecognizer.t list * Nonatomic_jrecognizer.t * Nonatomic_jrecognizer.t * 
                                Nonatomic_jrecognizer.t list * Nonatomic_jrecognizer.t list);;

module Private=struct


let constant_aspect_for_atomic_hrecognizer =function
   Atomic_hrecognizer.Constant(s)->Some(s)
  |_->None;;

let rec constant_aspect=function
 Nonatomic_jrecognizer.Leaf(_,atm)->constant_aspect_for_atomic_hrecognizer atm 
|Nonatomic_jrecognizer.Chain(_,l)->
      if List.length(l)>1 then None else constant_aspect(List.hd l)    
|Nonatomic_jrecognizer.Ordered_disjunction(_,l)->
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
        Nonatomic_jrecognizer.Leaf(_,atm)->common_prefix_for_atomic_hrecognizer atm
        |Nonatomic_jrecognizer.Chain(_,l)->
            let (graet1,da_ober1)=extract_left_constants ("",l) in
            (match da_ober1 with
             []->graet1
             |a::peurrest->(graet1)^(common_prefix a)
            )
        |Nonatomic_jrecognizer.Ordered_disjunction(_,l)->
                  Strung.largest_common_prefix(Image.image common_prefix l)
        |Nonatomic_jrecognizer.Keyword_avoider(_,(x,_))->common_prefix x;;

exception First_char_for_atomic of Atomic_hrecognizer.t;;

let first_char_for_atomic_hrecognizer x=match x with
        Atomic_hrecognizer.Constant(s)->Tidel.singleton(String.get s 0)
       |Atomic_hrecognizer.Later_constant(s)->raise(First_char_for_atomic(x))
       |Atomic_hrecognizer.Star(l_chr)->raise(First_char_for_atomic(x))
       |Atomic_hrecognizer.Star_outside(l_chr)->raise(First_char_for_atomic(x))
       |Atomic_hrecognizer.Enclosed(opener,closer)->Tidel.singleton opener
       |Atomic_hrecognizer.Simple_quoted->Tidel.singleton('\'')
       |Atomic_hrecognizer.Double_quoted->Tidel.singleton('"');;

exception First_char_for_nonatomic of Nonatomic_jrecognizer.t;;       

let rec naive_first_char_for_nonatomic_hrecognizer x=match x with
        Nonatomic_jrecognizer.Leaf(_,atm)->first_char_for_atomic_hrecognizer atm
        |Nonatomic_jrecognizer.Chain(_,l)->naive_first_char_for_nonatomic_hrecognizer (List.hd l)
        |Nonatomic_jrecognizer.Ordered_disjunction(_,l)->
                  Tidel.big_teuzin(Image.image naive_first_char_for_nonatomic_hrecognizer l)
        |Nonatomic_jrecognizer.Keyword_avoider(_,(x,_))->naive_first_char_for_nonatomic_hrecognizer x ;;

let first_char_for_nonatomic_hrecognizer x =
  try naive_first_char_for_nonatomic_hrecognizer x with
  _->raise(First_char_for_nonatomic(x));;

let keyword_avoider_aspect=function
  Nonatomic_jrecognizer.Keyword_avoider(_,data)->Some(data)
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

let naive_test_for_immediate_disjointness x y=
   if test_for_string_strict_disjointness (common_prefix x) (common_prefix y)
   then true
   else
   if Tidel.kengeij_goullo
     (first_char_for_nonatomic_hrecognizer x)
     (first_char_for_nonatomic_hrecognizer y)
   then true
   else  
   check_avoider_case x y
   ;;

let test_for_immediate_disjointness x y=
   try  naive_test_for_immediate_disjointness x y with _->false;;  

let rec compute_leftmost_difference (graet,l1,l2)=
   match l1 with []->None |a1::b1->
   (
    match l2 with []->None |a2::b2->
    if (Nonatomic_jrecognizer.name a1)=
       (Nonatomic_jrecognizer.name a2)
    then compute_leftmost_difference  (a1::graet,b1,b2)
    else Some(List.rev(graet),a1,a2,b1,b2) 
   );;

let check_compare_two_chains (l1,l2)=
    match compute_leftmost_difference ([],l1,l2) with
     None->None
    |Some(graet,a1,a2,b1,b2)->if test_for_immediate_disjointness a1 a2
                  then None
                  else Some(graet,a1,a2,b1,b2);;   

let check_disjunction st_ll=
  let ll=Standard_jdisjunction.unveil st_ll in
  let temp1=Uple.list_of_pairs ll in
  Option.find_and_stop  check_compare_two_chains temp1;;
             

let expand_because_of_disjointness st_ll (graet,a,b) =
  let ll=Standard_jdisjunction.unveil st_ll in
   let whole=graet@(a::b) in
   match a with
  Nonatomic_jrecognizer.Chain(_,_)->
       let new_whole = graet@((Nonatomic_jrecognizer.flatten a)@b) in
       (false,Standard_jdisjunction.veil(Image.image(fun x->if x=whole then new_whole else x) ll))
  |Nonatomic_jrecognizer.Ordered_disjunction(_,l)->
        let temp1=Image.image (fun component->graet@(component::b)) l in
        let temp2=Image.image (fun x->if x=whole then temp1 else [x]) ll in
        (false,Standard_jdisjunction.veil(List.flatten temp2))
  |_->(true,st_ll);;

  

let pusher_for_repairing (_,st_ll)=
    match  check_disjunction st_ll with
    None->(true,st_ll)
    |Some(graet,a1,a2,b1,b2)->
       let (expansion_failed1,st_ll1)=expand_because_of_disjointness st_ll (graet,a1,b1) in
       let (expansion_failed2,st_ll2)=expand_because_of_disjointness st_ll1 (graet,a2,b2) in
       if expansion_failed1 && expansion_failed2
       then raise(Repair_exn(graet,a1,a2,b1,b2))
       else (false,st_ll2);; 

let rec iterator_for_repairing (end_reached,pair)=
    if end_reached then pair else
    iterator_for_repairing ( pusher_for_repairing (end_reached,pair));;
   
end;;

let check l1 l2=Private.check_compare_two_chains (l1,l2);;

let repair ll=Private.iterator_for_repairing(false,
  Standard_jdisjunction.veil ll);; 







