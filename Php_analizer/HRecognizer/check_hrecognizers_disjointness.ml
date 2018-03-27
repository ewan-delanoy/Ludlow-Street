(*

#use"Php_analizer/HRecognizer/check_hrecognizers_disjointness.ml";;

*)

let rec flatten_nonatomic_hrecognizer x=match x with
  Nonatomic_hrecognizer.Chain(_,l)->
      List.flatten (Image.image flatten_nonatomic_hrecognizer l)
 |Nonatomic_hrecognizer.Ordered_disjunction(_,l)->
      if List.length l =1
      then flatten_nonatomic_hrecognizer(List.hd l)
      else [x]
 |_->[x];;

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

let rec helper_for_left_shadow (graet,da_ober)=
    match da_ober with
     []->(graet,[])
    |a::peurrest->
       let opt=constant_aspect a in
       if opt=None then (graet,da_ober) else 
       helper_for_left_shadow (graet^(Option.unpack opt),peurrest);;

let left_shadow_for_atomic_hrecognizer =function
 Atomic_hrecognizer.Constant(s)->s
|Atomic_hrecognizer.Later_constant(s)->""
|Atomic_hrecognizer.Star(l_chr)->""
|Atomic_hrecognizer.Star_outside(l_chr)->""
|Atomic_hrecognizer.Enclosed(opener,closer)->String.make 1 opener
|Atomic_hrecognizer.Simple_quoted->"'"
|Atomic_hrecognizer.Double_quoted->"\"";;


let rec left_shadow=function
        Nonatomic_hrecognizer.Leaf(_,atm)->left_shadow_for_atomic_hrecognizer atm
        |Nonatomic_hrecognizer.Chain(_,l)->
            let (graet1,da_ober1)=helper_for_left_shadow ("",l) in
            (match da_ober1 with
             []->graet1
             |a::peurrest->(graet1)^(left_shadow a)
            )
        |Nonatomic_hrecognizer.Ordered_disjunction(_,l)->
                  if List.length(l)=1 then left_shadow(List.hd l) else ""
        |Nonatomic_hrecognizer.Keyword_avoider(_,(x,_))->left_shadow x          
        |Nonatomic_hrecognizer.Star(_,_)->""
        |Nonatomic_hrecognizer.Maybe(_,_)->"";;

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


exception Unknown_disjointness of 
   Nonatomic_hrecognizer.t * Nonatomic_hrecognizer.t;;


let test_for_disjointness x y=
   let lsx=left_shadow x
   and lsy=left_shadow y
   in
   if test_for_string_strict_disjointness lsx lsy
   then true
   else raise(Unknown_disjointness(x,y))
   ;;


let rec compute_leftmost_difference (l1,l2)=
   match l1 with []->None |a1::b1->
   (
    match l2 with []->None |a2::b2->
    if (Nonatomic_hrecognizer.name a1)=
       (Nonatomic_hrecognizer.name a2)
    then compute_leftmost_difference  (b1,b2)
    else Some(a1,a2) 
   );;

let main_problem_finder (l1,l2)=
   match compute_leftmost_difference (l1,l2) with
    None->false
   |Some(a1,a2)->not(test_for_disjointness a1 a2);;



