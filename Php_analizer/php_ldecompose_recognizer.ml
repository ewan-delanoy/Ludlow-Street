(*

#use"Php_analizer/php_ldecompose_recognizer.ml";;

*)

  
let niy=[];;

let ld_leaf sel=niy;;

let ld_generalized old_f gen rcgzr=niy;;

exception Empty_chain;;

let ld_chain old_f l=
   if l=[] then raise(Empty_chain) else
   let h=List.hd(l) and peurrest=List.tl(l) in
   let temp1=old_f(h) in
   Image.image (fun (x,y)->
   (x,Php_constructible_recognizer.Chain (y::peurrest))) temp1;;

exception Nondisjoint of  Php_constructible_recognizer.t list;;

let ld_disjunction old_f l=
    let temp1=List.flatten(Image.image old_f l) in
    let temp2=Image.image fst temp1 in
    let temp3=Uple.list_of_pairs temp2 in
    if List.exists (fun (x,y)->
      not(Php_projected_token_set.kengeij_goullo x y)
    ) temp3
    then raise(Nondisjoint(l))
    else 
    let temp4=Ordered.diforchan_plaen Php_projected_token_set.order temp2 in
    let temp5=Image.image (fun x->(x,List.assoc x temp1)) temp4 in
    temp5;;


let rec ld =function
   Php_constructible_recognizer.Leaf(sel)->ld_leaf sel
  |Php_constructible_recognizer.Generalized(gen,rcgzr)->ld_generalized ld gen rcgzr
  |Php_constructible_recognizer.Chain(l)->ld_chain ld l
  |Php_constructible_recognizer.Disjunction(l)->ld_disjunction ld l
  |Php_constructible_recognizer.End_already_reached->[];;


