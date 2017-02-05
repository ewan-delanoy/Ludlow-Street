(*

#use"simplify_without_orderings.ml";;

This module has several functions deleting redundancies in
several contexts, using only equality (= or <>) and never
<,<=,>, or >=.

For larger lists, sorting and orderings should be used.

*)


let partition_carefully_according_to_snd l=
 let rec partition_carefully_according_to_snd0=
 (function
   (graet,da_ober)->match da_ober with
   []->List.rev(graet)
   |(x0,y0)::peurrest->
     let temp1=List.partition(function (x,y)->y=y0)(peurrest) in
	 let temp2=x0::(Image.image(fst)(fst temp1)) in
     partition_carefully_according_to_snd0((y0,temp2)::graet,snd(temp1))
 ) in
 partition_carefully_according_to_snd0([],l);;

let partition_carefully_according_to_f f l=
 let temp1=Image.image(function x->(x,f x) )(l) in
 partition_carefully_according_to_snd temp1;;

let generic_symmetric_decomposition f old_l1 old_l2=
  let rec tempf=(fun
    (g_common,g1_only,l1,l2)->match l1 with
    []->(List.rev(g_common),List.rev(g1_only),Image.image f l2)
    |x1::peurrest1->
       let y1=f x1 in
       let (left,opt,right)=Three_parts.select_center_element 
       (fun x->(f x)=y1) l2 in
       let remains=left@right in
       (
         match opt with
         None->tempf(g_common,y1::g1_only,peurrest1,remains)
         |Some(x2)->tempf((x1,x2)::g_common,g1_only,peurrest1,remains)
       )
  ) in
  tempf([],[],old_l1,old_l2);;
  
let complete_symmetric_decomposition l1 l2=
  let (g_common,g1,g2)=generic_symmetric_decomposition (fun x->x) l1 l2 in
  (Image.image fst g_common,g1,g2);; 
  
let symmetric_decomposition l1 l2=
  let (_,g1,g2)=complete_symmetric_decomposition l1 l2 in (g1,g2);;  
  
  
  