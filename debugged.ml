module Sctotal_ordering=struct 
type result=Lower |Equal |Greater;;

type 'a t=('a->'a->result);;

let leq (computer:'a t) x y=
   let v=computer(x)(y) in
   (v=Lower)||(v=Equal);;
   
 let lt (computer:'a t) x y=(computer(x)(y)=Lower);;   
 
 let geq (computer:'a t) x y=
   let v=computer(x)(y) in
   (v=Lower)||(v=Equal);;
   
 let gt (computer:'a t) x y=(computer(x)(y)=Greater);;   
 
 let from_lt f=
   let temp1=(fun x y->
     if f(x)(y)
     then Lower
     else if f(y)(x)
          then Greater
          else Equal
   ) in
   (temp1:'a t);;
 
 let standard_completion f g=
  let answer=(fun x y->
   if f(y)(x)
   then Greater
   else if f(x)(y)
        then Lower
        else if g(x)(y)
             then Equal
             else if x<y
                  then Lower
                  else Greater
  ) in
  (answer: 'a t);;
 
 let standard=((fun x y->
    if x=y
    then Equal
    else if x<y
         then Lower
         else Greater
 ): 'a t);;
 
 let completion f (g:'a t)=
  let answer=(fun x y->
   if f(y)(x)
   then Greater
   else if f(x)(y)
        then Lower
         else g(x)(y)
  ) in
  (answer: 'a t);;
 
 let product (f:'a t) (g:'b t)=
  ((fun (x1,y1) (x2,y2)->
     let t=f(x1)(x2) in
     if t<>Equal 
     then t
     else g y1 y2
 ): ('a*'b) t);;
 
 let triple_product (f:'a t) (g:'b t) (h:'c t)=
  ((fun (x1,y1,z1) (x2,y2,z2)->
     let tx=f(x1)(x2) in
     if tx<>Equal 
     then tx
     else let ty=g(y1)(y2) in
          if ty<>Equal 
          then ty
          else h z1 z2
 ): ('a*'b*'c) t);;
 
 let rec lex_compare (f:'a t)=
  let rec tempf=(
    fun l1 l2->
     if l1=l2 then Equal else
     if l1=[] then Lower else
     if l2=[] then Greater else
     let t=f(List.hd l1)(List.hd l2) in
     if t<>Equal then t else
     tempf (List.tl l1) (List.tl l2)) in
     (tempf:>( ('a list) t));;
 


let silex_compare (f:'a t)=
  let tempf=(
    fun l1 l2->
     let t=standard(List.length l1)(List.length l2) in
     if t<>Equal then t else
     lex_compare f l1 l2
  ) in
   (tempf:>( ('a list) t));;
 

let from_list (l:'a list)=
  let tempc=(fun x y->
  let rec tempf=(function
   []->(x<y)
   |u::peurrest->if u=x then List.mem(y)(peurrest)
                 else if u=y then false
                 else tempf(peurrest)
  ) in
  tempf l) in
  from_lt tempc;;

let min (f:'a t)=function
 []->failwith("Min of the empty set is undefined")
 |a::b->
   let rec tempf=(fun
    (candidate,l)->match l with
      []->candidate
      |c::peurrest->if f(c)(candidate)=Lower
                    then tempf(c,peurrest)
                    else tempf(candidate,peurrest)
   ) in
   tempf(a,b);;

let max (f:'a t)=function
 []->failwith("Max of the empty set is undefined")
 |a::b->
   let rec tempf=(fun
    (candidate,l)->match l with
      []->candidate
      |c::peurrest->if f(c)(candidate)=Greater
                    then tempf(c,peurrest)
                    else tempf(candidate,peurrest)
   ) in
   tempf(a,b);;
   
let minimize_it_with_care (cf:'a t) 
   f=function
[]->failwith("careful min on empty set undefined")
|x::y->
 let rec minimize_it_with_care0=(function
  (current_candidates,current_value,da_ober)->match da_ober with
  []->(current_value,List.rev(current_candidates))
  |a::peurrest->let va=f(a) in
                let howl=cf(va)(current_value) in
                if howl=Lower
				then minimize_it_with_care0([a],va,peurrest)
				else if howl=Equal
				     then minimize_it_with_care0(a::current_candidates,current_value,peurrest)
					 else minimize_it_with_care0(current_candidates,current_value,peurrest)
 ) 
in
 minimize_it_with_care0([x],f(x),y);;


let maximize_it_with_care (cf:'a t) 
   f=function
[]->failwith("careful max on empty set undefined")
|x::y->
 let rec maximize_it_with_care0=(function
  (current_candidates,current_value,da_ober)->match da_ober with
  []->(current_value,List.rev(current_candidates))
  |a::peurrest->let va=f(a) in
                let howl=cf(va)(current_value) in
                if howl=Greater
				then maximize_it_with_care0([a],va,peurrest)
				else if howl=Equal
				     then maximize_it_with_care0(a::current_candidates,current_value,peurrest)
					 else maximize_it_with_care0(current_candidates,current_value,peurrest)
 ) 
in
 maximize_it_with_care0([x],f(x),y);;

let modify_locally (f:'a t) l=
  let big_m=max(f)(l) in
  let tempf=(fun x y->
    if List.mem(x)(l)
    then if List.mem(y)(l)
         then if x=y
              then Equal
              else (from_list l x y)
         else f big_m y
    else if List.mem(y)(l)
         then f x big_m
         else f x y
  
  ) in
  (tempf:>( 'a t));;


let for_characters=let tempf=(fun x y->
  if x<y then Lower else
  if y<x then Greater else
  Equal
  ) in (tempf:>char t);;



 
 
 
 
 
 
 end;;


module Scimage=struct (*

The most used function in all those modules !

*)


let image f l=
  let rec tempf=(fun
   (graet,da_ober)->match da_ober with
   []->List.rev graet
   |a::peurrest->tempf(f(a)::graet,peurrest)
  ) in
  tempf([],l);;


 end;;


module Scbasic=struct let delta_list l=
let rec sub_f=
(function (accu,a,rl)->match rl with
[]->List.rev(accu)
|b::x->sub_f((b-a)::accu,b,x)
) in
match l with
[]->[]
|u::v->sub_f([],u,v);;

exception Big_cut_exn of int*int;;

let big_cut r l=let rec tempf=
(function (j,kleiz,dehou)->
if j=0 then (List.rev(kleiz),dehou) else 
match dehou with
[]->raise(Big_cut_exn(r,List.length l))
|a::peurrest->tempf(j-1,a::kleiz,peurrest)
) in
tempf(r,[],l);;

let big_head r l=if (r>(List.length l)) then l else fst(big_cut(r)(l));;

let big_tail r l=if (r>(List.length l)) then [] else snd(big_cut(r)(l));;

let big_sum=function
[]->0
|a::b->List.fold_left(+)(a)(b);;

let cumsum l=
  if l=[] then [] else
  let rec cumsum0=(fun
    (da_ober,s,graet)->
      match da_ober with
      []->List.rev(s::graet)
      |a::peurrest->cumsum0(peurrest,a+s,s::graet)
  ) in
  cumsum0(List.tl(l),List.hd(l),[]);;
  
let functional_if(bowl,x,y)=if bowl then x else y;; 

let nearest_int_of_float x=
  let i=int_of_float x in
  let fi=float i in
  if fi<x
  then if x<fi+.0.5 then i else i+1
  else if fi-.0.5<x then i else i-1;;
 

let careful_if bowl f1 arg1 f2 arg2=if bowl then f1 arg1 else f2 arg2;;

let frac_floor0 a b=
 (*we assume that b is positive *)
      if (a>=0)||((a mod b)=0) 
      then (a/b)
      else -(((-a)/b)+1);;
 
let frac_floor a b=
 if (b=0) 
 then failwith("division by zero in frac_floor")
 else if (b>0)
      then frac_floor0 a b
      else frac_floor0 (-a) (-b);;

let frac_ceiling0 a b=
 (*we assume that b is positive *)
 if (a mod b)=0 
 then (a/b) 
 else if (a>=0) 
      then (a/b)+1
      else -(( abs a)/b);;
 
let frac_ceiling a b=
 if (b=0) 
 then failwith("division by zero in frac_ceiling")
 else if (b>0)
      then frac_ceiling0 a b
      else frac_ceiling0 (-a) (-b);;
 
let nonequal_floor a b=
   let q=frac_floor a b in
   if (a mod b)=0 then q-1 else q;;
   
let nonequal_ceiling a b=
   let q=frac_ceiling a b in
   if (a mod b)=0 then q+1 else q;;   

let ceiling_mod a b=
 match (a mod b) with 0->b |k->k;;
 


 end;;


module Sclistennou=struct 
(*

#use"listennou.ml";;

*)




let rec uncurrified_rev_append (x,y)=match x with
[]->y
|a::peurrest->uncurrified_rev_append (peurrest,a::y);;

let rec uncurrified_append (x,y)=uncurrified_rev_append (List.rev x,y);;



let didrochan x=
let rec didrochan0=
(function (u,accu1,accu2,bowl)->match u with
 []->(accu1,accu2)
 |a::b->if bowl
        then didrochan0(b,a::accu1,accu2,false)
        else didrochan0(b,accu1,a::accu2,true))  
in
didrochan0(x,[],[],true);;

let find_index x ll=
let rec sub_f=
(function (j,l)->match l with
[]->(-1)      
|u::v->if u=x then j else sub_f(j+1,v)) in
sub_f(1,ll);;



let morzholan f x=
let rec sub_f=(function (u,v)->if u=v then u else sub_f(v,f(v)))
in sub_f(x,f(x));;

let rec morzhol_bihan f k x=
if k=0 then x else morzhol_bihan f (k-1) (f(x));;



let power_set l=
let rec tempf=
(function (da_ober,graet)->match da_ober with
[]->graet
|a::peurrest->tempf(peurrest,graet@(Scimage.image(function y->a::y)(graet)))
) in
tempf(List.rev(l),[[]]);;

let big_head=Scbasic.big_head;; 

let big_tail=Scbasic.big_tail;;


let fold_right f x0 l=List.fold_left(function x->(function a->f a x)) x0 l;;


let assoc l x=
 try Some(List.assoc x l) with
 Not_found->None;; 
 
 let rec r_assoc l x=
   let rec tempf=(function
     []->None
     |(u,v)::peurrest->if v=x
                   then Some(u)
				   else tempf(peurrest)
   ) in
   tempf l;;

let universal_delta_list l=
let rec sub_f=
(function (accu,a,rl)->match rl with
[]->List.rev(accu)
|b::x->sub_f((a,b)::accu,b,x)
) in
match l with
[]->[]
|u::v->sub_f([],u,v);;

 
let delete_redundancies r l=
 let rec tempf=(function
   (graet,da_ober)->match da_ober with
   []->List.rev(graet)
   |x::peurrest->
     if List.exists(function y->r y x)(peurrest)
     then tempf(graet,peurrest)
     else let temp1=List.filter(function y->not(r x y))(peurrest) in
          tempf(x::graet,temp1)
 ) in
 tempf([],l);;

let nonredundant_version l=
  let rec tempf=(
    fun (graet,da_ober)->
      match da_ober with
      []->List.rev graet
      |a::peurrest->if List.mem a graet
                    then tempf(graet,peurrest)
                    else tempf(a::graet,peurrest)
  ) in
  tempf([],l);;


let connected_components_in_intlist l=
  if l=[] then [] else
  let rec tempf=(fun
  (graet,i,j,da_ober)->
     match da_ober with
      []->List.rev((i,j)::graet)
     |a::peurrest->if a=(j+1)
                   then tempf(graet,i,j+1,peurrest)
                   else tempf((i,j)::graet,a,a,peurrest)) in
   tempf([],List.hd l,List.hd l,List.tl l);;

let rev_map f l=
   let rec tempf=(
     fun (graet,da_ober)->match da_ober with
     []->graet
     |a::peurrest->tempf((f a)::graet,peurrest)
   ) in
   tempf([],l);;
   
let constant_slices f l=
  if l=[] then [] else
  let (a0,l0)=(List.hd l,List.tl l) in
  let rec tempf=(fun 
    (graet,current_val,slice,da_ober)->
    match da_ober with
    []->List.rev((List.rev slice)::graet)
    |a::peurrest->
        let va=f(a) in
        if va=current_val
        then tempf(graet,current_val,a::slice,peurrest)
        else tempf((List.rev slice)::graet,va,[a],peurrest) 
  ) in   
  tempf ([],f a0,[a0],l0);;
   

let glued_slices c l=
  if l=[] then [] else
  let rec tempf=(fun 
    (graet,pure_glue,slice,slice_element,da_ober)->
    match da_ober with
    []->( 
          if pure_glue=[]
          then (
                 if slice=[]
                 then List.rev graet
                 else List.rev((List.rev slice)::graet)
               )  
          else List.rev([pure_glue]::graet)
        )  
    |a::peurrest->
        if a=c
        then (
              if pure_glue=[]
              then (
                    if slice_element<>[]
                    then tempf(graet,[c],(List.rev slice_element)::slice,[],peurrest)
                    else tempf(graet,[c],slice,slice_element,peurrest)
                   )
              else if pure_glue=[c]
                   then (
                          if slice=[]
                          then tempf(graet,[c;c],[],[],peurrest)
                          else tempf((List.rev slice)::graet,[c;c],[],[],peurrest)
                        )  
                   else tempf(graet,c::pure_glue,[],[],peurrest)
             )
        else ( 
               if pure_glue=[]
               then tempf(graet,[],slice,a::slice_element,peurrest)
               else (
                     if pure_glue=[c]
                     then (
                           if graet=[]
                           then tempf([[[c]]],[],[],[a],peurrest)
                           else tempf(graet,[],slice,[a],peurrest)
                           )
                     else tempf([pure_glue]::graet,[],[],[a],peurrest)
                    )
             ) 
  ) in   
  tempf ([],[],[],[],l);;

let hi=List.length;;
let rev=List.rev;;

(*
glued_slices 0 [1;2;3;0;4;5;0;6;7;8;0;0;0;9;10;0;11;0;12;13;0;0];;
glued_slices 0 [0;1;2;3;0;4;5;0;6;7;8;0;0;0;9;10;0;11;0;12;13;0;0];;
glued_slices 0 [0;0;1;2;3;0;4;5;0;6;7;8;0;0;0;9;10;0;11;0;12;13;0;0];;



*) end;;


module Scordered=struct (*
    hardcore version :
    
type 'a old_set=S of 'a list;;
let unsafe_set x=S(x);;
let forget_order (S x)=x;;


*)
(*
    soft version :
    
type 'a old_set='a list;;
let unsafe_set=((fun x->x):>('a list->'a old_set));;
let forget_order=((fun x->x):>('a old_set->'a list));;


*)

type 'a old_set=S of 'a list;;

let unsafe_set x=S(x);;
let forget_order (S x)=x;;

let eq (S x) (S y)=(x=y);;
let neq (S x) (S y)=(x<>y);;

let kreskus (kenver:'a Sctotal_ordering.t) l=
  if List.length(l)<2 then true else
  let rec tempf=(function
  (a,da_ober)->match da_ober with
   []->true
   |b::peurrest->if (kenver(a)(b)<>Sctotal_ordering.Greater)
                 then tempf(b,peurrest)
                 else false
  ) in
  tempf(List.hd l,List.tl l);;
  
let kreskus_strizh (kenver:'a Sctotal_ordering.t) l=
  if List.length(l)<2 then true else
  let rec tempf=(function
  (a,da_ober)->match da_ober with
   []->true
   |b::peurrest->if (kenver(a)(b)=Sctotal_ordering.Lower)
                 then tempf(b,peurrest)
                 else false
  ) in
  tempf(List.hd l,List.tl l);;

let rec elfenn (kenver:'a Sctotal_ordering.t) x ol=
   let rec elfenn0=(function
    []->false
    |a::peurrest->match kenver(x)(a) with
       Sctotal_ordering.Lower->false
       |Sctotal_ordering.Equal->true
       |Sctotal_ordering.Greater->elfenn0 peurrest
   )  in
   elfenn0 (forget_order ol);;
		
            
let teuzin (kenver:'a Sctotal_ordering.t) ox oy=
let rec teuzin0=
(function (u,v,accu)->
if u=[] then (List.rev_append(accu)(v)) else
if v=[] then (List.rev_append(accu)(u)) else
let xu=List.hd(u) and yu=List.tl(u) 
and xv=List.hd(v) and yv=List.tl(v) in
match kenver(xu)(xv) with
   Sctotal_ordering.Lower->teuzin0(yu,v,xu::accu)
   |Sctotal_ordering.Equal->teuzin0(yu,yv,xu::accu)
   |Sctotal_ordering.Greater->teuzin0(u,yv,xv::accu)

) in
unsafe_set(teuzin0(forget_order ox,forget_order oy,[]));;

let rec diforchan (kenver:'a Sctotal_ordering.t) x=
  if List.length(x)<2
  then unsafe_set(x)
  else let temp1=Sclistennou.didrochan(x) in
       let y1=diforchan(kenver)(fst temp1)
       and y2=diforchan(kenver)(snd temp1) in
       teuzin kenver y1 y2;;
  
  
  let lemel (kenver:'a Sctotal_ordering.t) ox oy=
let rec lemel0=
(function (u,v,accu)->
if u=[] then (List.rev(accu)) else
if v=[] then (List.rev_append(accu)(u)) else
let xu=List.hd(u) and yu=List.tl(u) 
and xv=List.hd(v) and yv=List.tl(v) in
match kenver(xu)(xv) with
   Sctotal_ordering.Lower->lemel0(yu,v,xu::accu)
   |Sctotal_ordering.Equal->lemel0(yu,yv,accu)
   |Sctotal_ordering.Greater->lemel0(u,yv,accu)

) in
unsafe_set(lemel0(forget_order ox,forget_order oy,[]));;

let kengeij (kenver:'a Sctotal_ordering.t) ox oy=
let rec kengeij0=
(function (u,v,accu)->
if u=[] then (List.rev(accu)) else
if v=[] then (List.rev(accu)) else
let xu=List.hd(u) and yu=List.tl(u) 
and xv=List.hd(v) and yv=List.tl(v) in
match kenver(xu)(xv) with
   Sctotal_ordering.Lower->kengeij0(yu,v,accu)
   |Sctotal_ordering.Equal->kengeij0(yu,yv,xu::accu)
   |Sctotal_ordering.Greater->kengeij0(u,yv,accu)

) in
unsafe_set(kengeij0(forget_order ox,forget_order oy,[]));;

let kengeij_goullo (kenver:'a Sctotal_ordering.t) ox oy=
let rec kengeij_goullo0=
(function (u,v)->
if (u=[])||(v=[]) then true else
let xu=List.hd(u) and yu=List.tl(u) 
and xv=List.hd(v) and yv=List.tl(v) in
match kenver(xu)(xv) with
   Sctotal_ordering.Lower->kengeij_goullo0(yu,v)
   |Sctotal_ordering.Equal->false
   |Sctotal_ordering.Greater->kengeij_goullo0(u,yv)
) in
kengeij_goullo0(forget_order ox,forget_order oy);;


let ental (kenver:'a Sctotal_ordering.t) ox oy=
let rec ental0=
(function (u,v)->
if u=[] then true else
if v=[] then false else
let xu=List.hd(u) and yu=List.tl(u) 
and xv=List.hd(v) and yv=List.tl(v) in
match kenver(xu)(xv) with
   Sctotal_ordering.Lower->false
   |Sctotal_ordering.Equal->ental0(yu,yv)
   |Sctotal_ordering.Greater->ental0(u,yv)
) in
ental0(forget_order ox,forget_order oy);;

let min=((fun kenver x->match x with
  []->failwith("The empty set has no min")
  |a::b->
    let rec tempf=(fun 
     (trecher,da_ober)->match da_ober with
      []->trecher
      |c::peurrest->
        if kenver(c)(trecher)=Sctotal_ordering.Lower
        then tempf(c,peurrest)
        else tempf(trecher,peurrest)
    ) in
    tempf(a,b)):> ('a Sctotal_ordering.t -> 'a list->'a));;

let max=((fun kenver x->match x with
  []->failwith("The empty set has no max")
  |a::b->
    let rec tempf=(fun 
     (trecher,da_ober)->match da_ober with
      []->trecher
      |c::peurrest->
        if kenver(c)(trecher)=Sctotal_ordering.Greater
        then tempf(c,peurrest)
        else tempf(trecher,peurrest)
    ) in
    tempf(a,b)):> ('a Sctotal_ordering.t -> 'a list->'a));;

let cooperation_for_two kenver x y=
  List.filter (fun ox->
    forget_order ox<>[]
  ) [kengeij kenver x y;lemel kenver x y;lemel kenver y x];;
  
let expand_boolean_algebra kenver l=
  if List.length(l)<2 then l else
  let rec tempf=(fun 
    (graet,y0,etre,da_ober)->
      if etre=[] 
      then if da_ober=[]
           then y0::graet
           else let z0=List.hd(da_ober) and peurrest=List.tl(da_ober) in
                tempf([],z0,y0::graet,peurrest)
      else 
      let x0=List.hd(etre) and peurrest_etre=List.tl(etre) in
      let t1=kengeij kenver x0 y0 and t2=lemel kenver x0 y0 in
      let y1=lemel kenver y0 t1 in
      let temp1=List.filter (fun ox->forget_order ox<>[]) [t1;t2] in
      tempf(List.rev_append temp1 graet,y1,peurrest_etre,da_ober)
  )  in
  let x1=List.hd(l) and r1=List.tl(l) in
  tempf([],x1,[],r1);;

let length ox=List.length(forget_order ox);;
let image f ox=Scimage.image(f)(forget_order ox);;
let insert kenver x oy=teuzin kenver (unsafe_set [x])(oy);;
let exists f ox=List.exists f (forget_order ox);;
let safe_set kenver ox=if kreskus(kenver)(ox) 
                       then unsafe_set ox 
                       else diforchan kenver ox;;
let rev_map f ox=List.rev_map(f)(forget_order ox);;
let filter f ox=unsafe_set(List.filter(f)(forget_order ox));;
let for_all f ox=List.for_all(f)(forget_order ox);;
let singleton x=unsafe_set [x];;
let empty_set=unsafe_set [];;
let big_teuzin kenver x=List.fold_left (teuzin kenver) empty_set x;;
let big_kengeij kenver=function
   []->failwith("empty intersection undefined")
  |a::b->List.fold_left(kengeij kenver)(a)(b);;
let nelfenn kenver a ox=not(elfenn kenver a ox);;
let nental kenver a ox=not(ental kenver a ox);;
let eq ox oy=(forget_order ox)=(forget_order oy);;
  

 end;;


module Sctidel=struct (* file created using the "write_contents_of_ordered_list_module" function *)
(* in Creators/ordered_list_creator.ml *)

type 'a set='a Scordered.old_set;;
let lt x y=x<y;;
let cmp=Sctotal_ordering.standard;;
let unsafe_set=(Scordered.unsafe_set:>('a list-> 'a set));;
let forget_order=(Scordered.forget_order:>('a set->'a list));;

let kreskus_strizh x=Scordered.kreskus_strizh cmp x;;
let kreskus x=Scordered.kreskus cmp x;;

let elfenn=((fun a ox->Scordered.elfenn cmp a ox):>('a->'a set->bool));;
let teuzin=((fun ox oy->Scordered.teuzin cmp ox oy):>( 'a set->'a set->'a set));;
let diforchan=((fun x->Scordered.diforchan cmp x):>('a list->'a set));;
let lemel=((fun ox oy->Scordered.lemel cmp ox oy):>('a set->'a set->'a set));;
let ental=((fun ox oy->Scordered.ental cmp ox oy):>('a set->'a set->bool));;
let kengeij=((fun ox oy->Scordered.kengeij cmp ox oy):>'a set->'a set->'a set);;
let kengeij_goullo=((fun ox oy->Scordered.kengeij_goullo cmp ox oy):>'a set->'a set->bool);;
let min=((fun x->Scordered.min cmp x):>'a list->'a);;
let max=((fun x->Scordered.max cmp x):>'a list->'a);;

let hd ox=List.hd(forget_order ox);;
let image f ox=Scimage.image f (forget_order ox);;
let rev_map f ox=Scimage.image f (forget_order ox);;
let empty_set=unsafe_set [];;
let singleton x=unsafe_set [x];;
let filter f x=unsafe_set(List.filter(f)(forget_order x));;
let partition f ox=
         match List.partition(f)(forget_order ox) with
           (u,v)->(unsafe_set u,unsafe_set v);;
let length x=List.length(forget_order x);;

let nelfenn a ox=not(elfenn a ox);;
let nental ox oy=not(ental ox oy);;

let insert x oy=teuzin(singleton x) oy;;
let safe_set x=if kreskus_strizh(x) then unsafe_set(x) else diforchan(x);;
let outsert x oy=lemel(oy)(singleton x);;
let delta_set ox oy=teuzin(lemel ox oy)(lemel oy ox);;
let delta_distance ox oy=length(delta_set ox oy);;


let big_teuzin x=List.fold_left teuzin empty_set x;;
let big_kengeij=function
    []->failwith("empty intersection undefined")
    |a::b->List.fold_left(kengeij)(a)(b);;
    
    
let expand_boolean_algebra=
 ((fun x->Scordered.expand_boolean_algebra cmp x):>('a set list->('a set list)));; 
 
 
 end;;


module Scoption=struct 

exception Unpackable of string;;

let unpack_with_error_message s=function
None->raise(Unpackable(s))
|Some(x)->x;;

let unpack x =unpack_with_error_message "void is not unpackable" x;;


let propagate f=function
None->None
|Some(x)->Some(f(x));;

let rec find_it f =function
[]->None
|a::b->if f(a) then Some(a) else find_it(f)(b);;

let find_really f l=unpack(find_it f l);;

let rec filter_and_unpack f l=
 let rec filter0=(function
  (graet,da_ober)->match da_ober with
   []->List.rev(graet)
   |x::peurrest->match f(x) with
		None->filter0(graet,peurrest)
		|Some(y)->filter0(y::graet,peurrest)
 ) in
 filter0([],l);;

let rec partition_and_unpack f l=
 let rec filter0=(function
  (graet1,graet2,da_ober)->match da_ober with
   []->(List.rev(graet1),List.rev(graet2))
   |x::peurrest->match f(x) with
		None->filter0(graet1,x::graet2,peurrest)
		|Some(y)->filter0(y::graet1,graet2,peurrest)
 ) in
 filter0([],[],l);;


let rec find_and_stop f l=
 let rec find_and_stop0=(function
  da_ober->match da_ober with
   []->None
   |a::peurrest->match f(a) with
		None->find_and_stop0(peurrest)
		|Some(x)->Some(x)
 ) in
 find_and_stop0(l);;


let rec filter_and_develop f l=
 let rec filter_and_develop0=(function
  (graet,da_ober)->match da_ober with
   []->List.rev(graet)
   |a::peurrest->match a with
		None->filter_and_develop0(graet,peurrest)
		|Some(x)->filter_and_develop0(f(x)::graet,peurrest)
 ) in
 filter_and_develop0([],l);;
 
 let add_element x l=match x with
  None->l
  |Some(a)->a::l;;
 
 let add_if_nonempty x y=if x=[] then y else x::y;;
 
 let original_filter_and_separate opt_f l=
   let rec tempf1=(function
    (graet1,graet2,da_ober)->
      if da_ober=[] 
      then List.rev(add_if_nonempty (List.rev (graet2)) graet1) else
      let x0=List.hd(da_ober) and peurrest=List.tl(da_ober) in
      match opt_f x0 with
      None->tempf1(add_if_nonempty (List.rev (graet2)) graet1,[],peurrest)
      |Some(y0)->tempf1(graet1,y0::graet2,peurrest)
    ) in
    tempf1([],[],l);;
 
  
 let catch_test f x=if f x then Some x else None;;  
 let catch_exception f x=try(Some(f x)) with _->None;;
 
 let filter_and_except f l=original_filter_and_separate 
   (catch_exception f) l;;
   
 let filter_and_test f l=original_filter_and_separate 
   (catch_test f) l;;
  
let add_perhaps opt l=match opt with
None->l
|Some(a)->a::l;;      
      
 end;;


module Scennig=struct  
 let doyle f a b=
 let accu=ref([]) in
 let rec doyle0=(function
 j->if j<a
    then (!accu)
	else let _=(accu:=f(j)::(!accu)) in doyle0(j-1)
 ) in
 doyle0 b;;
 
 let slow_doyle f a b=
 let accu=ref([]) in
 let rec slow_doyle0=(function
 j->if j>b
    then List.rev(!accu)
	else let _=(accu:=f(j)::(!accu)) in slow_doyle0(j+1)
 ) in
 slow_doyle0 a;;
 
 
 let doyle_for_delta f n u0=
 let accu=ref([u0]) and traveler=ref(u0) in
 let rec doyle0=(function
 da_ober->if da_ober<1
          then List.rev(!accu)
	      else let _=(traveler:=f(!traveler);accu:=(!traveler)::(!accu)) in 
	           doyle0(da_ober-1)
 ) in
 doyle0 n;;
  
 
let ennig a b=doyle (function x->x) a b;; 
 
let index_everything l=
  let rec tempf=
   (function (j,graet,da_ober)->
     match da_ober with
      []->graet
     |a::b->tempf(j-1,(j,a)::graet,b)
    )    in
    tempf(List.length(l),[],List.rev(l));;

let for_all f a b=
 let rec for_all0=(function
 j->if j>b
    then true
	else if f(j)
	     then for_all0(j+1)
		 else false
 ) in
 for_all0 a;;

let rec exists f a b=
if (a>b) 
then false
else if f(a)
	 then true
	 else exists f (a+1) b;;	 
 
let rec find_it f a b=
if (a>b) 
then None
else if f(a)
	 then Some(a)
	 else find_it f (a+1) b;;	  

let rec find_and_stop f a b=
 let rec find_and_stop0=(function
  j->if (j>b)
     then None
	 else match f(j) with
		None->find_and_stop0(j+1)
		|Some(x)->Some(x)
 ) in
 find_and_stop0 a;;

let constant_list n x=doyle (function j->x) 1 n;;

let describe_fibers_as_intervals f a b=
  if (a>b) then [] else
  let rec tempf=(function
    (graet,x1,x2,y0)->
       if (x2>=b) then List.rev((x1,x2,y0)::graet) else
       let x3=x2+1 in
       let y3=f(x3) in
       if (y3=y0)
       then tempf(graet,x1,x3,y0)
       else tempf((x1,x2,y0)::graet,x3,x3,y3)
  
  ) in
  tempf([],a,a,f(a));;
 end;;


module Scmemoized=struct 
type ('a,'b) map=('a->'b);;

let make_from (f:'a->'b) (a_hashtbl_for_f:('a,'b) Hashtbl.t)=
  let memoized_f=(fun x->
     if Hashtbl.mem(a_hashtbl_for_f)(x)
     then Hashtbl.find(a_hashtbl_for_f)(x)
     else let y=f(x) in
          let ()=(Hashtbl.add(a_hashtbl_for_f) x y) in
          y
  ) in
  (memoized_f:>('a,'b) map);;

let make (f:'a->'b)=
  let a_hashtbl_for_f=Hashtbl.create(100) in
  make_from f a_hashtbl_for_f;;
  
let recursive_from=((fun (big_f:('a->'b)->'a->'b) (a_hashtbl_for_f:('a,'b) Hashtbl.t)->
  let rec memoized_f=(fun x->
     if Hashtbl.mem(a_hashtbl_for_f)(x)
     then Hashtbl.find(a_hashtbl_for_f)(x)
     else let mf=(memoized_f:>('a->'b)) in
          let y=big_f(mf)(x) in
          let ()=(Hashtbl.add(a_hashtbl_for_f) x y) in
          y
  ) in
  memoized_f):>(('a->'b)-> 'a -> 'b) -> (('a,'b) Hashtbl.t) -> ('a, 'b) map);;

let recursive (big_f:('a->'b)->'a->'b)=
  let a_hashtbl_for_f=Hashtbl.create(100) in
  recursive_from big_f a_hashtbl_for_f;;

let small f initial_value=
  recursive(fun old_f k->if k<1 then initial_value else f(old_f(k-1)));;
  
let reversible (f:'a->'b)=
  let a_hashtbl_for_f=Hashtbl.create(100) 
  and a_hashtbl_for_the_inverse_of_f=Hashtbl.create(100)
  and a_hashtbl_for_the_second_inverse_of_f=Hashtbl.create(100)
  and a_hashtbl_for_the_projector=Hashtbl.create(50) 
  and irreducibles=ref([]) 
  and minimal_reductions=ref([]) in
  let compute_f=(fun x accu->
     let y=f(x) in
     let ()=(Hashtbl.add(a_hashtbl_for_f) x y;accu:=[y]) in
      if Hashtbl.mem(a_hashtbl_for_the_second_inverse_of_f)(y)
     then let old_x=Hashtbl.find(a_hashtbl_for_the_inverse_of_f)(y) in
          Hashtbl.add(a_hashtbl_for_the_projector)(x)(old_x)
     else     
     if Hashtbl.mem(a_hashtbl_for_the_inverse_of_f)(y)
     then let old_x=Hashtbl.find(a_hashtbl_for_the_inverse_of_f)(y) in
          (Hashtbl.add(a_hashtbl_for_the_projector)(x)(old_x);
          Hashtbl.add(a_hashtbl_for_the_second_inverse_of_f)(y)(x);
          minimal_reductions:=(x,old_x)::(!minimal_reductions))
     else (Hashtbl.add(a_hashtbl_for_the_inverse_of_f)(y)(x);
            irreducibles:=x::(!irreducibles))
     
  ) in
  let memoized_f=(fun x->
     if Hashtbl.mem(a_hashtbl_for_f)(x)
     then Hashtbl.find(a_hashtbl_for_f)(x)
     else let accu=ref([]) in
          let _=compute_f(x)(accu) in
          List.hd(!accu)
  ) 
  and memoized_inverse_of_f=Hashtbl.find(a_hashtbl_for_the_inverse_of_f) in
  let memoized_projector=(fun x->
    let ()=compute_f(x)(ref[]) in
    if Hashtbl.mem(a_hashtbl_for_the_projector)(x)
    then Hashtbl.find(a_hashtbl_for_the_projector)(x)
    else x
    ) in
  (memoized_f,memoized_inverse_of_f,memoized_projector,irreducibles,minimal_reductions);;
 end;;


module Scutf_eight=struct (*

#use"utf_eight.ml";;

*)



let array_to_predict_char_length = [| (* uchar byte length according to first UTF-8 byte. *)
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2;
  2; 2; 2; 2; 2; 2; 2; 2; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3;
  4; 4; 4; 4; 4; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];;
  
exception Malformed_exn of int*(int list);;   
exception Bad_starter of int*int;;

let pusher_for_decoding (stream,j) =
  let b0=List.nth stream j in
  let l=Array.get array_to_predict_char_length b0 in
  if l=1 
  then (b0,j+l)
  else
  let b1=List.nth stream (j+1) in
  if l=2
  then (
        if b1 lsr 6 != 0b10 
        then raise(Malformed_exn(1,[b0;b1])) 
        else (((b0 land 0x1F) lsl 6) lor (b1 land 0x3F),j+l)
       )
  else
  let b2=List.nth stream (j+2) in    
  if b2 lsr 6 != 0b10 then raise(Malformed_exn(2,[b0;b1;b2]))  else 
  if l=3
  then (
        let c = ((b0 land 0x0F) lsl 12) lor
                ((b1 land 0x3F) lsl 6) lor
                 (b2 land 0x3F) in
        begin match b0 with
      	| 0xE0 -> if b1 < 0xA0 || 0xBF < b1 then raise(Malformed_exn(3,[b0;b1;b2])) else (c,j+l)
      	| 0xED -> if b1 < 0x80 || 0x9F < b1 then raise(Malformed_exn(4,[b0;b1;b2])) else (c,j+l)
      	| _ -> if b1 lsr 6 != 0b10 then raise(Malformed_exn(5,[b0;b1;b2])) else (c,j+l)
      	end
       )
  else
  let b3=List.nth stream (j+3) in  
  if b3 lsr 6 != 0b10 then raise(Malformed_exn(6,[b0;b1;b2;b3]))  else  
  if l=4
  then 
       (
        let c = ((b0 land 0x07) lsl 18) lor
                ((b1 land 0x3F) lsl 12) lor
          		((b2 land 0x3F) lsl 6) lor
                 (b3 land 0x3F)        in
        begin match b0 with
      	| 0xF0 -> if b1 < 0x90 || 0xBF < b1 then raise(Malformed_exn(6,[b0;b1;b2;b3])) else (c,j+l)
      	| 0xF4 -> if b1 < 0x80 || 0x8F < b1 then raise(Malformed_exn(7,[b0;b1;b2;b3])) else (c,j+l)
      	| _ -> if b1 lsr 6 != 0b10 then raise(Malformed_exn(8,[b0;b1;b2;b3])) else (c,j+l)
      	end
       )
  else raise(Bad_starter(b0,l));;

let decode l=
   let n=List.length l in
   if n=0 then [] else
   let rec tempf=(fun (accu,j)->
     if j>=n
     then List.rev(accu)
     else let (c,new_j)=pusher_for_decoding (l,j) in
          tempf(c::accu,new_j)
   ) in
   tempf([],0);;

let encode u =
    if u <= 0x007F 
    then [u] 
    else 
    if u <= 0x07FF 
    then[(0xC0 lor (u lsr 6)); (0x80 lor (u land 0x3F))]
    else 
    if u <= 0xFFFF 
    then  [ (0xE0 lor (u lsr 12));
            (0x80 lor ((u lsr 6) land 0x3F));
            (0x80 lor (u land 0x3F))]
    else
          [ (0xF0 lor (u lsr 18));
            (0x80 lor ((u lsr 12) land 0x3F));
            (0x80 lor ((u lsr 6) land 0x3F));
            (0x80 lor (u land 0x3F)) ];;

let encode_as_string u=
  let temp1=encode u in
  let d=List.length(temp1) in
  let b=Bytes.create d in
  let _=(for i=0 to (d-1) do Bytes.set b i (Char.chr (List.nth temp1 i)) done) in
  Bytes.to_string b;;
  
            
            
let unicode_point s=
   let n=String.length(s) in
   let temp1=Scennig.doyle (fun j->Char.code(String.get s j)) 0 (n-1) in
   Printf.sprintf "%X" (fst(pusher_for_decoding (temp1,0)));;             
            
let decompose s=
   let n=String.length(s) in
   let temp1=Scennig.doyle (fun j->Char.code(String.get s j)) 0 (n-1) in
   let temp2=decode temp1 in
   Scimage.image encode_as_string temp2;;           
            
            
            
            











 end;;


module Scunix_command=struct (*

Wrapper on the Sys dot command function.

#use"unix_command.ml";;

*)


exception Command_failed of string;;

let accu=ref([]:string list);;
let remember_commands_mode=ref(false);;


let hardcore_uc s=
   let i=Sys.command s in
   if i<>0
   then raise(Command_failed(s))
   else let _=(if (!remember_commands_mode) then accu:=s::(!accu)) in 
        i;;

let mild_uc s=
   let i=Sys.command s in
   let _=(
   if i<>0
   then (print_string ("Failed during "^s);flush stdout)
   else (if (!remember_commands_mode) then accu:=s::(!accu))
   ) in
   i;;

let hardcore_mode=ref(false);;

let uc s=
   if (!hardcore_mode)
   then hardcore_uc s
   else mild_uc s;;


 end;;


module Sccapitalize_directory_names=struct (*

#use"capitalize_directory_names.ml";;


*)

let cdn s=
  let n=String.length(s) in
  let temp1=List.filter(
     fun j->(String.get s j)='/'
  )(Scennig.ennig 0 (n-1)) in
  if temp1=[] then s else
  let temp4=List.rev(List.tl(List.rev temp1)) in
  let temp2=0::(Scimage.image (fun j->j+1) temp4) in
  let tempf=(fun j->
    let t=String.make 1 (String.get s j) in
    if List.mem j temp2
    then String.capitalize_ascii t
    else t
  ) in
  let temp3=Scennig.doyle tempf 0 (n-1) in
  String.concat "" temp3;;

(*

cdn "do/You/feel/like/sixty/feet";;
cdn "/do/You/feel/like/sixty/feet";;
cdn "do/You/feel/like/sixty/feet/";;
cdn "/do/You/feel/like/sixty/feet/";;

cdn "peggy";;
cdn "peggy/lee";;

*) end;;


module Sctools_for_absolute_path=struct (*

Standardize filename path. Non-directories never 
end with /, directories always do.


*)

exception Bad_filename;;

let number_of_double_points s=
  let n=String.length(s) in
  let rec tempf=(fun j->
     let k=(3*j) in
     if (n<k+2) then j else
     if (String.get s k='.')&&(String.get s (k+1)='.')
     then if n=(k+2) then j+1 else
          if (String.get s (k+2)='/') 
          then tempf(j+1)
          else raise(Bad_filename)
     else j
  ) in
  tempf(0);;
  
let iterated_string_father0 j0 s=
   let rec tempf=(fun (j,k)->
     if j<1 then (String.sub s 0 k) else
     let i=String.rindex_from(s)(k-1)('/') in
     tempf(j-1,i)
     ) in
    tempf (j0,String.length s);;
 
exception Too_much_double_points;;  
 
 let iterated_string_father j0 s=try iterated_string_father0 j0 s with
   forzh_petra->raise(Too_much_double_points);;

let delete_left_blanks s=
  let n=String.length(s) in
  let rec tempf=(fun j->
    if j>=n then failwith("You have named no file") else
    if String.get(s)(j)=' '
    then tempf(j+1)
    else j
  ) in
  let j0=tempf 0 in
  String.sub s j0 (n-j0);;

let absolute_path_of_string0 s0=
  let s1=delete_left_blanks(s0) in
  let dp1=number_of_double_points(s1) in
  if (dp1>0) 
  then  let smaller_pwd=iterated_string_father dp1 (Sys.getcwd()) in
        let j1=(3*dp1)-1 in 
         smaller_pwd^(String.sub s1 j1 ((String.length s1)-j1) )    
  else
  if s1="/" then "/" else
  match String.get(s1)(0) with
  '/'->s1
  |'~'->(Sys.getenv "HOME")^(String.sub s1 1 (String.length(s1)-1))
  |'.'->if s1="." 
        then (Sys.getcwd()) 
        else (Sys.getcwd())^"/"^(String.sub s1 2 (String.length(s1)-2))
  |arall->(Sys.getcwd())^"/"^s1;;
  
 let remove_trailing_slash s=
    let n=String.length(s) in
    if ((String.get s (n-1))='/')
    then String.sub s 0 (n-1)
    else s;;
  
 exception Inexistent_file of string;; 
  
 let absolute_path_of_string1 s=
  let s0=absolute_path_of_string0(s) in
  if Sys.file_exists(s0)
  then if s0="/" then s0 else
       let s1=remove_trailing_slash s0 in
       if Sys.is_directory s1
       then s1^"/"
       else s1
  else raise(Inexistent_file(s));;
  
 let of_string s=
   Sccapitalize_directory_names.cdn(absolute_path_of_string1 s);; end;;


module Scabsolute_path=struct (*

#use"absolute_path.ml";;

*)

type t=AP of string;;

let of_string s=AP(Sctools_for_absolute_path.of_string s);;
let to_string (AP s)=s;;

let ocaml_name ap=
 let s=to_string ap in
"Absolute"^"_path"^"."^"of_string(\""^s^"\"";;

let test_equal_paths s1 s2=
((of_string s1)=(of_string s2));;

exception Error_during_file_creation;;
 
let create_file w=
    let cr=(fun w->
      let ld=Unix.openfile w [Unix.O_RDWR;Unix.O_CREAT;Unix.O_EXCL] 0o666 in
       Unix.close ld
    ) in
    if Sys.file_exists w then of_string w else
    if (not(String.contains w '/'))
    then (cr w;of_string w)
    else 
    let i=String.rindex w '/' in
    let filename=String.sub w (i+1) ((String.length w)-(i+1)) in
    let g1="jnoxgghg_"^filename in
    let _=Scunix_command.uc ("rm -f "^g1) in
    let _=cr g1 in
    let _=Scunix_command.uc ("mv "^g1^" "^w) in
    let _=Scunix_command.uc ("rm -f "^g1) in
    of_string w;;
    
let print_out (fmt:Format.formatter) ap=
   Format.fprintf fmt "@[%s@]" (to_string ap);;

 end;;


module Scdictionary_order=struct (*

#use"dictionary_order.ml";;

*)


let dictionary_order=
 ((fun s1 s2->
   let m1=String.length s1
   and m2=String.length s2
   in
   let m=min(m1)(m2) in
   match Scoption.find_it (fun j->(String.get s1 j)<>(String.get s2 j)) (Scennig.ennig 0 (m-1)) with
   None->Sctotal_ordering.standard m1 m2
   |Some(j)->Sctotal_ordering.standard (String.get s1 j) (String.get s2 j) 
 ) : string Sctotal_ordering.t);;
 
  end;;


module Sckeyval_ordering=struct (*

#use"keyval_ordering.ml";;

*)


let ko=((fun x y->Sctotal_ordering.product 
  Scdictionary_order.dictionary_order Sctotal_ordering.standard x y): 
  (string * 'a) Sctotal_ordering.t);; end;;


module Scphp_keyword=struct (*

#use"Php_analizer/php_keyword.ml";;

*)



(* from https://github.com/php/php-langspec/blob/master/spec/09-lexical-structure.md#keywords 

   added the "exit" keyword
*)

type t=
     T_ABSTRACT
    |T_AS
    |T_BREAK
    |T_CALLABLE
    |T_CASE
    |T_CATCH
    |T_CLASS
    |T_CONST
    |T_CONTINUE
    |T_DECLARE
    |T_DEFAULT
    |T_DO
    |T_ECHO
    |T_ELSE
    |T_ELSEIF
    |T_ENDDECLARE
    |T_ENDFOR
    |T_ENDFOREACH
    |T_ENDIF
    |T_ENDSWITCH
    |T_ENDWHILE
    |T_EXIT
    |T_EXTENDS
    |T_FINAL
    |T_FINALLY
    |T_FOR
    |T_FOREACH
    |T_FUNCTION
    |T_GLOBAL
    |T_GOTO
    |T_IF
    |T_IMPLEMENTS
    |T_INCLUDE
    |T_INCLUDE_ONCE
    |T_INSTEADOF
    |T_INTERFACE
    |T_LIST
    |T_NAMESPACE
    |T_PRINT
    |T_PRIVATE
    |T_PROTECTED
    |T_PUBLIC
    |T_REQUIRE
    |T_REQUIRE_ONCE
    |T_RETURN
    |T_STATIC
    |T_SWITCH
    |T_THROW
    |T_TRAIT
    |T_TRY
    |T_USE
    |T_VAR
    |T_WHILE
    |T_YIELD;;

let to_string=function
     T_ABSTRACT->"abstract"
    |T_AS->"as"
    |T_BREAK->"break"
    |T_CALLABLE->"callable"
    |T_CASE->"case"
    |T_CATCH->"catch"
    |T_CLASS->"class"
    |T_CONST->"const"
    |T_CONTINUE->"continue"
    |T_DECLARE->"declare"
    |T_DEFAULT->"default"
    |T_DO->"do"
    |T_ECHO->"echo"
    |T_ELSE->"else"
    |T_ELSEIF->"elseif"
    |T_ENDDECLARE->"enddeclare"
    |T_ENDFOR->"endfor"
    |T_ENDFOREACH->"endforeach"
    |T_ENDIF->"endif"
    |T_ENDSWITCH->"endswitch"
    |T_ENDWHILE->"endwhile"
    |T_EXIT->"exit"
    |T_EXTENDS->"extends"
    |T_FINAL->"final"
    |T_FINALLY->"finally"
    |T_FOR->"for"
    |T_FOREACH->"foreach"
    |T_FUNCTION->"function"
    |T_GLOBAL->"global"
    |T_GOTO->"goto"
    |T_IF->"if"
    |T_IMPLEMENTS->"implements"
    |T_INCLUDE->"include"
    |T_INCLUDE_ONCE->"include_once"
    |T_INSTEADOF->"insteadof"
    |T_INTERFACE->"interface"
    |T_LIST->"list"
    |T_NAMESPACE->"namespace"
    |T_PRINT->"print"
    |T_PRIVATE->"private"
    |T_PROTECTED->"protected"
    |T_PUBLIC->"public"
    |T_REQUIRE->"require"
    |T_REQUIRE_ONCE->"require_once"
    |T_RETURN->"return"
    |T_STATIC->"static"
    |T_SWITCH->"switch"
    |T_THROW->"throw"
    |T_TRAIT->"trait"
    |T_TRY->"try"
    |T_USE->"use"
    |T_VAR->"var"
    |T_WHILE->"while"
    |T_YIELD->"yield";;




let all_pairs =
  Scimage.image (fun kwd->(to_string kwd,kwd))
  [
     T_ABSTRACT;
     T_AS;
     T_BREAK;
     T_CALLABLE;
     T_CASE;
     T_CATCH;
     T_CLASS;
     T_CONST;
     T_CONTINUE;
     T_DECLARE;
     T_DEFAULT;
     T_DO;
     T_ECHO;
     T_ELSE;
     T_ELSEIF;
     T_ENDDECLARE;
     T_ENDFOR;
     T_ENDFOREACH;
     T_ENDIF;
     T_ENDSWITCH;
     T_ENDWHILE;
     T_EXIT;
     T_EXTENDS;
     T_FINAL;
     T_FINALLY;
     T_FOR;
     T_FOREACH;
     T_FUNCTION;
     T_GLOBAL;
     T_GOTO;
     T_IF;
     T_IMPLEMENTS;
     T_INCLUDE;
     T_INCLUDE_ONCE;
     T_INSTEADOF;
     T_INTERFACE;
     T_LIST;
     T_NAMESPACE;
     T_PRINT;
     T_PRIVATE;
     T_PROTECTED;
     T_PUBLIC;
     T_REQUIRE;
     T_REQUIRE_ONCE;
     T_RETURN;
     T_STATIC;
     T_SWITCH;
     T_THROW;
     T_TRAIT;
     T_TRY;
     T_USE;
     T_VAR;
     T_WHILE;
     T_YIELD

];;
 
let all_keywords=Scimage.image snd all_pairs;; 

exception Unknown_keyword_string of string;; 

let of_prudent_string s=
  Scoption.find_it (fun oprtr->to_string(oprtr)=s) all_keywords;; 
 
let of_string s=
  match of_prudent_string s with
   None->raise(Unknown_keyword_string(s))
  |Some(oprtr)->oprtr;;
  
let all_strings=Scimage.image to_string all_keywords;;        
 
  
  
   
 end;;


module Scassociativity=struct (*


#use"associativity.ml";;

*)

type t=
    Left_associative
   |Right_associative
   |Non_associative
   |Unspecified_associativity;;
   
   

 end;;


module Scphp_operator=struct (*

#use"Php_analizer/php_operator.ml";;

*)


let left=Scassociativity.Left_associative;;
let right=Scassociativity.Right_associative;;
let nonassoc=Scassociativity.Non_associative;;

(* from http://php.net/manual/en/language.operators.precedence.php *)
type t=
     T_CLONE
    |T_NEW
    |T_LBRACKET
    |T_RBRACKET
    |T_STAR_STAR
    |T_PLUS_PLUS
    |T_MINUS_MINUS
    |T_TILDA
    |T_COERCE_TO_INT
    |T_COERCE_TO_FLOAT
    |T_COERCE_TO_STRING
    |T_COERCE_TO_ARRAY
    |T_COERCE_TO_OBJECT
    |T_COERCE_TO_BOOL
    |T_AT
    |T_INSTANCEOF
    |T_EXCLAMATION
    |T_STAR
    |T_DIVIDE
    |T_PERCENTAGE
    |T_PLUS
    |T_MINUS
    |T_DOT
    |T_LESS_LESS
    |T_MORE_MORE
    |T_LESS
    |T_LESS_EQUALS
    |T_MORE
    |T_MORE_EQUALS
    |T_EQUALS_EQUALS
    |T_EXCLAMATION_EQUALS
    |T_EQUALS_EQUALS_EQUALS
    |T_EXCLAMATION_EQUALS_EQUALS
    |T_LESS_MORE
    |T_AMPERSAND
    |T_CIRCUMFLEX
    |T_VLINE
    |T_AMPERSAND_AMPERSAND
    |T_VLINE_VLINE
    |T_QUESTION
    |T_COLON
    |T_EQUALS
    |T_PLUS_EQUALS
    |T_MINUS_EQUALS
    |T_STAR_EQUALS
    |T_STAR_STAR_EQUALS
    |T_DIVIDE_EQUALS
    |T_DOT_EQUALS
    |T_PERCENTAGE_EQUALS
    |T_AMPERSAND_EQUALS
    |T_VLINE_EQUALS
    |T_CIRCUMFLEX_EQUALS
    |T_LESS_LESS_EQUALS
    |T_MORE_MORE_EQUALS
    |T_EQUALS_MORE
    |T_AND
    |T_XOR
    |T_OR;;

let to_string=function
     T_CLONE->"clone"
    |T_NEW->"new"
    |T_LBRACKET->"["
    |T_RBRACKET->"]"
    |T_STAR_STAR->"**"
    |T_PLUS_PLUS->"++"
    |T_MINUS_MINUS->"--"
    |T_TILDA->"~"
    |T_COERCE_TO_INT->"(int)"
    |T_COERCE_TO_FLOAT->"(float)"
    |T_COERCE_TO_STRING->"(string)"
    |T_COERCE_TO_ARRAY->"(array)"
    |T_COERCE_TO_OBJECT->"(object)"
    |T_COERCE_TO_BOOL->"(bool)"
    |T_AT->"@"
    |T_INSTANCEOF->"instanceof"
    |T_EXCLAMATION->"!"
    |T_STAR->"*"
    |T_DIVIDE->"/"
    |T_PERCENTAGE->"%"
    |T_PLUS->"+"
    |T_MINUS->"-"
    |T_DOT->"."
    |T_LESS_LESS->"<<"
    |T_MORE_MORE->">>"
    |T_LESS->"<"
    |T_LESS_EQUALS->"<="
    |T_MORE->">"
    |T_MORE_EQUALS->">="
    |T_EQUALS_EQUALS->"=="
    |T_EXCLAMATION_EQUALS->"!="
    |T_EQUALS_EQUALS_EQUALS->"==="
    |T_EXCLAMATION_EQUALS_EQUALS->"!=="
    |T_LESS_MORE->"<>"
    |T_AMPERSAND->"&"
    |T_CIRCUMFLEX->"^"
    |T_VLINE->"|"
    |T_AMPERSAND_AMPERSAND->"&&"
    |T_VLINE_VLINE->"||"
    |T_QUESTION->"?"
    |T_COLON->":"
    |T_EQUALS->"="
    |T_PLUS_EQUALS->"+="
    |T_MINUS_EQUALS->"-="
    |T_STAR_EQUALS->"*="
    |T_STAR_STAR_EQUALS->"**="
    |T_DIVIDE_EQUALS->"/="
    |T_DOT_EQUALS->".="
    |T_PERCENTAGE_EQUALS->"%="
    |T_AMPERSAND_EQUALS->"&="
    |T_VLINE_EQUALS->"|="
    |T_CIRCUMFLEX_EQUALS->"^="
    |T_LESS_LESS_EQUALS->"<<="
    |T_MORE_MORE_EQUALS->">>="
    |T_EQUALS_MORE->"=>"
    |T_AND->"and"
    |T_XOR->"xor"
    |T_OR->"or";;



let pre_list_for_precedences=
[
    (nonassoc,[T_CLONE;T_NEW]);
    (left,[T_LBRACKET;T_RBRACKET]);
    (right,[T_STAR_STAR]);
    (right,[T_PLUS_PLUS;T_MINUS_MINUS;T_TILDA;T_COERCE_TO_INT;T_COERCE_TO_FLOAT;T_COERCE_TO_STRING;T_COERCE_TO_ARRAY;T_COERCE_TO_OBJECT;T_COERCE_TO_BOOL;T_AT]);
    (nonassoc,[T_INSTANCEOF]);
    (right,[T_EXCLAMATION]);
    (left,[T_STAR;T_DIVIDE;T_PERCENTAGE]);
    (left,[T_PLUS;T_MINUS;T_DOT]);
    (left,[T_LESS_LESS;T_MORE_MORE]);
    (nonassoc,[T_LESS;T_LESS_EQUALS;T_MORE;T_MORE_EQUALS]);
    (nonassoc,[T_EQUALS_EQUALS;T_EXCLAMATION_EQUALS;T_EQUALS_EQUALS_EQUALS;T_EXCLAMATION_EQUALS_EQUALS;T_LESS_MORE]);
    (left,[T_AMPERSAND]);
    (left,[T_CIRCUMFLEX]);
    (left,[T_VLINE]);
    (left,[T_AMPERSAND_AMPERSAND]);
    (left,[T_VLINE_VLINE]);
    (left,[T_QUESTION;T_COLON]);
    (left,[T_EQUALS;T_PLUS_EQUALS;T_MINUS_EQUALS;T_STAR_EQUALS;T_STAR_STAR_EQUALS;T_DIVIDE_EQUALS;T_DOT_EQUALS;T_PERCENTAGE_EQUALS;T_AMPERSAND_EQUALS;T_VLINE_EQUALS;T_CIRCUMFLEX_EQUALS;T_LESS_LESS_EQUALS;T_MORE_MORE_EQUALS;T_EQUALS_MORE]);
    (left,[T_AND]);
    (left,[T_XOR]);
    (left,[T_OR])
];;

let list_for_precedences=
  let temp1=Scennig.index_everything pre_list_for_precedences in
  Scimage.image (fun (i,(x,y))->(y,(i,x))) temp1;;


let precedence oprtr=
  let (_,(j,_))=Scoption.find_really 
  (fun (l,_)->List.mem oprtr l) list_for_precedences in
  j;;

  let all_pairs=
    let temp1=List.flatten(Scimage.image snd pre_list_for_precedences) in
    let temp2=Scimage.image (fun x->(to_string x,x)) temp1 in
    Scordered.forget_order
      (Scordered.diforchan Sckeyval_ordering.ko temp2);;  

let all_operators=Scimage.image snd all_pairs;;  
 
exception Unknown_operator_string of string;; 
 
let of_prudent_string s=
   Scoption.find_it (fun oprtr->to_string(oprtr)=s) all_operators ;;
 
let of_string s=
  match of_prudent_string s with
   None->raise(Unknown_operator_string(s))
  |Some(oprtr)->oprtr;;
  
let level s=
  let p0=precedence(of_string s) in
  List.filter (fun op->precedence(op)=p0) all_operators;;  
  
let all_strings=Scimage.image fst all_pairs;;   
 
  
  
   
 end;;


module Scphp_punctuator=struct (*

#use"Php_analizer/php_punctuator.ml";;

*)



(* 

from https://github.com/php/php-langspec/blob/master/spec/09-lexical-structure.md#operators-and-punctuators 
I do not consider =& as a single token, but as a composite = followed by &.

*)

type t=
     T_LPARENTHESIS
    |T_RPARENTHESIS
    |T_LBRACE
    |T_RBRACE
    |T_ARROW
    |T_SEMICOLON
    |T_COMMA
    |T_COLON_COLON;;

let to_string=function
     T_LPARENTHESIS->"("
    |T_RPARENTHESIS->")"
    |T_LBRACE->"{"
    |T_RBRACE->"}"
    |T_ARROW->"->"
    |T_SEMICOLON->";"
    |T_COMMA->","
    |T_COLON_COLON->"::";;





  let all_pairs =
    Scordered.forget_order( Scordered.diforchan Sckeyval_ordering.ko 
    (Scimage.image (fun x->(to_string x,x))
[
     T_LPARENTHESIS;
     T_RPARENTHESIS;
     T_LBRACE;
     T_RBRACE;
     T_ARROW;
     T_SEMICOLON;
     T_COMMA;
     T_COLON_COLON

]));;
 
let all_punctuators =Scimage.image snd all_pairs;; 

exception Unknown_punctuator_string of string;; 

let of_prudent_string s=
  Scoption.find_it (fun oprtr->to_string(oprtr)=s) all_punctuators;; 
 
let of_string s=
  match of_prudent_string s with
   None->raise(Unknown_punctuator_string(s))
  |Some(oprtr)->oprtr;;
  
let all_strings=Scimage.image fst all_pairs;;      
 
  
  
   
 end;;


module Scphp_constant_token=struct (*

#use"Php_analizer/php_constant_token.ml";;

*)

type t=
     Kwd of Scphp_keyword.t
    |Punct of Scphp_punctuator.t
    |Op of Scphp_operator.t;;

let to_string=function
      (Kwd s)->Scphp_keyword.to_string s
     |(Punct s)->Scphp_punctuator.to_string s
     |(Op s)->Scphp_operator.to_string s;;
  
let all_pairs=
       let kwds=Scimage.image (fun (s,kwd)->(s,Kwd kwd)) Scphp_keyword.all_pairs 
       and puncts=Scimage.image (fun (s,punct)->(s,Punct punct)) Scphp_punctuator.all_pairs
       and ops=Scimage.image (fun (s,op)->(s,Op op)) Scphp_operator.all_pairs in
  Scordered.forget_order(Scordered.diforchan Sckeyval_ordering.ko  
     (kwds@puncts@ops) );;

let all_string_constants=Scimage.image fst all_pairs;;

let all=Scimage.image snd all_pairs;;

exception Unknown of string;;

let of_string s=
   try List.assoc s all_pairs with
   _->raise(Unknown(s));;

let putative_of_string s=try (Some(of_string s)) with _->None;;

 end;;


module Scphp_projected_token=struct (*

#use"Php_analizer/php_projected_token.ml";;

*)
type t=
     Constant of Scphp_constant_token.t
    |Variable 
    |Ident 
    |Comment 
    |Single_quoted 
    |Double_quoted 
    |Heredoc 
    |Nowdoc 
    |Namespacer 
    |External_echo 
    |Int 
    |Float 
    |Char 
    |End_of_text;;

let is_a_comment=function
   Comment->true
  |_->false;;

let fixture_of_nonconstants=
    [
       Variable; 
       Ident;
       Comment;
       Single_quoted;
       Double_quoted;
       Heredoc;
       Nowdoc;
       Namespacer;
       External_echo;
       Int;
       Float;
       Char;
    ];;


  
let precedence=function 
  Constant ctok->(match ctok with
                    (Scphp_constant_token.Op op)->Some(Scphp_operator.precedence(op))
                   |_->None
                 )
  |_->None;;


let constant_part=function 
 Constant ctok->Some(ctok)
|_->None;;



let op s=Constant(Scphp_constant_token.Op(Scphp_operator.of_string s));;
let punct s=Constant(Scphp_constant_token.Punct(Scphp_punctuator.of_string s));;
let kwd s=Constant(Scphp_constant_token.Kwd (Scphp_keyword.of_string s));;

let test ctok tok=(tok=Constant(ctok));;

let to_string=function
 Constant(ctok)->Scphp_constant_token.to_string ctok
|Variable->"variable"
|Ident->"id"
|Comment->"cmt"
|Single_quoted->"sqs"
|Double_quoted->"dqs"
|Heredoc->"heredoc"
|Nowdoc->"nowdoc"
|Namespacer->"nmspc"
|External_echo->"ext"
|Int->"integer"
|Float->"float"
|Char->"chr"
|End_of_text->"eot";;



let order=((
  fun x y->Scdictionary_order.dictionary_order 
     (to_string x) (to_string y)
): t Sctotal_ordering.t);;

let temp_pair=
  let temp1=(
    Scimage.image (fun ctok->Constant ctok) Scphp_constant_token.all
   )
   @
   fixture_of_nonconstants in
   let temp2=Scordered.forget_order(Scordered.diforchan order temp1) in
   (temp2,Scimage.image (fun ptok->(to_string ptok,ptok)) temp2);; 

let all_tokens=fst temp_pair;;
let all_pairs=snd temp_pair;;

  let string_tokens=
    [
      
      Variable;
      Ident;
      Comment;
      Single_quoted;
      Double_quoted;
      Heredoc;
      Nowdoc
      
    ];;     
      
 let harmless_tokens=string_tokens@[Int;Float];;  
 
 let precedence_neutral_tokens=harmless_tokens@
  (Scimage.image (fun x->Constant(Scphp_constant_token.Punct(x))) 
   [Scphp_punctuator.T_LPARENTHESIS;Scphp_punctuator.T_RPARENTHESIS]);;
    end;;


module Sccharset=struct (*

#use"charset.ml";;

*)



    
 let uppercase_letters= 
   ['A';'B';'C';'D';'E';'F';'G';'H';'I';'J';
    'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';
    'U';'V';'W';'X';'Y';'Z'];;
    
 let lowercase_identifier_elements=    
    ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';
     'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';
     'u';'v';'w';'x';'y';'z';'_';'+';'-';'*';
     '0';'1';'2';'3';'4';'5';'6';'7';'8';'9']@uppercase_letters;;
     
 let strictly_alphanumeric_characters =
  [
   'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';
   'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';
   'u';'v';'w';'x';'y';'z';
   'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';
   'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';
   'U';'V';'W';'X';'Y';'Z';
   '0';'1';'2';'3';'4';'5';'6';'7';'8';'9';
   '_';
   ];;   

let alphanumeric_characters =
  strictly_alphanumeric_characters @
  [
   '.';'\''
  ];;    

let unix_filename_admissible_characters =
  strictly_alphanumeric_characters @
  [
   '.';'/';'!';'~';
  ];;        
    
 let look_for_capitalized_identifiers s=
   let n=String.length s in
   let rec tempf=(fun (graet,j,j0)->
       if (j>=(n-1)) then List.rev(graet) else
       let c=String.get s (j+1) in
       if (j0>=0)
       then if List.mem c lowercase_identifier_elements
            then tempf(graet,j+1,j0)
            else let s1=String.lowercase_ascii(String.sub s j0 (j-j0+1)) in
                 if List.mem s1 graet
                 then tempf(graet,j+1,-1)
                 else tempf(s1::graet,j+1,-1)
       else
            if List.mem c uppercase_letters
            then tempf(graet,j+1,j+1)
            else tempf(graet,j+1,(-1))
   ) in
   tempf([],-1,-1);;
    
    
let is_a_lowercase c=let i=int_of_char c in (97<=i)&&(i<=122);;
let is_an_uppercase c=let i=int_of_char c in (65<=i)&&(i<=90);;
let character_is_alphanumeric c=List.mem c alphanumeric_characters;;
let character_is_strictly_alphanumeric c=List.mem c strictly_alphanumeric_characters;;
let is_an_uppercase_letter c=List.mem c uppercase_letters;;   
  
let string_is_alphanumeric s=
   List.for_all (fun j->
     character_is_alphanumeric(String.get s j)
   ) (Scennig.ennig 0 (String.length(s)-1));;  
  
let is_unix_filename_admissible s=
   List.for_all (fun j->
     List.mem (String.get s j) unix_filename_admissible_characters
   ) (Scennig.ennig 0 (String.length(s)-1));;  

exception Unix_rewrite_exn of string;;

let list_for_unix_usual=
   Scimage.image (fun c->let s=String.make 1 c in (s,s) ) 
  unix_filename_admissible_characters;;

let list_for_unix_rewriting=
   [
                 " ","_";
                 "-","_";
                 "'","_single_quote_";
                "\"","_double_quote_";
                 "&","_and_";
                 "(","_left_parenthesis_";
                 ")","_right_parenthesis_";
                 "?","_question_mark_";
                 "|","_vertical_bar_";
                 "<","_lower_than_";
                 ">","_greater_than_";
                 "=","_equals_";
                 ",","_comma_";
                 ";","_semicolon_";
          "\xc2\xa0","_";
          "\xcc\x80","_grave_";
          "\xcc\x81","_acute_";
          "\xcc\x83","_tilde_";
          "\xcc\xa7","_cedilla_";
      "\xe2\x80\x93","_";
      "\xe2\x80\x94","_";
      "\xe2\x80\x98","_lquote_";
      "\xe2\x80\x99","_rquote_";
      "\xe2\x80\xa6","_etc_";
    ];;
  
let unix_rewrite_char t=List.assoc t
  ((list_for_unix_usual)@(list_for_unix_rewriting));;
  
exception Unix_unknown of string;;  
  
let make_unix_compliant s=
   try String.concat "" (Scimage.image unix_rewrite_char (Scutf_eight.decompose s)) with
   _->raise(Unix_unknown(s));;  
  
let unix_unknowns_in_string s=
  List.filter(
    fun t->try(fun _->false)
    (unix_rewrite_char t) with
    _->true
  )(Scutf_eight.decompose s);;  
  
let starry_from l s i=
   let n=String.length s in
   let rec tempf=(fun (k0,k)->
    if k>n
    then String.sub s (k0-1) (k-k0)
    else 
    if List.mem(String.get s (k-1)) l 
    then tempf(k0,k+1)
    else String.sub s (k0-1) (k-k0)
   ) in
   tempf(i,i);;
     
  

  
   end;;


module Scprepared=struct let filter f=function
[]->[]
|l->
 let rec filter_easily0=(function
 (graet,da_ober)->match da_ober with
   []->List.rev(graet)
   |a::peurrest->if f(a) 
				 then filter_easily0(a::graet,peurrest)
                 else List.rev(graet)
 ) in
 filter_easily0([],l);; 

let partition f=function
[]->[]
|x::y->
 let rec partition_easily0=(function
 (graet,y,la,da_ober)->match da_ober with
   []->List.rev(List.rev(la)::graet)
   |b::peurrest->let z=f(b) in
                 if z=y
				 then partition_easily0(graet,y,b::la,peurrest)
                 else partition_easily0(List.rev(la)::graet,z,[b],peurrest)
 ) in
 partition_easily0([],f(x),[x],y);;  
 
let partition_in_two_parts f l=
   let rec tempf=(fun
    (graet,da_ober)->match da_ober with
      []->(List.rev graet,[])
      |a::peurrest->
         if f(a)
         then tempf(a::graet,peurrest)
         else (List.rev graet,da_ober)
   ) in
   tempf([],l);; 
 
 let write_interval (i,j)=match (j-i) with
  0->string_of_int(i)
  |1->(string_of_int i)^","^(string_of_int j)
  |arall->"["^((string_of_int i)^".."^(string_of_int j))^"]";;
 
 let doyle f a b=
  if (b<a) then [] else
   let rec tempf=(function
  (graet,y,i0,current_i)->
     if (current_i>b)
     then List.rev( (write_interval(i0,current_i-1),y) ::graet)
     else let z=f(current_i) in
          if z=y
          then tempf(graet,y,i0,current_i+1)
          else tempf( (write_interval(i0,current_i-1),y) ::graet, z,current_i,current_i+1)
   ) in
   tempf([],f(a),a,a+1);;
 
 let partition_according_to_fst=function
[]->[]
|(x1,y1)::lost->
 let rec partition_easily0=(function
 (graet,xi,ly,da_ober)->match da_ober with
   []->List.rev((xi,List.rev(ly))::graet)
   |(x,y)::peurrest->
                 if x=xi
				 then partition_easily0(graet,xi,y::ly,peurrest)
                 else partition_easily0((xi,List.rev(ly))::graet,x,[y],peurrest)
 ) in
 partition_easily0([],x1,[y1],lost);;  
 end;;


module Scsubstring=struct (*

Operation on substring finding, with indexes starting from 1.

#use"find_substring.ml";;


*)



let begins_with x y=
      let ly=String.length(y) in
      if String.length(x)<ly
      then false
      else (String.sub x 0 ly)=y;;
      
 let is_the_beginning_of y x=begins_with x y;;     
   
 let ends_with x y=
      let ly=String.length(y) in
      if String.length(x)<ly
      then false
      else (String.sub x ((String.length x)-ly) ly)=y;;  
   
 let is_the_ending_of y x=ends_with x y;;  
 
 
 
 let is_a_substring_located_at y x old_j =
    let j=old_j-1 in
    let ly=String.length(y) in
      if String.length(x)<j+ly
      then false
      else (String.sub x j ly)=y;;
 
  let is_a_substring_of x y=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) in
      Scennig.exists tester 0 (String.length(y)-lx);; 
      
  let leftmost_index_of_in x y=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) in
      try (Scoption.unpack(Scennig.find_it tester 0 (String.length(y)-lx))+1) with
      _->(-1);;
  
  let rightmost_index_of_in x y=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) 
      and temp1=List.rev(Scennig.ennig(0)(String.length(y)-lx)) in
      try ((Scoption.find_really tester temp1)+1) with
      _->(-1);;
  
   let leftmost_index_of_in_from x y i=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) in
      match Scennig.find_it tester (i-1) (String.length(y)-lx) with
         None->(-1)
        |Some(k)->k+1;;
  
 let occurrences_of_in x y=
   let lx=String.length x 
   and n=String.length y in
   let rec tempf=(fun (j,accu)->
      if j>n then List.rev(accu) else
      let k=leftmost_index_of_in_from x y j in
      if k<0 then List.rev(accu) else
      tempf(k+lx,k::accu)
   )  in
   tempf (1,[]);;
 
 exception Beginning_of_string_appears_twice;;   
   
 let left_helper_for_unique_occurrence s i j=
   let tester=(
     fun k->List.length(occurrences_of_in(String.sub s (k-1) (j-k+1)) s)=1
   ) in  
   if (not(tester 1))
   then raise(Beginning_of_string_appears_twice)
   else let rec tempf=(fun k->
           if tester k
           then k
           else tempf(k-1)
        ) in
        let k0=tempf(i) in
        String.sub s (k0-1) (i-k0);;

 let show ()=Scunix_command.uc "ocamlc -i substring.ml";;  
    end;;


module Sccull_string=struct (*

#use"cull_string.ml";;

*)




let interval s a b=String.sub s (a-1) (b-a+1);;

exception Beginning_failure;;

let beginning k s=
   if k<1 then "" else
   let n=String.length(s) in
   if (k>n)
   then raise(Beginning_failure)
   else String.sub s 0 k;;
   
exception Ending_failure;;   
   
 let ending k s=
   if k<1 then "" else
   let n=String.length(s) in
   if (k>n)
   then raise(Ending_failure)
   else String.sub s (n-k) k;;
    
 let cobeginning k s=ending (String.length(s)-k) s;; 
 
 let coending k s=beginning (String.length(s)-k) s;; 
 
 let resize_from_left s p c=
   let d=p-String.length(s) in
   if d>0
   then s^(String.make d c)
   else beginning p s;;
   
  let resize_from_right s p c=
   let d=p-String.length(s) in
   if d>0
   then (String.make d c)^s
   else ending p s;;  
     
 type leftwing_length=int;;
 type rightwing_length=int;;  
   
 let without_the_lid  (a:leftwing_length) s (b:rightwing_length)=
   String.sub s a (String.length(s)-b-a);;
 
 
let before_and_after w x=
  let j=Scsubstring.leftmost_index_of_in(w)(x) in
  if j=(-1) then None else 
   Some(  beginning (j-1) x,
    cobeginning (j+String.length(w)-1) x);;


 let trim_spaces s=
   let n=String.length s in
   let opt1=Scoption.find_it(fun j->not(List.mem(String.get s (j-1)) [' ';'\t';'\n']))(Scennig.ennig 1 n) in
   if opt1=None then "" else
   let i1=Scoption.unpack opt1 in
   let k1=Scoption.find_really(fun j->not(List.mem(String.get s (n-j)) [' ';'\t';'\n']))(Scennig.ennig 1 n) in 
   let j1=(n+1)-k1 in
   interval s i1 j1;;

 let left_core x y=
    if (x="")||(y="") then ("",x,y) else
    let hx=String.length(x) and hy=String.length(y) in
    let rec tempf=(fun j->
     if (j>=hx)||(j>=hy)
     then (beginning j y,cobeginning j x,cobeginning j y)
     else if String.get(x)(j)=String.get(y)(j)
          then tempf(j+1)
          else (beginning j y,cobeginning j x,cobeginning j y)
    )  in
    tempf 0;;  
   
 let right_core x y=
     if (x="")||(y="") then (x,y,"") else
    let hx=String.length(x) and hy=String.length(y) in
    let rec tempf=(fun j->
     if (j>=hx)||(j>=hy)
     then (coending j x,coending j y,ending j y)
     else if String.get(x)(hx-j)=String.get(y)(hy-j)
          then tempf(j+1)
          else (coending j x,coending j y,ending j y)
    )  in
    tempf 0;;
   
 let two_sided_core x y= 
    let (x1,y1,rc)=right_core x y in
    let (lc,x2,y2)=left_core x1 y1 in
    (lc,x2,y2,rc);;    
   
    
type left_encloser=string;;
type right_encloser=string;;

let try_remove_left_encloser s (le:left_encloser)=
    if Scsubstring.begins_with s le 
    then Some(cobeginning (String.length le) s)
    else None;; 

let try_remove_right_encloser s (re:right_encloser)=
    if Scsubstring.ends_with s re 
    then Some(coending (String.length re) s)
    else None;;    
   
let try_remove_both_enclosers s (le,re)=
    match try_remove_left_encloser s le with
     None->None
    |Some(t)->try_remove_right_encloser t re;;
      
 let closeup_around_index s j=
   let n=String.length s in
   let temp1=List.filter(fun j->(String.get s (j-1))='\n')(Scennig.ennig 1 n) in
   let (temp2,temp3)=Scprepared.partition_in_two_parts(fun k->k<j) temp1 in
   let a=(if List.length(temp2)<6 then 1 else List.nth(List.rev temp2)(5))
   and b=(if List.length(temp3)<6 then n else List.nth(temp3)(5)) in
   String.sub s (a-1) (b-a);;
   
exception Absent_beginning_marker of string;;
exception Absent_ending_marker of string;; 
 
let between_markers (bm,em) s=
     if (bm,em)=("","") then s else
     let i1=Scsubstring.leftmost_index_of_in_from bm s 1  in
     if i1<1 then raise(Absent_beginning_marker(bm)) else
     let j1=i1+(String.length bm) in
     let i2=Scsubstring.leftmost_index_of_in_from em s (j1+1) in
     if i2<1 then raise(Absent_ending_marker(bm)) else
     interval s j1 (i2-1);; 
 
let optional_between_markers p s=
   try Some(between_markers p s) with _->None;; 
   
(*

between_markers ("aaa","bb") "123aaa45bb678";;

*)     
   

  
   end;;


module Sccode_namespace=struct (*
#use"code_namespace.ml";;
*)


let encode (bowl,l,s)=
    let s_bowl=(if bowl then "t" else "f") in
    s_bowl^(String.concat "#" l)^"*"^s;;
 
 let decode s=
   let j=Scsubstring.leftmost_index_of_in "*" s in
   let s1=Sccull_string.interval s 2 (j-1)
   and s2=Sccull_string.cobeginning j s in
   let l=Str.split (Str.regexp_string "#") s1 in
   ((String.get s 0)='t',l,s2);;
 
(*   
 let z1=(false,["ani";"mal";"inst";"inct"],"sally");;
 let z2=encode z1;;
 let check=(decode z2);;
 
 let z1=(true,["uncle";"joe";"is";"sick"],"again");;
 let z2=encode z1;;
 let check=(decode z2);;
*)
 end;;


module Scoverwriter=struct (*

#use"overwriter.ml";;

*)


type t=Ovw of string;;

let of_string s=Ovw(s);;
let to_string (Ovw s)=s;;
 end;;


module Scio=struct 
let make_filename_complete s=
  let home=Sys.getenv("HOME") in
  if s="" then Scabsolute_path.of_string(home) else
  let c=String.get s 0 in
  if c='/' then Scabsolute_path.of_string(s) else
  if c='~' then Scabsolute_path.of_string(home^(String.sub s 1 (String.length(s)-1))) else
  Scabsolute_path.of_string((Sys.getcwd ())^"/"^s);;

let open_in_locally x=try open_in(x) with 
_->failwith("File "^x^" cannot be opened in");;

let open_out_locally x=try open_out(x) with 
_->failwith("File "^x^" cannot be opened out");;  

let put_whole_content_of_file_in_buffer s=
  let x=Scabsolute_path.to_string(make_filename_complete(s)) in
  let janet=open_in_locally(x) in
  let n=in_channel_length(janet) in
  let b=Buffer.create(n) in
  let _=Buffer.add_channel(b)(janet)(n) in
  let _=close_in janet in
  b;;
  
type filename=string;;
  
let erase_file_and_fill_it_with_contents_of_buffer (fn:filename) b=
   let x=Scabsolute_path.to_string(make_filename_complete(fn)) in
  let john=open_out_locally(x) in
  (Buffer.output_buffer(john)(b);close_out john);;
  
let erase_file_and_fill_it_with_string ap s=
   let fn=Scabsolute_path.to_string ap in
   let n=String.length(s) in
   let b=Buffer.create(n) in
   let _=Buffer.add_string b s in
   erase_file_and_fill_it_with_contents_of_buffer fn b;;
   
let read_whole_file ap=   
   let s=Scabsolute_path.to_string ap in
   let b=put_whole_content_of_file_in_buffer(s) in
   Buffer.contents b;;

let append_string_to_file s ap=
  let new_content=(read_whole_file ap)^s in
  erase_file_and_fill_it_with_string ap new_content;; 

     
   
   
  
 end;;


module Scmy_global_replace=struct (*


#use"my_global_replace.ml";;

The my_global_replace is a replacement for Ocaml's Str.global_replace which has
the disadvantage of applying certain transforms to the replacement string.


*)


exception Ambiguity of string;;

let my_global_replace (a,b) s=
  let n=String.length(s) and na=String.length(a) in
  let indices=Scsubstring.occurrences_of_in a s in
  if indices=[] then s else
  let m=List.length(indices)+1 in
  let pattern_appears_left=((List.nth indices 0)=1)
  and pattern_appears_right=((List.nth indices (m-2))=n+1-na) in
  if m=1 then s else
  let fst_coord=(fun j->if j=1 then 1 else (List.nth indices (j-2))+na)
  and snd_coord=(fun j->if j=m then n else (List.nth indices (j-1))-1) in
  let coords=(fun j->
    if (j=1)&&pattern_appears_left then None else
    if (j=m)&&pattern_appears_right then None else
    Some(fst_coord j,snd_coord j)
  )
  in
  let unchanged_intervals=Scoption.filter_and_unpack coords (Scennig.ennig 1 m) in
  if List.exists (fun (x,y)->x>y) unchanged_intervals
  then raise(Ambiguity(a)) 
  else 
  let unchanged_substrings=Scimage.image (fun (x,y)->Sccull_string.interval s x y) unchanged_intervals in
  let draft=String.concat b unchanged_substrings in
  let left_padding=(if pattern_appears_left then b else "")
  and right_padding=(if pattern_appears_right then b else "") in
  left_padding^draft^right_padding;;
  
(*  
my_global_replace ("ab","cd") "12345ab6ab78cd91234ab679";; 
my_global_replace ("ab","cd") "ab12345ab6ab78cd91234ab679";; 
my_global_replace ("ab","cd") "12345ab6ab78cd91234ab679ab";;
my_global_replace ("1111","") "abc1111111111def";;
my_global_replace ("ab","cd") "xyz";;
*)  
  
 end;;


module Screplace_inside=struct (*

#use"replace_inside.ml";;

*)


let replace_inside_string (a,b) s=
  Scmy_global_replace.my_global_replace (a,b) s;;
 
let replace_several_inside_string l t=List.fold_left 
(fun s (a,b)->replace_inside_string (a,b) s) t l;;  
 
let replace_inside_file (a,b) fn=
    let s1=Scio.read_whole_file fn in
    let la=String.length(a) in
    if List.exists (fun j->(String.sub s1 j la)=a) (Scennig.ennig 0 ((String.length s1)-la))
    then let s2=replace_inside_string (a,b) s1 in
         Scio.erase_file_and_fill_it_with_string fn s2
    else ();; 
    
let replace_several_inside_file l fn=
    let s1=Scio.read_whole_file fn in
    let s2=replace_several_inside_string l s1  in
    Scio.erase_file_and_fill_it_with_string fn s2;; 

exception Absent_beginning_marker of string;;
exception Absent_ending_marker of string;; 
 
let overwrite_between_markers_inside_string ovw_b (bm,em)
   s1=
     let b=Scoverwriter.to_string ovw_b in
     if (bm,em)=("","") then b else
     let substring_leftmost_index_from=(fun x y i0->
      let lx=String.length(x) and ly=String.length(y) in
      let rec tempf=(fun j->
        if j>ly-lx then (-1) else 
        if (String.sub y j lx)=x then j else (tempf(j+1))
      ) in
      tempf i0) in
     let i1=substring_leftmost_index_from bm s1 0 in
     if i1=(-1) then raise(Absent_beginning_marker(bm)) else
     let j1=i1+(String.length bm)-1 in
     let i2=substring_leftmost_index_from em s1 (j1+1) in
     if i2=(-1) then raise(Absent_ending_marker(bm)) else
     let before=String.sub s1 0 (j1+1)
     and after=String.sub s1 i2 (String.length(s1)-i2) 
     in
     before^b^after ;; 
     
let overwrite_between_markers_inside_file 
   ovw_b (bm,em)
   fn =
    let s1=Scio.read_whole_file fn in
    let s2=overwrite_between_markers_inside_string ovw_b (bm,em) s1 in
    Scio.erase_file_and_fill_it_with_string fn s2;;      


let overwrite_and_dump_markers_inside_string ovw_b (bm,em)
   s1=
     let b=Scoverwriter.to_string ovw_b in
     if (bm,em)=("","") then b else
     let substring_leftmost_index_from=(fun x y i0->
      let lx=String.length(x) and ly=String.length(y) in
      let rec tempf=(fun j->
        if j>ly-lx then (-1) else 
        if (String.sub y j lx)=x then j else (tempf(j+1))
      ) in
      tempf i0) in
     let i1=substring_leftmost_index_from bm s1 0 in
     if i1=(-1) then raise(Absent_beginning_marker(bm)) else
     let j1=i1+(String.length bm)-1 in
     let i2=substring_leftmost_index_from em s1 (j1+1) in
     if i2=(-1) then raise(Absent_ending_marker(bm)) else
     let corrected_i2=i2+(String.length bm)-1 in
     let before=String.sub s1 0 i1
     and after=String.sub s1 corrected_i2 (String.length(s1)-corrected_i2) 
     in
     before^b^after ;; 
     
let overwrite_and_dump_markers_inside_file 
   ovw_b (bm,em)
   fn =
    let s1=Scio.read_whole_file fn in
    let s2=overwrite_and_dump_markers_inside_string ovw_b (bm,em) s1 in
    Scio.erase_file_and_fill_it_with_string fn s2;;      

let show ()=Scunix_command.uc "ocamlc -i replace_inside.ml";;  
 
(* 


 overwrite_between_markers_inside_string
  (Overwriter.of_string "456")
  ("aaa","bb")
   "123aaa5678bb78910" ;;    
   
overwrite_and_dump_markers_inside_string
  (Overwriter.of_string "456")
  ("aaa","bb")
   "123aaa5678bb78910" ;;       
   
     
*)




 end;;


module Scstrung=struct (*

#use"strung.ml";;

*)


let get s i=String.get s (i-1);;
 
let set s i c=Bytes.set s (i-1) c;;

let enclose s=
  let encloser="\"" in
  encloser^s^encloser;;


let implode l=
   let n=List.length(l) in
   let by=Bytes.make n ' ' in
   let _=(for i=0 to (n-1) do Bytes.set by i (List.nth l i) done;) in
   Bytes.to_string by;;
  
    
let explode s=
    let n=String.length s in
    Scennig.doyle (String.get s) 0 (n-1);;
    
 
 let finder f s w0=
   let n=(String.length s) in
   let rec tempf=(fun j->
     if j>=n then 0 else
     if f(String.get s  j) then j+1 else
     tempf(j+1)
   ) in
   tempf(w0-1);;
 
let show_indices s=
  let n=String.length s in
  Scennig.doyle (fun i->(i,String.get s (i-1)) ) 1 n;;   
   
let number_of_lines_before s i=
   if i<1 then 0 else
   let m=min i (String.length s) in
   List.length(List.filter(fun j->(get s j)='\n')(Scennig.ennig 1 m));;
     
let split c s=
   let n=String.length s in
   let temp1=List.filter (fun j->(String.get s (j-1))=c) (Scennig.ennig 1 n) in
   if temp1=[] then [s] else
   let i1=List.hd(temp1) and i2=List.hd(List.rev temp1) in
   let  leftmost_helper=(if i1=1 then [] else [0,i1])
   and rightmost_helper=(if i2=n then [] else [i2,n+1]) in
   let temp2=leftmost_helper@(Sclistennou.universal_delta_list temp1)@rightmost_helper in
   Scimage.image (fun (i,j)->String.sub s i (j-i-1)) temp2;;
   
(*   
  
split '.' "abc.de.back.in.the.days";;  
   
*)   
     
exception No_match_found of string;;     
     
let longest_match_parsing lexemes s=
    let n=String.length(s) in
    let rec tempf=(fun
        (graet,j)->
          if j>n
          then List.rev(graet)
          else            
          let c=get s j in
          if List.mem c [' ';'\n';'\r';'\t']
          then tempf(graet,j+1)
          else
          match Scoption.find_it(fun t->
            let l=String.length(t) in
            if j+l>n+1
            then false
            else (String.sub s (j-1) l)=t
          ) lexemes with
          None->raise(No_match_found(String.sub s (j-1) (n-j+1)))
          |Some(t0)->tempf(t0::graet,j+String.length(t0))
    ) in
    tempf([],1);;

exception Integer_too_big_for_string_of_int;; 

let left_completed_string_of_int l_max m=
   let s1=string_of_int(m) in
   let d=l_max-(String.length s1) in
   if d<0
   then raise(Integer_too_big_for_string_of_int)
   else
   (String.make d '0')^s1;;

(*

longest_match_parsing
  ["finally";"final";"else";"then";"dog";"if"]
   "if \n\rfinal then\t finally else dog";;
longest_match_parsing
  ["finally";"final";"else";"then";"dog";"if"]
   "if \n\rfinal then\t finally else dug";;


*)     
     
 
   
 end;;


module Scordered_string=struct (* file created using the "write_contents_of_ordered_list_module" function *)
(* in Creators/ordered_list_creator.ml *)

type set=string Scordered.old_set;;
let lt s1 s2=
	let n1=String.length(s1) and n2=String.length(s2) in
	if n1=n2
	then match Scennig.find_it(function j->String.get(s1)(j)<>String.get(s2)(j) )(0)(n1-1)
		with
			 None->false
			|Some(j0)->String.get(s1)(j0)<String.get(s2)(j0) 
	else n1<n2;;
let cmp=((Sctotal_ordering.from_lt lt):>(string Sctotal_ordering.t));;


let unsafe_set=(Scordered.unsafe_set:>(string list-> set));;
let forget_order=(Scordered.forget_order:>(set->string list));;

let kreskus_strizh x=Scordered.kreskus_strizh cmp x;;
let kreskus x=Scordered.kreskus cmp x;;

let elfenn=((fun a ox->Scordered.elfenn cmp a ox):>(string->set->bool));;
let teuzin=((fun ox oy->Scordered.teuzin cmp ox oy):>( set->set->set));;
let diforchan=((fun x->Scordered.diforchan cmp x):>(string list->set));;
let lemel=((fun ox oy->Scordered.lemel cmp ox oy):>(set->set->set));;
let ental=((fun ox oy->Scordered.ental cmp ox oy):>(set->set->bool));;
let kengeij=((fun ox oy->Scordered.kengeij cmp ox oy):>set->set->set);;
let kengeij_goullo=((fun ox oy->Scordered.kengeij_goullo cmp ox oy):>set->set->bool);;
let min=((fun x->Scordered.min cmp x):>string list->string);;
let max=((fun x->Scordered.max cmp x):>string list->string);;

let hd ox=List.hd(forget_order ox);;
let image f ox=Scimage.image f (forget_order ox);;
let rev_map f ox=Scimage.image f (forget_order ox);;
let empty_set=unsafe_set [];;
let singleton x=unsafe_set [x];;
let filter f x=unsafe_set(List.filter(f)(forget_order x));;
let partition f ox=
         match List.partition(f)(forget_order ox) with
           (u,v)->(unsafe_set u,unsafe_set v);;
let length x=List.length(forget_order x);;

let nelfenn a ox=not(elfenn a ox);;
let nental ox oy=not(ental ox oy);;

let insert x oy=teuzin(singleton x) oy;;
let safe_set x=if kreskus_strizh(x) then unsafe_set(x) else diforchan(x);;
let outsert x oy=lemel(oy)(singleton x);;
let delta_set ox oy=teuzin(lemel ox oy)(lemel oy ox);;
let delta_distance ox oy=length(delta_set ox oy);;


let big_teuzin x=List.fold_left teuzin empty_set x;;
let big_kengeij=function
    []->failwith("empty intersection undefined")
    |a::b->List.fold_left(kengeij)(a)(b);;
    
    
let expand_boolean_algebra=
 ((fun x->Scordered.expand_boolean_algebra cmp x):>(set list->(set list)));; 
 
 
 end;;


module Scphp_token=struct (*

#use"Php_analizer/php_token.ml";;

*)



type t=
    {
      form : Scphp_projected_token.t;
      content : string;
    }

let form tok=tok.form;;
let content tok=tok.content;;

let make proj s=
    {
      form = proj;
      content =s ;
    }
    
(* Constructors *)

    let comment s = make Scphp_projected_token.Comment s;;
    let constant ctok = make (Scphp_projected_token.Constant(ctok)) "";;
    let double_quoted s = make Scphp_projected_token.Double_quoted s;;
    let end_of_text = make Scphp_projected_token.End_of_text "";;
    let external_echo s = make Scphp_projected_token.External_echo s;;
    let ident s = make Scphp_projected_token.Ident s;;
    let heredoc s = make Scphp_projected_token.Heredoc s;;
    let namespacer triple = let s=Sccode_namespace.encode triple in
                        make Scphp_projected_token.Namespacer s;;
    let nowdoc s = make Scphp_projected_token.Nowdoc s;;
    let of_char s = make Scphp_projected_token.Char s;;
    let of_float s = make Scphp_projected_token.Float s;;
    let of_int s = make Scphp_projected_token.Int s;;
    let single_quoted s = make Scphp_projected_token.Single_quoted s;;
    let variable s = make Scphp_projected_token.Variable s;;

    let op s=constant(Scphp_constant_token.Op(Scphp_operator.of_string s));;
    let punct s=constant(Scphp_constant_token.Punct(Scphp_punctuator.of_string s));;
    let kwd s=constant(Scphp_constant_token.Kwd (Scphp_keyword.of_string s));;


(* end of constructors ¨*)

    let short_content x=
      let s=content x in
      if String.length(s)>50
      then "..."
      else s;;
   
   let is_a_comment x=(form x)=Scphp_projected_token.Comment;;
   
   let fixture_of_nonconstants=
       [
          variable""; 
          ident"";
          comment"";
          single_quoted"";
          double_quoted"";
          heredoc"";
          nowdoc"";
          namespacer (false,[],"");
          external_echo"";
          of_int "0";
          of_float "0.";
          of_char "0";
       ];;
   

let put_lexeme_in_category=Scmemoized.make(fun s->
  match Scphp_operator.of_prudent_string s with
   Some(_)->op s
  |None->
  (
   match Scphp_punctuator.of_prudent_string s with
   Some(_)->punct s
  |None->
   (
    match Scphp_keyword.of_prudent_string s with
     Some(_)->kwd s
    |None->ident s
   ) 
  ));;
  
let of_string=put_lexeme_in_category;;  
  
let all_constant_strings=
   ( Scphp_operator.all_strings)
  @( Scphp_punctuator.all_strings)
  @( Scphp_keyword.all_strings);;  
  
let nonalphanumeric_lexemes=
  let temp4=List.filter (fun s->
     not(Sccharset.string_is_alphanumeric s)
    ) all_constant_strings in
  let temp5=Scordered_string.diforchan temp4 in
  let temp6=Scordered.forget_order temp5 in
  List.rev_map (fun s->(s,put_lexeme_in_category s) ) temp6;;
   

let instructions_for_nonalphanumeric_lexemes=
  let temp1=Scimage.image (
     fun (x,_)->Scstrung.enclose (Str.global_replace (Str.regexp_string "\n") "\\n" x) 
  ) nonalphanumeric_lexemes in
  let temp2=String.concat "\n  | " temp1 in
  "\n  | "^temp2^" as op {add_to_list lexbuf (read_word op);usual lexbuf}";;
  
   
let give_instructions_for_nonalphanumeric_lexemes ()=
   let s=instructions_for_nonalphanumeric_lexemes 
   and beg_m="(* instructions for nonalphanumeric chars begin here *)"
   and end_m="(* instructions for nonalphanumeric chars end here *)" in
   Screplace_inside.overwrite_between_markers_inside_file 
    (Scoverwriter.of_string s)
    (beg_m,end_m)
    (Scabsolute_path.of_string "Php_analizer/php_lexer.mll");;


let projected_version tok=Scphp_projected_token.to_string(form tok);;

 

let precedence tok=Scphp_projected_token.precedence(form tok);;




let test ctok tok=(tok=constant(ctok));;



 end;;


module Scpositioned_php_token=struct (*

#use"Php_analizer/positioned_php_token.ml";;

*)

type t=
    PPL of Scphp_token.t*(Lexing.position * Lexing.position);; 

let make x y=PPL(x,y);;
let unveil (PPL(x,y))=(x,y);;
let fst (PPL(x,y))=x;;
let snd (PPL(x,y))=y;;

let file (PPL(_,(y1,_)))=y1.Lexing.pos_fname;;

let print (PPL(x,y))=
  let s=Scphp_token.content x in
  if String.length(s)>50
  then "\xe2\x8c\x98...\xe2\x8c\x98 "
  else "\xe2\x8c\x98 "^s^"\xe2\x8c\x98 ";;

let print_out (fmt:Format.formatter) x=
   Format.fprintf fmt "@[%s@]" (print x);;


 end;;


module Scpositioned_php_token_list=struct (*

#use"Php_analizer/positioned_php_token_list.ml";;

*)

type t={
   contained : Scpositioned_php_token.t list;
};;

let empty={contained=[]};;
let is_empty x=(x.contained=[]);;
let hd x=List.hd(x.contained);;
let tl x={contained=List.tl(x.contained)};;
let concat x y={contained=(x.contained)@(y.contained)};;

let cons a x={contained=a::(x.contained)};;
let singleton x={contained=[x]};;
let rev x={contained=List.rev(x.contained)};;
let length x=List.length(x.contained);;
let big_head d x={contained=Sclistennou.big_head d (x.contained)};;

let filter f x={contained=List.filter f (x.contained)}

exception Ht_exn;;

let ht x=match x.contained with
    []->raise(Ht_exn)
    |a::b->(a,{contained=b});;
    
exception File_exn;;    
    
let file x=match x.contained with
    []->raise(File_exn)
    |a::_->Scpositioned_php_token.file a;;    
    
let print x=
  let temp1=Scimage.image(fun ptok->
    let tok=Scpositioned_php_token.fst ptok in
    Scphp_token.projected_version tok
   ) x.contained in
  "\xe3\x80\x90  "^(String.concat " " temp1)^"  \xe3\x80\x91";;

let print_out (fmt:Format.formatter) x=
   Format.fprintf fmt "@[%s@]" (print x);;

     end;;


module Screcreating_tools=struct (*

#use"recreating_tools.ml";;

*)


let encode_ctoken=function
   Scphp_constant_token.Kwd(kwd)->(Some(kwd),None,None)
  |Scphp_constant_token.Punct(pct)->(None,Some(pct),None)
  |Scphp_constant_token.Op(op)->(None,None,Some(op));;

exception Decode_ctoken_exn;;

let decode_ctoken (opt1,opt2,opt3)=
   if opt1<>None
   then Scphp_constant_token.Kwd(Scoption.unpack opt1)
   else 
   if opt2<>None
   then Scphp_constant_token.Punct(Scoption.unpack opt2)
   else 
   if opt3<>None
   then Scphp_constant_token.Op(Scoption.unpack opt3)
   else raise(Decode_ctoken_exn);;  

let encode_ptoken ptok=match ptok with
  Scphp_projected_token.Constant(ctok)->(encode_ctoken ctok,Scphp_projected_token.Int)
  |_->((None,None,None),ptok);; 

let decode_ptoken (w,ptok1)=
    if w=(None,None,None)
    then ptok1
    else Scphp_projected_token.Constant(decode_ctoken w);;  

let encode_token tok=
    (encode_ptoken (tok.Scphp_token.form),tok.Scphp_token.content);;

let decode_token (frm,ctnt)=
   {
    Scphp_token.form=decode_ptoken(frm);
    Scphp_token.content=ctnt;
   };;
   
let encode_postoken (Scpositioned_php_token.PPL(x,y))=
    (encode_token x,y);;

let decode_postoken (x1,y)=
  (Scpositioned_php_token.PPL(decode_token x1,y));;
      
let encode_postokenlist x=
    Scimage.image encode_postoken
    (x.Scpositioned_php_token_list.contained);;

let decode_postokenlist l=  
  { Scpositioned_php_token_list.contained=
     Scimage.image decode_postoken l };;














 end;;


module Scmax=struct 


let list=function 
[]->failwith("max of empty set undefined according to Garfield")
|a::b->List.fold_left(max)(a)(b);;

let maximize_it f=function
[]->failwith("max on empty set undefined")
|x::y->
 let rec maximize_it0=(function
  (current_candidate,current_value,da_ober)->match da_ober with
  []->(current_candidate,current_value)
  |a::peurrest->let va=f(a) in
                if (va>current_value)
				then maximize_it0(a,va,peurrest)
				else maximize_it0(current_candidate,current_value,peurrest)
 ) 
in
 maximize_it0(x,f(x),y);;
 
let maximize_it_if_possible f l=
   let temp1=Scoption.filter_and_unpack (function 
     None->None
    |Some(x)->Some(x,f x) ) l in
   if temp1=[]
   then None
   else Some(fst(maximize_it(snd) temp1));;
 

let maximize_it_with_care f=function
[]->failwith("careful max on empty set undefined")
|x::y->
 let rec maximize_it_with_care0=(function
  (current_candidates,current_value,da_ober)->match da_ober with
  []->(current_value,List.rev(current_candidates))
  |a::peurrest->let va=f(a) in
                if (va>current_value)
				then maximize_it_with_care0([a],va,peurrest)
				else if (va=current_value)
				     then maximize_it_with_care0(a::current_candidates,current_value,peurrest)
					 else maximize_it_with_care0(current_candidates,current_value,peurrest)
 ) 
in
 maximize_it_with_care0([x],f(x),y);;
 end;;


module Scphp_char_range=struct (*

#use"Php_analizer/php_char_range.ml";;

*)

type t=CR of (Lexing.position * Lexing.position);;

let make a b=CR(a,b);;

let unveil(CR(a,b))=(a,b);;

let fst (CR(a,b))=a;;
let snd (CR(a,b))=b;;

let chasles (CR(a,_)) (CR(_,b))=CR(a,b);;

let dummy_lexing=
{Lexing.pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = 0};;

let dummy=
  CR(dummy_lexing,dummy_lexing);;
  
let add_if_nondummy x l=
   if x=dummy_lexing then l else x::l;;

let select_head l=
   if l=[] 
   then dummy_lexing
   else List.hd(l);;

 end;;


module Scphp_parser=struct (*

#use"Php_analizer/php_parser.ml";;

*)

type 'a t= 
((Scpositioned_php_token_list.t )->
((('a)*Scphp_char_range.t*(Scpositioned_php_token_list.t)) option));;

 
let parse (f:'a t) l=f l;;  
 

  
 end;;


module Scglued_or_not=struct (*

#use"retained_or_not.ml";;

*)


type t=
   Glued
  |Retained_not_glued
  |Not_retained_not_glued;;
  
  

 end;;


module Scparenthesed_block=struct (*

#use"parenthesed_block.ml";;

Decompose a string into parenthesed blocks.
Sample examples at the end of this file

The same left parenthesis may correspond to different right
parentheses. This is why the currently_open_pars field has type
(parenthesis_pair list) list rather than just parenthesis_pair list.


*)

type parenthesis_pair=string*string;;
type associator=string;;


type data_for_decomposition={
   mutable partial_result : ((parenthesis_pair option)*string) list;
   mutable currently_open_pars : (parenthesis_pair list) list;
   mutable smallest_unprocessed_index : int;
   mutable cursor_location : int; 
};;

let initial_data={
  partial_result=[];
  currently_open_pars=[];
  smallest_unprocessed_index=1;
  cursor_location=1;
};;



let test_for_left_paren_at_index 
   s i ((lparen,rparen):parenthesis_pair)=Scsubstring.is_a_substring_located_at lparen s i;;
 
let test_for_right_paren_at_index 
   s i ((lparen,rparen):parenthesis_pair)=Scsubstring.is_a_substring_located_at rparen s i;;
 
let look_for_left_paren_at_index app s i=
   let rec finder=(fun
    possibilities->match possibilities with
    []->None
    |paren::other_parens->
      if test_for_left_paren_at_index s i paren
      then Some(paren)
      else finder other_parens
   ) in
   finder app;;
  
let process_without_open_pars app  s data=
   match look_for_left_paren_at_index app s data.cursor_location with
     None->(data.cursor_location<-data.cursor_location+1)
    |Some(paren)->
                let (lparen,rparen)=paren in
               let _=(
               if data.currently_open_pars=[]
               then let i_start=data.smallest_unprocessed_index
                    and i_end=data.cursor_location-1 in
                    let _=(data.smallest_unprocessed_index<-data.cursor_location) in
                    if i_start<=i_end 
                    then let enclosed_substring=Sccull_string.interval s i_start i_end in
                    	 let new_result=(None,enclosed_substring) in
                    	 data.partial_result<-new_result::(data.partial_result)
               ) in
               let temp1=List.filter (fun par->fst(par)=lparen) app in
               (
                data.currently_open_pars<-(temp1::data.currently_open_pars);
                data.cursor_location<-data.cursor_location+String.length(lparen)
               );;
               
let process_with_open_pars app  s data=
  let temp1=List.hd(data.currently_open_pars) 
  and i=data.cursor_location in
  let opt1=Scoption.find_it (fun paren->test_for_right_paren_at_index s i paren) temp1 in
  if opt1=None
  then process_without_open_pars app  s data
  else 
       let best_paren=Scoption.unpack opt1 in
       let (lparen,rparen)=best_paren 
       and new_list=List.tl(data.currently_open_pars) in
       let _=(
          data.currently_open_pars<-new_list;
        (data.cursor_location<-data.cursor_location+String.length(rparen))
       ) in
       if new_list<>[]
       then ()
       else let i_start=data.smallest_unprocessed_index+String.length(lparen)
            and i_end=i-1 in
            let enclosed_substring=Sccull_string.interval s i_start i_end in
            let new_result=(Some(best_paren),enclosed_substring) in
            (
                data.partial_result<-new_result::(data.partial_result);
                data.smallest_unprocessed_index<-data.cursor_location
             )
            ;;              

let process app s data=
  if data.currently_open_pars=[]
  then process_without_open_pars app s data
  else process_with_open_pars app s data;;

let final_touch s data=
  let a=data.smallest_unprocessed_index
  and b=String.length s in
  if (a<=b)
  then let new_result=(None,Sccull_string.interval s a b) in
       data.partial_result<-new_result::(data.partial_result);;

let decompose_without_taking_blanks_into_account app s=
  let data={
  		partial_result=[];
  		currently_open_pars=[];
  		smallest_unprocessed_index=1;
  		cursor_location=1;
  } in
  while data.cursor_location<=(String.length s)
  do
     process app s data
  done;
  final_touch s data;
  List.rev(data.partial_result);;   

module With_associator=struct

   let test_for_associator_at_index  (asc:associator) s i=
     Scsubstring.is_a_substring_located_at asc s i;;

   let process_without_open_pars (asc:associator) app  s data=
   match look_for_left_paren_at_index app s data.cursor_location with
     None->(
           if not(test_for_associator_at_index asc s data.cursor_location)
           then data.cursor_location<-data.cursor_location+1
           else
                (
                  let i_start=data.smallest_unprocessed_index
                  and i_end=data.cursor_location-1 in
                (
                  if i_start<=i_end 
                    then let enclosed_substring=Sccull_string.interval s i_start i_end in
                    	 let new_result=(None,enclosed_substring) in
                    	 data.partial_result<-new_result::(data.partial_result)  
                );
                data.cursor_location<-data.cursor_location+String.length(asc);
                data.smallest_unprocessed_index<-data.cursor_location
                )
           )
    |Some(paren)->
               let (lparen,rparen)=paren in
               let temp1=List.filter (fun par->fst(par)=lparen) app in
               data.currently_open_pars<-(temp1::data.currently_open_pars);
               data.cursor_location<-data.cursor_location+String.length(lparen)
               ;;
               
let process_with_open_pars (asc:associator) app  s data=
  let temp1=List.hd(data.currently_open_pars) 
  and i=data.cursor_location in
  let opt1=Scoption.find_it (fun paren->test_for_right_paren_at_index s i paren) temp1 in
  if opt1=None
  then (
          match look_for_left_paren_at_index app s data.cursor_location with
     	  None->(data.cursor_location<-data.cursor_location+1)
        |Some(paren)->
               let (lparen,rparen)=paren in
               let temp1=List.filter (fun par->fst(par)=lparen) app in
               data.currently_open_pars<-(temp1::data.currently_open_pars);
               data.cursor_location<-data.cursor_location+String.length(lparen)
              
        )
  else (
       let best_paren=Scoption.unpack opt1 in
       let (lparen,rparen)=best_paren 
       and new_list=List.tl(data.currently_open_pars) in
       data.currently_open_pars<-new_list;
       data.cursor_location<-data.cursor_location+String.length(rparen)
       );;              

let process asc app s data=
  if data.currently_open_pars=[]
  then process_without_open_pars asc app s data
  else process_with_open_pars asc app s data;;

let final_touch s data=
  let a=data.smallest_unprocessed_index
  and b=String.length s in
  if (a<=b)
  then let new_result=(None,Sccull_string.interval s a b) in
       data.partial_result<-new_result::(data.partial_result);;

let decompose_without_taking_blanks_into_account asc app s=
  let data={
  		partial_result=[];
  		currently_open_pars=[];
  		smallest_unprocessed_index=1;
  		cursor_location=1;
  } in
  while data.cursor_location<=(String.length s)
  do
     process asc app s data
  done;
  final_touch s data;
  List.rev_map snd (data.partial_result);;   


end;;

let decompose_with_associator=
  ((With_associator.decompose_without_taking_blanks_into_account):
   associator -> parenthesis_pair list -> string -> string list
  );;

let decompose app s=
  let temp1=decompose_without_taking_blanks_into_account app s in
  let temp2=Scoption.filter_and_unpack (
     fun (lab,t)->
       let u=Sccull_string.trim_spaces t in
       if lab<>None then Some[lab,u] else
       if u="" then None else
       let ttemp1=Str.split(Str.regexp"[ \n\t]+") u in
       Some(Scimage.image (fun v->(None,v)) ttemp1)
  ) temp1 in
  List.flatten temp2;;

(*

Sample examples :

decompose [ ("(",")");("{","}");("BEGIN","END") ]
("How (much (research effort) is {expected} when) BEGIN posting a"^
"Code Review ENDquestion? A "^
"lot. {{(An absurd amount)}}. More BEGIN than  BEGIN you think END"^
"you ENDare capable of.");;


decompose [ ("[","]+");("[","]*");("BEGIN","END") ]
("ijk [abc [def]+ gh]* lm hhh [nop [qr]* stu]+ vw []+ ab  ");;

decompose [ ("[","]") ] "[ab]cd[efg]";;
decompose_without_taking_blanks_into_account [ ("[","]") ] "[ab]cd[efg]";;
decompose_without_taking_blanks_into_account [ ("[","]") ] "uv[ab][efg]";;
decompose_without_taking_blanks_into_account [ ("(",")") ]
"abc((de)|fgh)|ij|kl|||mno";;

decompose_with_associator "|" [ ("(",")") ]
"abc(de|fgh)|ij|kl|||mno";;

decompose_with_associator "|" [ ("(",")") ]
"|abc(de|fgh)|ij|kl|||mno";;

decompose_with_associator "asc" [ ("(",")") ]
"abc(deascfgh)ascijascklascascascmno";;

decompose_with_associator "|" [ ("(",")") ] "123((67)|012)|56|89|||345";;

*)






 end;;


module Scgeneralizer=struct (*

#use"Php_analizer/php_generalizer.ml";;

The values returned by the pair function below should
be compatible with Php_short_selector.all_string_constants list
and with the Php_constructible_recognizer.pair_for_disjunction
and Php_constructible_recognizer.associator_for_disjunction values.


   
*)

type t=
   Zero_or_one
  |Zero_or_more
  |One_or_more
  |One_or_more_with_right_end_removed;;
  
let all=
  [Zero_or_one;Zero_or_more;One_or_more;One_or_more_with_right_end_removed];;  
  
let pair=function
   Zero_or_one->("_l_","_r?_")
  |Zero_or_more->("_l_","_r*_")
  |One_or_more->("_l_","_r+_")
  |One_or_more_with_right_end_removed->("_l_","_r~_");;  
  
 let all_pairs=Scimage.image pair all;; 
 end;;


module Scphp_recognizer=struct (*

#use"Php_analizer/php_recognizer.ml";;

*)

type t=( Scpositioned_php_token_list.t -> 
(Scphp_char_range.t * Scpositioned_php_token_list.t) option );;

let recognize (f:t) l=f l;; end;;


module Scphp_recognizer_homomorphism=struct (*

#use"Php_analizer/php_recognizer_homomorphism.ml";;

A php_recognizer only recognizes patterns, giving a yes/no
answer and does not go deeper
into how the pattern is realized.
It can be viewed as a special Php_combinator.parser object :
type Php_recognizer.t would be equivalent to "unit Php_combinator.parser".
This identification only works 99% of the way though.

For example, Php_combinator.chain has type
'a parser list -> 'a list parser, while the chain here has type
t list-> t, as "unit list" can be identified with "unit".


*)


exception Stepper_for_chain_exn;;   
  
let stepper_for_chain 
  (da_ober,support,nondummy_left_lexings,nondummy_right_lexings,should_exit)=
   match da_ober with
   []->raise(Stepper_for_chain_exn)
   |rcgzr::peurrest->
     (
        match Scphp_recognizer.recognize rcgzr support with
       None->([],Scpositioned_php_token_list.empty,[],[],true)
      |Some(cr,support2)->
        (peurrest,support2,
          Scphp_char_range.add_if_nondummy (Scphp_char_range.fst cr) nondummy_left_lexings,
          Scphp_char_range.add_if_nondummy (Scphp_char_range.snd cr) nondummy_right_lexings, 
          false
        )    
     )
;;  

let rec helper_for_chain x=
    let (da_ober,support,nondummy_left_lexings,nondummy_right_lexings,should_exit)=x in
    if should_exit
    then None
    else 
    if da_ober=[]
    then  let u=Scphp_char_range.select_head nondummy_left_lexings
          and v=Scphp_char_range.select_head nondummy_right_lexings in
          let cr=Scphp_char_range.make u v in
          Some(cr,support)
    else helper_for_chain(stepper_for_chain x);; 
  
let chain l_rcgzr=
  let tempf=(fun l->helper_for_chain
     (l_rcgzr,l,[],[],false)
  ) in
  (tempf:Scphp_recognizer.t);;  
  
let zzz_helper_for_ordered_disjunction l=
  let rec tempf=(fun lf->
     match lf with
      []->None
     |rcgzr::peurrest->(
                      match Scphp_recognizer.recognize rcgzr l with
                       None->tempf peurrest
                      |Some(cr,l2)->Some(cr,l2)
                    )
  ) in 
  tempf;;
  
let ordered_disjunction lf=
   let f=(
      fun l->zzz_helper_for_ordered_disjunction l lf
   ) in
   (f: Scphp_recognizer.t);;  
   
let star rcgzr=
   let rec tempf=(fun (u,v,l)->
   match Scphp_recognizer.recognize rcgzr l with
    None->Some(Scphp_char_range.make u v,l)
   |Some(cr2,l2)->tempf(u,Scphp_char_range.snd cr2,l2)
   ) in 
   let f=(fun l->
   match Scphp_recognizer.recognize rcgzr l with
    None->Some(Scphp_char_range.dummy,l)
   |Some(cr1,l2)->tempf (Scphp_char_range.fst cr1,Scphp_char_range.snd cr1,l2)
   ) in   
   (f: Scphp_recognizer.t);;  
   
let unfinished_star rcgzr=
   let rec tempf=(fun (u,v1,l2,v2,l3)->
   match Scphp_recognizer.recognize rcgzr l3 with
    None->Some(Scphp_char_range.make u v1,l2)
   |Some(cr3,l4)->tempf(u,v2,l3,Scphp_char_range.snd cr3,l4)
   ) in 
   let f=(fun l1->
   match Scphp_recognizer.recognize rcgzr l1 with
    None->None
   |Some(cr1,l2)->
        let (u1,v1)=Scphp_char_range.unveil cr1 in
        (
          match Scphp_recognizer.recognize rcgzr l2 with
           None->Some(Scphp_char_range.dummy,l1)
          |Some(cr2,l3)->
            let (_,v2)=Scphp_char_range.unveil cr2 in
            tempf(u1,v1,l2,v2,l3)
        )
   ) in   
   (f: Scphp_recognizer.t);;  
   
   
let plus rcgzr=chain [rcgzr;star rcgzr];;   


let optional rcgzr=
   let f=(fun l->
   match Scphp_recognizer.recognize rcgzr l with
    None->Some(Scphp_char_range.dummy,l)
   |Some(cr,l2)->Some(cr,l2)
   ) in   
   (f: Scphp_recognizer.t);;  

let generalize glz rcgzr=match glz with
    Scgeneralizer.Zero_or_one->optional rcgzr
   |Scgeneralizer.Zero_or_more->star rcgzr
   |Scgeneralizer.One_or_more->plus rcgzr
   |Scgeneralizer.One_or_more_with_right_end_removed->unfinished_star rcgzr;;

   
   
    end;;


module Sctidel2=struct (* file created using the "write_contents_of_ordered_list_module" function *)
(* in Creators/ordered_list_creator.ml *)

type ('a,'b) set=('a*'b) Scordered.old_set;;
let lt ((u1,u2):'a*'b) ((v1,v2):'a*'b)=
        if u1=v1 then u2<v2 else u1<v1;;
let cmp=((fun x y->
		if lt(x)(y) then Sctotal_ordering.Lower else
		if lt(y)(x) then Sctotal_ordering.Greater else
		Sctotal_ordering.Equal): ('a*'b) Sctotal_ordering.t);;
let unsafe_set=(Scordered.unsafe_set:>(('a*'b) list-> ('a,'b) set));;
let forget_order=(Scordered.forget_order:>(('a,'b) set->('a*'b) list));;

let kreskus_strizh x=Scordered.kreskus_strizh cmp x;;
let kreskus x=Scordered.kreskus cmp x;;

let elfenn=((fun a ox->Scordered.elfenn cmp a ox):>(('a*'b)->('a,'b) set->bool));;
let teuzin=((fun ox oy->Scordered.teuzin cmp ox oy):>( ('a,'b) set->('a,'b) set->('a,'b) set));;
let diforchan=((fun x->Scordered.diforchan cmp x):>(('a*'b) list->('a,'b) set));;
let lemel=((fun ox oy->Scordered.lemel cmp ox oy):>(('a,'b) set->('a,'b) set->('a,'b) set));;
let ental=((fun ox oy->Scordered.ental cmp ox oy):>(('a,'b) set->('a,'b) set->bool));;
let kengeij=((fun ox oy->Scordered.kengeij cmp ox oy):>('a,'b) set->('a,'b) set->('a,'b) set);;
let kengeij_goullo=((fun ox oy->Scordered.kengeij_goullo cmp ox oy):>('a,'b) set->('a,'b) set->bool);;
let min=((fun x->Scordered.min cmp x):>('a*'b) list->('a*'b));;
let max=((fun x->Scordered.max cmp x):>('a*'b) list->('a*'b));;

let hd ox=List.hd(forget_order ox);;
let image f ox=Scimage.image f (forget_order ox);;
let rev_map f ox=Scimage.image f (forget_order ox);;
let empty_set=unsafe_set [];;
let singleton x=unsafe_set [x];;
let filter f x=unsafe_set(List.filter(f)(forget_order x));;
let partition f ox=
         match List.partition(f)(forget_order ox) with
           (u,v)->(unsafe_set u,unsafe_set v);;
let length x=List.length(forget_order x);;

let nelfenn a ox=not(elfenn a ox);;
let nental ox oy=not(ental ox oy);;

let insert x oy=teuzin(singleton x) oy;;
let safe_set x=if kreskus_strizh(x) then unsafe_set(x) else diforchan(x);;
let outsert x oy=lemel(oy)(singleton x);;
let delta_set ox oy=teuzin(lemel ox oy)(lemel oy ox);;
let delta_distance ox oy=length(delta_set ox oy);;


let big_teuzin x=List.fold_left teuzin empty_set x;;
let big_kengeij=function
    []->failwith("empty intersection undefined")
    |a::b->List.fold_left(kengeij)(a)(b);;
    
    
let expand_boolean_algebra=
 ((fun x->Scordered.expand_boolean_algebra cmp x):>(('a,'b) set list->(('a,'b) set list)));; 
 
 
 end;;


module Scphp_blocker_name=struct (*

#use"Php_analizer/php_blocker_name.ml";;


   
*)

type t=
   Parenthesis
  |Brace
  |Bracket
  |Ternop;;

let parenthesis=Parenthesis;;
let brace=Brace;;
let bracket=Bracket;;
let ternop=Ternop;;

let all=
  [parenthesis;brace;bracket;ternop];;  
  
let pair x=match x with
   Parenthesis->("(",")")
  |Brace->("{","}")
  |Bracket->("[","]")
  |Ternop->("?",":");;  
  
  let token_pair blckr=
    let (x,y)=pair blckr in
    (Scphp_token.put_lexeme_in_category x,Scphp_token.put_lexeme_in_category y);;
   
   end;;


module Scphp_blocker=struct (*

#use"Php_analizer/php_blocker.ml";;


   
*)


  
type t=Bl of Scphp_blocker_name.t * int;;

let depth (Bl(x,d))=d;;

let make x d=Bl(x,d);;

let parenthesis=Bl(Scphp_blocker_name.parenthesis,1);;
let brace=Bl(Scphp_blocker_name.brace,1);;
let bracket=Bl(Scphp_blocker_name.bracket,1);;
let ternop=Bl(Scphp_blocker_name.ternop,1);;

let all=
  [parenthesis;brace;bracket;ternop];;  
  
let pair (Bl(x,d))=Scphp_blocker_name.pair x;;  
  
let all_pairs=Scimage.image pair all;;  
  
let token_pair blckr=
   let (x,y)=pair blckr in
   (Scphp_token.put_lexeme_in_category x,Scphp_token.put_lexeme_in_category y);;
   end;;


module Scstrict_or_loose=struct (*

#use"strict_or_loose.ml";;
   
*)

type t=Strict |Loose;;

let all=[Strict;Loose];;

let to_string=function
   Strict->"strict"
  |Loose->"loose";;

let test sl x y=match sl with
    Strict->x<y
   |Loose->x<=y;;   end;;


module Scphp_projected_token_set=struct (*

#use"Php_analizer/php_projected_token_set.ml";;

*)
type t=N of Scphp_projected_token.t Scordered.old_set;;


module Private=struct
(* Inherited set operations *)

let from_list l=N(Scordered.diforchan Scphp_projected_token.order l);;

let kengeij (N x) (N y)=N(Scordered.kengeij 
   Scphp_projected_token.order x y
);;
    
let lemel (N x) (N y)=N(Scordered.lemel
Scphp_projected_token.order x y
);;

let is_a_singleton (N x)=(Scordered.length(x)=1);;

let whole=from_list Scphp_projected_token.all_tokens;;

let complement x=lemel whole x;;

let complement_from_list l=complement(from_list l);;


let from_precedence sol op=
    from_list(
      Scphp_projected_token.precedence_neutral_tokens
      @
      (
        List.filter (fun ptok->
        match Scphp_projected_token.precedence(ptok) with
        None->false
        |Some(p)->Scstrict_or_loose.test sol p (Scphp_operator.precedence op)
        )
        Scphp_projected_token.all_tokens
      )
    );;


(* Naming used sets *)

let namelist=ref([]:(string*t) list);;

let name_counter=ref(0);;

let get_name_for_set x opt=
    match Scoption.find_it(fun (n,y)->y=x)(!namelist) with
     Some(name1,_)->name1
    |None->
      (
        match opt with
        Some(name2)->
           let _=(namelist:=(name2,x)::(!namelist)) in
           name2
        |None-> 
           let p=(!name_counter)+1 in
           let name3="tokset_"^(string_of_int p) in
           let _=(
                  name_counter:=p;
                  namelist:=(name3,x)::(!namelist)
                  ) in
           name3        
      );;

exception Unused_name of string;;

let get_set_for_name name=
  match Scoption.find_it(fun (n,y)->n=name)(!namelist) with
  Some(_,x)->x
 |None->raise(Unused_name(name));;

let define_precedence_set sol op=
    get_name_for_set (from_precedence sol op);;

(* Particular sets *)

let assign=from_list( Scimage.image 
           (fun x->Scphp_projected_token.Constant
                (Scphp_constant_token.Op(x)))
          [
           Scphp_operator.T_EQUALS; 
           Scphp_operator.T_VLINE_EQUALS; 
           Scphp_operator.T_PLUS_EQUALS; 
           Scphp_operator.T_MINUS_EQUALS;
           Scphp_operator.T_STAR_EQUALS
           ]
);;   

get_name_for_set assign (Some "assign");;

let coerce=from_list( Scimage.image 
(fun x->Scphp_projected_token.Constant
     (Scphp_constant_token.Op(x)))
[
  Scphp_operator.T_COERCE_TO_INT; 
  Scphp_operator.T_COERCE_TO_BOOL;
  Scphp_operator.T_COERCE_TO_STRING;
  Scphp_operator.T_COERCE_TO_ARRAY;
  Scphp_operator.T_COERCE_TO_OBJECT; 
  Scphp_operator.T_COERCE_TO_BOOL;
]
);;   

get_name_for_set coerce (Some "coerce");;


let id_or_var=from_list( 
[
  Scphp_projected_token.Variable; 
  Scphp_projected_token.Ident; 
]
);;   

get_name_for_set id_or_var (Some "id_or_var");;

let include_like=from_list( 
    Scimage.image (fun x->Scphp_projected_token.Constant (Scphp_constant_token.Kwd x))
    [Scphp_keyword.T_INCLUDE; 
     Scphp_keyword.T_INCLUDE_ONCE;                                
     Scphp_keyword.T_REQUIRE; 
     Scphp_keyword.T_REQUIRE_ONCE]   
  );;   
  
get_name_for_set include_like (Some "include_like");;

let int_or_string_or_var=from_list( 
  [
    Scphp_projected_token.Variable; 
    Scphp_projected_token.Ident; 
    Scphp_projected_token.Single_quoted;
    Scphp_projected_token.Double_quoted;
  ]
  );;   
  
get_name_for_set int_or_string_or_var (Some "int_or_string_or_var");;

let no_breach=complement_from_list( 
  Scimage.image (fun x->Scphp_projected_token.Constant(Scphp_constant_token.Kwd(x)))
  [
    Scphp_keyword.T_FOREACH;
    Scphp_keyword.T_ENDFOREACH;
  ]
  );;   
  
get_name_for_set no_breach (Some "no_breach");;

let no_colon=complement_from_list( 
  [
    Scphp_projected_token.Constant(Scphp_constant_token.Op Scphp_operator.T_COLON)
  ]
  );;   
  
get_name_for_set no_colon (Some "no_colon");;

let no_ivies=complement_from_list( 
  Scimage.image (fun x->Scphp_projected_token.Constant(Scphp_constant_token.Kwd(x)))
  [
    Scphp_keyword.T_IF; 
    Scphp_keyword.T_ELSE; 
    Scphp_keyword.T_ELSEIF; 
    Scphp_keyword.T_ENDIF;
  ]
  );;   
  
get_name_for_set no_ivies (Some "no_ivies");;

let no_left_brace=complement_from_list( 
  [
    Scphp_projected_token.Constant(Scphp_constant_token.Punct Scphp_punctuator.T_LBRACE)
  ]
  );;   
  
get_name_for_set no_left_brace (Some "no_left_brace");;

let no_semicolon=complement_from_list( 
  [
    Scphp_projected_token.Constant(Scphp_constant_token.Punct Scphp_punctuator.T_SEMICOLON)
  ]
  );;   
  
get_name_for_set no_semicolon (Some "no_semicolon");;

let no_ternary=complement_from_list( 
  Scimage.image (fun x->Scphp_projected_token.Constant(Scphp_constant_token.Op(x)))
  [
    Scphp_operator.T_QUESTION; 
    Scphp_operator.T_COLON;
  ]
  );;   
  
get_name_for_set no_ternary (Some "no_ternary");;

let stringy=complement_from_list( 
  (
    Scimage.image (fun x->Scphp_projected_token.Constant(x))
    [
      Scphp_constant_token.Op Scphp_operator.T_DOT; 
      Scphp_constant_token.Op Scphp_operator.T_LBRACKET;
      Scphp_constant_token.Op Scphp_operator.T_RBRACKET;
      Scphp_constant_token.Op Scphp_operator.T_QUESTION;
      Scphp_constant_token.Op Scphp_operator.T_COLON;
      Scphp_constant_token.Op Scphp_operator.T_EQUALS_MORE;
      Scphp_constant_token.Punct Scphp_punctuator.T_COLON_COLON;
      Scphp_constant_token.Punct Scphp_punctuator.T_LPARENTHESIS;
      Scphp_constant_token.Punct Scphp_punctuator.T_RPARENTHESIS;
      Scphp_constant_token.Punct Scphp_punctuator.T_COMMA;
      Scphp_constant_token.Punct Scphp_punctuator.T_ARROW;
    ]
  )
  @
  (
    [
      Scphp_projected_token.Variable; 
      Scphp_projected_token.Ident; 
      Scphp_projected_token.Comment; 
      Scphp_projected_token.Single_quoted;
      Scphp_projected_token.Double_quoted; 
      Scphp_projected_token.Heredoc; 
      Scphp_projected_token.Nowdoc
    ]
  ) 
  );;   
  
get_name_for_set stringy (Some "stringy");;

define_precedence_set Scstrict_or_loose.Loose Scphp_operator.T_EQUALS;;

let all_pairs=
   (
     Scimage.image 
     (fun (s,ptok)->(s,N(Scordered.S([ptok]))) )
     Scphp_projected_token.all_pairs
   )
   @
   (!namelist);;

end;;

let all_pairs=Private.all_pairs;;

let from_precedence=Private.from_precedence;;
let is_a_singleton=Private.is_a_singleton;;
let get_name_for_set=Private.get_name_for_set;;
let get_set_for_name=Private.get_set_for_name;;

let test (N s) x=Scordered.elfenn Scphp_projected_token.order x s;; 
 end;;


module Scphp_recognize_block=struct (*

#use"Php_analizer/php_recognize_block.ml";;

*)



let main
   f (left_blocker,right_blocker) depth tok_l=
   let rec tempf=(
   fun (graet,j,da_ober)->
     if Scpositioned_php_token_list.is_empty da_ober
     then None
     else 
     let (a,peurrest)=Scpositioned_php_token_list.ht da_ober in 
     let lxm=Scpositioned_php_token.fst(a) in
     if lxm=left_blocker
     then tempf(Scpositioned_php_token_list.cons a graet,j+1,peurrest)
     else
     if lxm=right_blocker
     then if j=1
          then Some((Scpositioned_php_token_list.rev graet,snd(Scpositioned_php_token.snd(a)),peurrest),a)
          else tempf(Scpositioned_php_token_list.cons  a graet,j-1,peurrest)
     else 
       if f lxm
       then tempf(Scpositioned_php_token_list.cons  a graet,j,peurrest)
       else None
   ) in
   tempf(Scpositioned_php_token_list.empty,depth,tok_l);;

 end;;


module Scphp_recognize_starting_block=struct (*

#use"Php_analizer/php_recognize_starting_block.ml";;

*)


let  rsb blckr_name=
   let (left_blocker,right_blocker)=Scphp_blocker_name.token_pair blckr_name in
  ((function l->
     if l=Scpositioned_php_token_list.empty then None else
     let (a,peurrest)=Scpositioned_php_token_list.ht l in
     if  Scpositioned_php_token.fst(a)<>left_blocker
     then None
     else 
     match Scphp_recognize_block.main (fun x->true) (left_blocker,right_blocker) 1 peurrest 
     with
     None->None
     |Some(((u,last_lxng,others),last_tok))->
        let fst_lxng=fst(Scpositioned_php_token.snd(a)) in
        Some(Scphp_char_range.make fst_lxng last_lxng,others)
   ) : Scphp_recognizer.t);;

 end;;


module Scphp_short_selector=struct (*

#use"Php_analizer/php_short_selector.ml";;

*)

type t =                                                                    
         Atomac of Scphp_projected_token_set.t 
        |Block of Scphp_blocker_name.t
        |Unusual_block of Scphp_blocker.t;;
      
let new_pairs=
   [
     "()",Block(Scphp_blocker_name.parenthesis);
     "{}",Block(Scphp_blocker_name.brace);
     "[]",Block(Scphp_blocker_name.bracket);
   ];;

let is_constant=function
   Atomac(atomac_sel)->Scphp_projected_token_set.is_a_singleton atomac_sel
  |Block(_)->false
  |Unusual_block(_)->false;;

let all_pairs=
   let temp1=
   (
     Scimage.image (fun (s,ato)->
        (s,Atomac(ato))
     ) Scphp_projected_token_set.all_pairs
   )
   @
   new_pairs in
   let temp2=Scimage.image (fun (s,ato)->(-(String.length s),(s,ato)) ) temp1 in
   let temp3=Sctidel2.diforchan temp2 in
   Sctidel2.image snd temp3;;

let all_string_constants=Scimage.image fst all_pairs;;

let list_from_string s=
  let temp1=Scstrung.longest_match_parsing all_string_constants s in
  Scimage.image (fun a->List.assoc a all_pairs) temp1;;

exception Unregistered of t;; 
 
let to_string x=try (fst(Scoption.find_really (fun p->snd(p)=x) all_pairs)) 
      with 
      _->raise(Unregistered(x));;

exception Unknown of string;;

let optional_of_string s0=match 
   Scoption.find_it (fun (s,sel)->s=s0) all_pairs with
   None->None
   |Some(_,sel)->Some(sel);;
   
let of_string s=match optional_of_string s with
   None->raise(Unknown(s))
  |Some(s)->s;;

   let recognize_atomac atomac_sel=
    let f=(function x->
       if x=Scpositioned_php_token_list.empty then None else
       let (a,peurrest)=Scpositioned_php_token_list.ht x in
         if Scphp_projected_token_set.test atomac_sel (Scphp_token.form(Scpositioned_php_token.fst a)) 
         then let (u,v)=Scpositioned_php_token.snd a in
              Some(Scphp_char_range.make u v,peurrest)
         else None
    ) in
    (f : Scphp_recognizer.t);;   

let recognize sel=
   let f=(function
      l->(
        match sel with
         Atomac(atomac_sel)->recognize_atomac atomac_sel l 
        |Block(blckr)->Scphp_recognize_starting_block.rsb blckr l
        |Unusual_block(blckr)->(match (Scphp_recognize_block.main (fun _->true) 
                                 (Scphp_blocker.token_pair blckr) 
                                 (Scphp_blocker.depth blckr) l) 
                                 with
                                 None->None
                                 |Some(((u,last_lxng,others),last_tok))->
                                    let fst_lxng=fst(Scpositioned_php_token.snd(Scpositioned_php_token_list.hd l)) in
                                    Some(Scphp_char_range.make fst_lxng last_lxng,others) 
        )                       
      )
   ) in
   (f : Scphp_recognizer.t);; 
   


(*

let gg x=
   let y=of_string x in
   let z=to_string y in
   let u=of_string z in
   (y,y=u);;

gg "if new instanceof kwd variable";;
gg "  if new instanceof kwd variable";;
gg "deny  if new instanceof kwd variable";;


*)


 end;;


module Scphp_constructible_recognizer=struct (*

#use"Php_analizer/php_constructible_recognizer.ml";;

Generic parser for php code.


*)

type t=
   Leaf of Scphp_short_selector.t
  |Generalized of Scgeneralizer.t*t
  |Chain of t list
  |Disjunction of t list;;
  
let pair_for_disjunction=("_l_","_rd_");;
let associator_for_disjunction="_u_";;  
  
let rec to_string=function
   Leaf(sel)->Scphp_short_selector.to_string sel
  |Generalized(grlz,x)->
                   let (lpar,rpar)=Scgeneralizer.pair grlz in
                   lpar^(to_string x)^rpar
  |Chain(l)->String.concat " " (Scimage.image to_string l)
  |Disjunction(l)->
                   let (lpar,rpar)=pair_for_disjunction in
                   lpar^
                   (String.concat associator_for_disjunction 
                     (Scimage.image to_string l))
                   ^rpar;;



let all_pairs=pair_for_disjunction::Scgeneralizer.all_pairs;;

exception Helper_for_string_reading_exn of ((string*string) option)*string;;

let helper_for_string_reading old_f (opt,t)=
       if opt=None then old_f t else
       let pair=Scoption.unpack opt in
       let opt2=Scoption.find_it
         (fun x->(Scgeneralizer.pair x)=pair)
         Scgeneralizer.all in
       if opt2<>None then Generalized(Scoption.unpack opt2,old_f t) else
       if pair=pair_for_disjunction
       then 
            let temp1=Scparenthesed_block.decompose_with_associator
                  associator_for_disjunction all_pairs t in
            Disjunction(Scimage.image old_f temp1)
       else
       raise(Helper_for_string_reading_exn(opt,t));; 



exception Empty_output;;

let rec of_string rough_s=
  let s=Sccull_string.trim_spaces rough_s in
  if s="" then raise(Empty_output) else
  let temp1=Scparenthesed_block.decompose_without_taking_blanks_into_account all_pairs s in
  let temp2=Scimage.image (fun (opt,t)->(opt,Sccull_string.trim_spaces t) ) temp1 in
  let temp3=List.filter (fun (opt,t)->t<>"") temp2 in
  if List.length(temp3)>1
  then Chain(Scimage.image (helper_for_string_reading of_string) temp3)
  else 
  let (opt,t)=List.hd temp3 in
  if opt<>None
  then helper_for_string_reading of_string (opt,t)
  else
  let temp5=Scphp_short_selector.list_from_string t in
  let temp4=Scimage.image (fun sel->Leaf(sel)) temp5 in
  if List.length(temp4)=1
  then List.hd(temp4)
  else Chain(temp4);;

let chain_content wh=
  match wh  with
   Chain(ch)->Some(ch)
  |_->None;;

let is_constant wh=
  match wh  with
   Leaf(sel)->Scphp_short_selector.is_constant sel
  |_->false;;



let recognize_selector=Scphp_short_selector.recognize;;

let recognize_generalized old_f grlz x=Scphp_recognizer_homomorphism.generalize grlz (old_f x);;

let recognize_chain old_f ch=Scphp_recognizer_homomorphism.chain (Scimage.image old_f ch);;

let recognize_disjunction old_f l=Scphp_recognizer_homomorphism.ordered_disjunction (Scimage.image old_f l);;


let rec recognize wh l=
  match wh  with
   Leaf(sel)->recognize_selector sel l
  |Generalized(grlz,x)->recognize_generalized recognize grlz x l
  |Chain(ch)->recognize_chain recognize ch l
  |Disjunction(dis)->recognize_disjunction recognize dis l;;


exception Reverse_sleepy_parse_exn of string;;

let reverse_sleepy_parse wh l=
  match wh  with
   Leaf(sel)->(Leaf(sel),l)
  |Generalized(grlz,x)->raise(Reverse_sleepy_parse_exn("generalized"))
  |Chain(ch)->raise(Reverse_sleepy_parse_exn("chain"))
  |Disjunction(dis)->raise(Reverse_sleepy_parse_exn("dis"));;





 end;;


module Scphp_lexer=struct  

let pos_fname x=x.Lexing.pos_fname;;
let pos_lnum x=x.Lexing.pos_lnum;;
let pos_bol x=x.Lexing.pos_bol;;
let pos_cnum x=x.Lexing.pos_cnum;;

let pos_make a b c d={
  Lexing.pos_fname=a;
  Lexing.pos_lnum=b;
  Lexing.pos_bol=c;
  Lexing.pos_cnum=d;
};;

let dummy_lexing = Lexing.dummy_pos;;
let dummy_pair = (dummy_lexing,dummy_lexing);;

let lexing_from_file file=
  let s=Scio.read_whole_file file in
  let lexbuf=Lexing.from_string s in
  lexbuf.Lexing.lex_curr_p <- {
      Lexing.pos_fname = (Scabsolute_path.to_string file);
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0;
    };
   lexbuf;;

let translated_lexing lxg j={
  Lexing.pos_fname=pos_fname lxg;
  Lexing.pos_lnum=pos_lnum lxg;
  Lexing.pos_bol=pos_bol lxg;
  Lexing.pos_cnum=(pos_cnum lxg)+j;
};;


let comment s=Scphp_token.comment(s);;
let ustring s=Scphp_token.single_quoted(s);;
let dstring s=Scphp_token.double_quoted(s);;
let variable s=Scphp_token.variable(s);;
let integer i=Scphp_token.of_int(i);;
let floating f=Scphp_token.of_float(f);;
let character c=Scphp_token.of_char(String.make 1 c);;
let end_of_text=Scphp_token.end_of_text;;
let external_echo s=Scphp_token.external_echo s;;

let read_word=Scphp_token.put_lexeme_in_category;;
    
type doctype=Nowdoc_type |Heredoc_type |Naked_doc_type;;    
    
let list_accu=ref Scpositioned_php_token_list.empty;;
let string_accu=ref"";;
let doc_ident_accu=ref"";;
let match_counter=ref 0;;
let current_doctype=ref Naked_doc_type;;
let faraway_beginning=ref Lexing.dummy_pos;;



let adhoc_doc_string doc_type s=match doc_type with
  Nowdoc_type->Scphp_token.nowdoc(s)
  |_->Scphp_token.heredoc(s);;
  
let namespace_list_accu=ref([]:string list);;
let namespace_absolute=ref(false);;
let namespace_string_accu=ref"";;  
  
let mk=Scpositioned_php_token.make;;
let uv=Scpositioned_php_token.unveil;;

let push lbuf (a,start_a,end_a) l=
   if Scpositioned_php_token_list.is_empty l 
   then Scpositioned_php_token_list.singleton(mk a (start_a,end_a)) 
   else
   let (h,peurrest)=Scpositioned_php_token_list.ht(l) in
   let (b,(start_b,end_b))=uv(h) in
    if (Scphp_token.form a,Scphp_token.form b)=
       (Scphp_projected_token.Comment,Scphp_projected_token.Comment)
    then  let ba=(Scphp_token.content a)^(Scphp_token.content b) in
          Scpositioned_php_token_list.cons (mk (comment ba) (start_b,end_a)) peurrest
    else Scpositioned_php_token_list.cons (mk a (start_a,end_a)) l ;;
   
let preceding_lexeme=ref(None);;
   
let add_to_list lbuf a=
    let start_a=translated_lexing(Lexing.lexeme_start_p lbuf) 1
    and end_a=Lexing.lexeme_end_p lbuf in
    (list_accu:=push lbuf (a,start_a,end_a) (!list_accu);
     preceding_lexeme:=Some(a));; 

let add_long_one_to_list lbuf a=
    let start_a=translated_lexing (!faraway_beginning) 1
    and end_a=translated_lexing (Lexing.lexeme_end_p lbuf) 0 in
    (list_accu:=push lbuf (a,start_a,end_a) (!list_accu);
     preceding_lexeme:=Some(a));;

let add_long_doc_to_list lbuf a=
    let start_a=translated_lexing (!faraway_beginning) 1
    and end_a=translated_lexing (Lexing.lexeme_end_p lbuf) (-1) in
    (list_accu:=push lbuf (a,start_a,end_a) (!list_accu);
     preceding_lexeme:=Some(a));;


let add_composite_to_list lbuf (b,c)=
    let start_b=translated_lexing(Lexing.lexeme_start_p lbuf) 1
    and end_c=Lexing.lexeme_end_p lbuf in
    let end_b=translated_lexing start_b (String.length b-1)
    and start_c=translated_lexing end_c (1-(String.length c)) in
    (
     list_accu:=push lbuf (read_word b,start_b,end_b) (!list_accu);
     list_accu:=push lbuf (read_word c,start_c,end_c) (!list_accu);
     preceding_lexeme:=Some(read_word c);
     );;     

let semicolon=Scphp_token.of_string ";";;

let insert_semicolon lbuf=
    let start_a=translated_lexing(Lexing.lexeme_start_p lbuf) 1 in
    let end_a=translated_lexing(Lexing.lexeme_start_p lbuf) 1 in
    list_accu:=push lbuf (semicolon,start_a,end_a) (!list_accu);;

let insert_semicolon_if_needed lbuf=
    if (!preceding_lexeme)<>(Some semicolon)
    then insert_semicolon lbuf;;
    
let add_to_string c=(string_accu:=(!string_accu)^(String.make 1 c));;
let add_to_doc_ident c=(doc_ident_accu:=(!doc_ident_accu)^(String.make 1 c))  ;;
let initialize_namespace lbuf=(
   faraway_beginning:=(Lexing.lexeme_end_p lbuf);
);;
  

let ingest_namespace_element word=
     let w=String.sub word 0 ((String.length word)-1) in
     (
     namespace_list_accu:=w::(!namespace_list_accu);
     namespace_string_accu:=(!namespace_string_accu)^word
     );;
let finish_namespace w lbuf=
      let full_list=List.rev(w::(!namespace_list_accu))
      and full_string=(!namespace_string_accu)^w in
      let tok=Scphp_token.namespacer (!namespace_absolute,full_list,full_string) in
      let start_t=(!faraway_beginning)
      and end_t=translated_lexing(Lexing.lexeme_end_p lbuf) 0 in
      (
        namespace_list_accu:=[];
        namespace_string_accu:="";
        list_accu:=Scpositioned_php_token_list.cons (mk tok (start_t,end_t)) (!list_accu);
      );;     
      
let initialize_doc lbuf=(
     doc_ident_accu:="\n"^(!doc_ident_accu);
     match_counter:=String.length(!doc_ident_accu)
     );;
let finish_comment lbuf =(add_long_one_to_list lbuf (comment(!string_accu)); string_accu:=""; );;
let finish_quote lbuf =(add_long_one_to_list lbuf (ustring(!string_accu)); string_accu:=""; );;
let finish_dquote lbuf =(add_long_one_to_list lbuf (dstring(!string_accu)); string_accu:=""; );;
let finish_nonphp lbuf =
 if (!string_accu)<>""
 then (
      
      add_long_one_to_list lbuf (external_echo(!string_accu));
      string_accu:="";
      );;


let finish_doc lbuf=
      let new_length=(String.length(!string_accu))-(String.length(!doc_ident_accu)) in
      let _=(string_accu:=String.sub (!string_accu) 0 new_length) in
      let tok=adhoc_doc_string (!current_doctype) (!string_accu) in
      string_accu:="";
      add_long_doc_to_list lbuf tok;
      insert_semicolon lbuf;;
      



exception Illegal_first_character_in_doc of char;;
exception Illegal_character_in_doc of char;;
exception Illegal_character_in_naked_doc of char;;
exception Illegal_character_in_namespace of char;;
exception Missing_newline_in_doc_beginning;;

exception Unfinished_doc of string*string*int;;
exception Badly_terminated_doc_ident;;

exception Badly_terminated_doc_text;;

let debug_list=ref [];;

    

let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\252\255\253\255\000\000\000\000\000\000\001\000\000\000\
    \254\255\255\255\082\000\237\255\238\255\241\255\162\000\242\000\
    \243\255\000\000\000\000\001\000\003\000\004\000\006\000\006\000\
    \008\000\161\000\009\000\010\000\011\000\013\000\012\001\063\001\
    \030\000\247\255\248\255\036\000\250\255\251\255\046\000\014\000\
    \026\000\028\000\255\255\032\000\254\255\252\255\253\255\005\000\
    \040\000\249\255\001\000\066\000\001\000\138\001\148\001\000\000\
    \000\000\095\000\000\000\004\000\104\000\107\000\001\000\113\000\
    \000\000\000\000\003\000\123\000\126\000\110\000\001\000\142\000\
    \158\000\142\000\002\000\172\000\190\000\167\000\003\000\178\000\
    \192\000\004\000\185\000\006\000\241\000\242\000\243\000\244\000\
    \242\255\223\001\042\002\117\002\192\002\011\003\134\002\253\255\
    \254\255\001\000\255\255\125\003\251\255\252\255\243\000\255\255\
    \014\001\017\001\253\255\018\001\254\255\063\003\251\255\252\255\
    \253\255\115\001\254\255\255\255\070\003\251\255\252\255\253\255\
    \071\003\254\255\255\255\117\003\252\255\192\003\254\255\255\255\
    \078\004\254\255\153\004\142\003\252\255\253\255\002\000\003\000\
    \255\255\254\255\111\000\254\255\255\255\020\004\253\255\254\255\
    \255\255\102\001\254\255\255\255\238\004\253\255\057\005\255\255\
    ";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\002\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\016\000\016\000\
    \255\255\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \009\000\255\255\255\255\012\000\255\255\255\255\012\000\008\000\
    \007\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \012\000\255\255\255\255\255\255\255\255\010\000\011\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\012\000\012\000\012\000\012\000\
    \255\255\016\000\016\000\016\000\016\000\015\000\255\255\255\255\
    \255\255\001\000\255\255\255\255\255\255\255\255\003\000\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\003\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \003\000\255\255\255\255\002\000\255\255\002\000\255\255\255\255\
    \000\000\255\255\000\000\255\255\255\255\255\255\003\000\003\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\001\000\255\255\
    ";
  Lexing.lex_default = 
   "\002\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\012\000\000\000\000\000\000\000\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\000\000\000\000\255\255\255\255\
    \255\255\255\255\000\000\255\255\000\000\000\000\000\000\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\096\000\000\000\
    \000\000\255\255\000\000\101\000\000\000\000\000\255\255\000\000\
    \255\255\255\255\000\000\255\255\000\000\111\000\000\000\000\000\
    \000\000\255\255\000\000\000\000\118\000\000\000\000\000\000\000\
    \255\255\000\000\000\000\124\000\000\000\255\255\000\000\000\000\
    \129\000\000\000\255\255\132\000\000\000\000\000\255\255\255\255\
    \000\000\000\000\139\000\000\000\000\000\142\000\000\000\000\000\
    \000\000\146\000\000\000\000\000\149\000\000\000\255\255\000\000\
    ";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\008\000\016\000\137\000\136\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \009\000\016\000\000\000\000\000\000\000\000\000\000\000\016\000\
    \000\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \098\000\016\000\000\000\000\000\086\000\000\000\000\000\000\000\
    \000\000\000\000\016\000\000\000\003\000\016\000\016\000\004\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\087\000\016\000\
    \085\000\016\000\016\000\084\000\053\000\043\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \046\000\041\000\042\000\034\000\039\000\045\000\044\000\033\000\
    \048\000\016\000\016\000\047\000\049\000\016\000\066\000\065\000\
    \006\000\051\000\063\000\016\000\071\000\067\000\082\000\079\000\
    \005\000\007\000\040\000\026\000\036\000\050\000\031\000\018\000\
    \019\000\037\000\030\000\016\000\027\000\020\000\016\000\021\000\
    \022\000\038\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\017\000\016\000\035\000\028\000\
    \029\000\023\000\016\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\016\000\013\000\016\000\
    \024\000\014\000\052\000\014\000\014\000\014\000\014\000\015\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\016\000\025\000\016\000\
    \016\000\075\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\061\000\062\000\016\000\064\000\
    \068\000\069\000\070\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\072\000\088\000\073\000\
    \001\000\014\000\074\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\016\000\076\000\077\000\
    \078\000\080\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\081\000\083\000\016\000\016\000\
    \016\000\016\000\107\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\105\000\088\000\106\000\
    \108\000\014\000\011\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\089\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\057\000\056\000\140\000\
    \147\000\000\000\058\000\000\000\000\000\055\000\000\000\000\000\
    \000\000\000\000\000\000\059\000\000\000\000\000\000\000\060\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\115\000\000\000\000\000\000\000\054\000\000\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\000\000\114\000\
    \000\000\000\000\000\000\000\000\000\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\000\000\
    \000\000\000\000\000\000\054\000\000\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\000\000\088\000\000\000\000\000\014\000\000\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\090\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\000\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\000\000\088\000\000\000\
    \000\000\014\000\000\000\014\000\014\000\014\000\014\000\091\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\000\000\
    \097\000\000\000\000\000\000\000\000\000\000\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \000\000\088\000\000\000\000\000\014\000\000\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\092\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\000\000\088\000\000\000\000\000\014\000\
    \000\000\014\000\014\000\014\000\014\000\014\000\093\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\112\000\088\000\
    \119\000\122\000\014\000\000\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\095\000\104\000\
    \000\000\000\000\103\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\126\000\
    \133\000\000\000\000\000\113\000\127\000\102\000\000\000\000\000\
    \000\000\000\000\120\000\121\000\000\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\000\000\
    \135\000\000\000\000\000\000\000\000\000\134\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \000\000\000\000\000\000\000\000\125\000\000\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\000\000\000\000\000\000\143\000\125\000\
    \000\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\000\000\000\000\000\000\000\000\110\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\117\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\144\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\100\000\130\000\130\000\
    \130\000\130\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\130\000\
    \130\000\130\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\130\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\130\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\000\000\000\000\000\000\000\000\130\000\000\000\130\000\
    \130\000\130\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\130\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\130\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\130\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\130\000\130\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\130\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\130\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\130\000\130\000\130\000\000\000\000\000\000\000\000\000\
    \130\000\000\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\130\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\130\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\130\000\130\000\130\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\000\000\000\000\000\000\000\000\150\000\255\255\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\000\000\151\000\000\000\000\000\
    \150\000\000\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\007\000\052\000\134\000\135\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \007\000\052\000\255\255\255\255\255\255\255\255\255\255\019\000\
    \255\255\065\000\070\000\074\000\078\000\081\000\020\000\083\000\
    \097\000\021\000\255\255\255\255\027\000\255\255\255\255\255\255\
    \255\255\255\255\017\000\255\255\000\000\018\000\019\000\003\000\
    \020\000\021\000\021\000\022\000\023\000\024\000\026\000\027\000\
    \028\000\028\000\029\000\029\000\032\000\039\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \038\000\040\000\041\000\010\000\010\000\038\000\043\000\010\000\
    \035\000\035\000\035\000\035\000\048\000\048\000\059\000\064\000\
    \005\000\050\000\062\000\038\000\058\000\066\000\055\000\056\000\
    \004\000\006\000\010\000\010\000\010\000\047\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\051\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\057\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\060\000\061\000\025\000\063\000\
    \067\000\068\000\069\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\071\000\014\000\072\000\
    \000\000\014\000\073\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\025\000\075\000\076\000\
    \077\000\079\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\080\000\082\000\084\000\085\000\
    \086\000\087\000\102\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\104\000\015\000\105\000\
    \107\000\015\000\010\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\030\000\030\000\138\000\
    \145\000\255\255\030\000\255\255\255\255\030\000\255\255\255\255\
    \255\255\255\255\255\255\030\000\255\255\255\255\255\255\030\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\113\000\255\255\255\255\255\255\031\000\255\255\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\255\255\113\000\
    \255\255\255\255\255\255\255\255\255\255\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\255\255\
    \255\255\255\255\255\255\054\000\255\255\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\089\000\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\255\255\089\000\255\255\255\255\089\000\255\255\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\090\000\090\000\090\000\090\000\090\000\090\000\
    \090\000\090\000\090\000\090\000\255\255\255\255\145\000\255\255\
    \255\255\255\255\255\255\090\000\090\000\090\000\090\000\090\000\
    \090\000\090\000\090\000\090\000\090\000\090\000\090\000\090\000\
    \090\000\090\000\090\000\090\000\090\000\090\000\090\000\090\000\
    \090\000\090\000\090\000\090\000\090\000\255\255\090\000\255\255\
    \255\255\090\000\255\255\090\000\090\000\090\000\090\000\090\000\
    \090\000\090\000\090\000\090\000\090\000\090\000\090\000\090\000\
    \090\000\090\000\090\000\090\000\090\000\090\000\090\000\090\000\
    \090\000\090\000\090\000\090\000\090\000\091\000\091\000\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\255\255\
    \094\000\255\255\255\255\255\255\255\255\255\255\091\000\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \255\255\091\000\255\255\255\255\091\000\255\255\091\000\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \092\000\092\000\092\000\092\000\092\000\092\000\092\000\092\000\
    \092\000\092\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\092\000\092\000\092\000\092\000\092\000\092\000\092\000\
    \092\000\092\000\092\000\092\000\092\000\092\000\092\000\092\000\
    \092\000\092\000\092\000\092\000\092\000\092\000\092\000\092\000\
    \092\000\092\000\092\000\255\255\092\000\255\255\255\255\092\000\
    \255\255\092\000\092\000\092\000\092\000\092\000\092\000\092\000\
    \092\000\092\000\092\000\092\000\092\000\092\000\092\000\092\000\
    \092\000\092\000\092\000\092\000\092\000\092\000\092\000\092\000\
    \092\000\092\000\092\000\093\000\093\000\093\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\093\000\093\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\093\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\093\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\093\000\109\000\093\000\
    \116\000\120\000\093\000\255\255\093\000\093\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\093\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\093\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\093\000\094\000\099\000\
    \255\255\255\255\099\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\123\000\
    \131\000\255\255\255\255\109\000\123\000\099\000\255\255\255\255\
    \255\255\255\255\116\000\120\000\255\255\123\000\123\000\123\000\
    \123\000\123\000\123\000\123\000\123\000\123\000\123\000\255\255\
    \131\000\255\255\255\255\255\255\255\255\131\000\123\000\123\000\
    \123\000\123\000\123\000\123\000\123\000\123\000\123\000\123\000\
    \123\000\123\000\123\000\123\000\123\000\123\000\123\000\123\000\
    \123\000\123\000\123\000\123\000\123\000\123\000\123\000\123\000\
    \255\255\255\255\255\255\255\255\123\000\255\255\123\000\123\000\
    \123\000\123\000\123\000\123\000\123\000\123\000\123\000\123\000\
    \123\000\123\000\123\000\123\000\123\000\123\000\123\000\123\000\
    \123\000\123\000\123\000\123\000\123\000\123\000\123\000\123\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\255\255\255\255\255\255\141\000\125\000\
    \255\255\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\255\255\255\255\255\255\255\255\109\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\116\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\141\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\123\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\099\000\128\000\128\000\
    \128\000\128\000\128\000\128\000\128\000\128\000\128\000\128\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\131\000\128\000\
    \128\000\128\000\128\000\128\000\128\000\128\000\128\000\128\000\
    \128\000\128\000\128\000\128\000\128\000\128\000\128\000\128\000\
    \128\000\128\000\128\000\128\000\128\000\128\000\128\000\128\000\
    \128\000\255\255\255\255\255\255\255\255\128\000\255\255\128\000\
    \128\000\128\000\128\000\128\000\128\000\128\000\128\000\128\000\
    \128\000\128\000\128\000\128\000\128\000\128\000\128\000\128\000\
    \128\000\128\000\128\000\128\000\128\000\128\000\128\000\128\000\
    \128\000\130\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\130\000\130\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\130\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\130\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\130\000\130\000\130\000\255\255\255\255\255\255\255\255\
    \130\000\255\255\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\130\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\130\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\130\000\130\000\130\000\141\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\255\255\255\255\255\255\255\255\148\000\128\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\150\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\255\255\150\000\255\255\255\255\
    \150\000\255\255\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\148\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec outside_php lexbuf =
    __ocaml_lex_outside_php_rec lexbuf 0
and __ocaml_lex_outside_php_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
              (finish_nonphp lexbuf; inside_php lexbuf;)

  | 1 ->
               (finish_nonphp lexbuf; inside_php lexbuf;)

  | 2 ->
let
         c
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
           (add_to_string c; outside_php lexbuf)

  | 3 ->
       (finish_nonphp lexbuf;Scpositioned_php_token_list.rev(!list_accu))

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_outside_php_rec lexbuf __ocaml_lex_state

and inside_php lexbuf =
    __ocaml_lex_inside_php_rec lexbuf 10
and __ocaml_lex_inside_php_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
           (
            faraway_beginning:=Lexing.lexeme_start_p lexbuf;
            (* insert_semicolon_if_needed lexbuf; *)
            outside_php lexbuf;)

  | 1 ->
           (
            
            faraway_beginning:=Lexing.lexeme_start_p lexbuf;
            (* insert_semicolon_if_needed lexbuf; *)
            Lexing.new_line lexbuf;
            outside_php lexbuf;
            )

  | 2 ->
         ( starred_comment lexbuf;)

  | 3 ->
         ( slashed_comment lexbuf;)

  | 4 ->
         ( 
            faraway_beginning:=Lexing.lexeme_start_p lexbuf; 
            single_quoted_string lexbuf;)

  | 5 ->
         ( 
            faraway_beginning:=Lexing.lexeme_start_p lexbuf;
            double_quoted_string lexbuf;)

  | 6 ->
          (
             faraway_beginning:=Lexing.lexeme_start_p lexbuf;
             doc_string lexbuf;
          )

  | 7 ->
               (inside_php lexbuf;)

  | 8 ->
                (Lexing.new_line lexbuf;inside_php lexbuf;)

  | 9 ->
let
              inum
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
   ( 
  	  let tok=integer inum in
  	  add_to_list lexbuf  tok; inside_php lexbuf
	)

  | 10 ->
let
                         fnum
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
   ( 
	  let tok=floating fnum in
  	  add_to_list lexbuf  tok; inside_php lexbuf
	)

  | 11 ->
let
              varname
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
                      (  add_to_list lexbuf  (variable varname); inside_php lexbuf)

  | 12 ->
let
           op
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
              (
                add_to_list lexbuf (read_word op);
                inside_php lexbuf
              )

  | 13 ->
let
               word
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
                    (
                      initialize_namespace lexbuf;
                      ingest_namespace_element word;
                      namespace_mode lexbuf
                    )

  | 14 ->
          (
            namespace_absolute:=true;
            namespace_string_accu:="\\";
            initialize_namespace lexbuf;
            namespace_mode lexbuf
           )

  | 15 ->
              ( add_composite_to_list lexbuf  ("else","if"); inside_php lexbuf )

  | 16 ->
let
          word
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
               ( add_to_list lexbuf  (read_word word); inside_php lexbuf )

  | 17 ->
let
         c
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
   (  add_to_list lexbuf  (character c); inside_php lexbuf)

  | 18 ->
   ( Scpositioned_php_token_list.rev(!list_accu) )

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_inside_php_rec lexbuf __ocaml_lex_state

and starred_comment lexbuf =
    __ocaml_lex_starred_comment_rec lexbuf 94
and __ocaml_lex_starred_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
        (finish_comment lexbuf; inside_php lexbuf)

  | 1 ->
let
         c
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
           (add_to_string c; starred_comment lexbuf)

  | 2 ->
       (Scpositioned_php_token_list.rev(!list_accu))

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_starred_comment_rec lexbuf __ocaml_lex_state

and slashed_comment lexbuf =
    __ocaml_lex_slashed_comment_rec lexbuf 99
and __ocaml_lex_slashed_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
               (finish_comment lexbuf; inside_php lexbuf)

  | 1 ->
         (finish_comment lexbuf; outside_php lexbuf)

  | 2 ->
          (finish_comment lexbuf; outside_php lexbuf)

  | 3 ->
let
         c
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
           (add_to_string c; slashed_comment lexbuf)

  | 4 ->
       (Scpositioned_php_token_list.rev(!list_accu))

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_slashed_comment_rec lexbuf __ocaml_lex_state

and single_quoted_string lexbuf =
    __ocaml_lex_single_quoted_string_rec lexbuf 109
and __ocaml_lex_single_quoted_string_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
           (add_to_string '\''; single_quoted_string lexbuf)

  | 1 ->
           (add_to_string '\\'; single_quoted_string lexbuf)

  | 2 ->
         (finish_quote lexbuf; inside_php lexbuf)

  | 3 ->
let
         c
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
           (add_to_string c; single_quoted_string lexbuf)

  | 4 ->
       (Scpositioned_php_token_list.rev(!list_accu))

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_single_quoted_string_rec lexbuf __ocaml_lex_state

and double_quoted_string lexbuf =
    __ocaml_lex_double_quoted_string_rec lexbuf 116
and __ocaml_lex_double_quoted_string_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
           (add_to_string '\''; double_quoted_string lexbuf)

  | 1 ->
           (add_to_string '\\'; double_quoted_string lexbuf)

  | 2 ->
         (finish_dquote lexbuf; inside_php lexbuf)

  | 3 ->
let
         c
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
           (add_to_string c; double_quoted_string lexbuf)

  | 4 ->
       (Scpositioned_php_token_list.rev(
        Scpositioned_php_token_list.cons
        (mk end_of_text (Lexing.lexeme_start_p lexbuf,Lexing.lexeme_end_p lexbuf))
         (!list_accu)))

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_double_quoted_string_rec lexbuf __ocaml_lex_state

and doc_string lexbuf =
    __ocaml_lex_doc_string_rec lexbuf 123
and __ocaml_lex_doc_string_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
         ( current_doctype:=Nowdoc_type;step_one_in_doc lexbuf;)

  | 1 ->
         ( current_doctype:=Heredoc_type;step_one_in_doc lexbuf;)

  | 2 ->
let
                s
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
                  (current_doctype:=Naked_doc_type;
                   doc_ident_accu:=s;
                   step_two_in_doc lexbuf;)

  | 3 ->
let
         c
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
           (raise(Illegal_first_character_in_doc(c)))

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_doc_string_rec lexbuf __ocaml_lex_state

and step_one_in_doc lexbuf =
    __ocaml_lex_step_one_in_doc_rec lexbuf 128
and __ocaml_lex_step_one_in_doc_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
                doc_ident
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
                          (doc_ident_accu:=doc_ident;step_two_in_doc lexbuf)

  | 1 ->
let
         c
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
           (raise(Illegal_character_in_doc(c)))

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_step_one_in_doc_rec lexbuf __ocaml_lex_state

and step_two_in_doc lexbuf =
    __ocaml_lex_step_two_in_doc_rec lexbuf 131
and __ocaml_lex_step_two_in_doc_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
          (if((!current_doctype)<>Heredoc_type) 
            then raise(Badly_terminated_doc_ident) 
            else initialize_doc lexbuf;step_three_in_doc lexbuf)

  | 1 ->
          (if((!current_doctype)<>Nowdoc_type) 
            then raise(Badly_terminated_doc_ident) 
            else initialize_doc lexbuf;step_three_in_doc lexbuf)

  | 2 ->
         (if((!current_doctype)<>Naked_doc_type) 
            then raise(Badly_terminated_doc_ident) 
            else initialize_doc lexbuf;step_three_in_doc lexbuf)

  | 3 ->
       (raise(Missing_newline_in_doc_beginning))

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_step_two_in_doc_rec lexbuf __ocaml_lex_state

and step_three_in_doc lexbuf =
    __ocaml_lex_step_three_in_doc_rec lexbuf 138
and __ocaml_lex_step_three_in_doc_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
       (raise(Unfinished_doc(!string_accu,!doc_ident_accu,!match_counter)))

  | 1 ->
let
          c
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
            (
      let s=(!doc_ident_accu)  in
      let n=String.length s in
      let j=(!match_counter) in
      (if ( c=(String.get s (n-j)) ) 
      then match_counter:=j-1
      else if c='\n'
           then match_counter:=n-1
           else match_counter:=n);
      add_to_string c;
      debug_list:=(c,!match_counter)::(!debug_list);
      if (( !match_counter)<1)
      then (
            step_four_in_doc lexbuf
           ) 
      else step_three_in_doc lexbuf
    )

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_step_three_in_doc_rec lexbuf __ocaml_lex_state

and step_four_in_doc lexbuf =
    __ocaml_lex_step_four_in_doc_rec lexbuf 141
and __ocaml_lex_step_four_in_doc_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
       (
         step_five_in_doc lexbuf      
        )

  | 1 ->
        (
         finish_doc lexbuf;
         inside_php lexbuf         
        )

  | 2 ->
let
        c
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
        (
           let m=String.length(!doc_ident_accu) in
           string_accu:=((!string_accu)^(!doc_ident_accu));
           add_to_string c;
           match_counter:=String.length(!doc_ident_accu);
           (if c='\n' 
           then match_counter:=m-1
           else match_counter:=m);
           step_three_in_doc lexbuf;
        )

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_step_four_in_doc_rec lexbuf __ocaml_lex_state

and step_five_in_doc lexbuf =
    __ocaml_lex_step_five_in_doc_rec lexbuf 145
and __ocaml_lex_step_five_in_doc_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
          (
             finish_doc lexbuf;
             inside_php lexbuf   
          )

  | 1 ->
         (
           raise(Badly_terminated_doc_text);
         )

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_step_five_in_doc_rec lexbuf __ocaml_lex_state

and namespace_mode lexbuf =
    __ocaml_lex_namespace_mode_rec lexbuf 148
and __ocaml_lex_namespace_mode_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
               word
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
                    (
                      ingest_namespace_element word;
                      namespace_mode lexbuf
                    )

  | 1 ->
let
          w
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
             (
               finish_namespace w lexbuf;
               inside_php lexbuf
             )

  | 2 ->
let
        c
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
          (raise(Illegal_character_in_namespace(c)))

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_namespace_mode_rec lexbuf __ocaml_lex_state

;;

 

let parse_string s =
          let _=(list_accu:=Scpositioned_php_token_list.empty;string_accu:="") in
          outside_php (Lexing.from_string s);;
  
let parse_file fn=
         let _=(list_accu:=Scpositioned_php_token_list.empty;string_accu:="") in
          outside_php (lexing_from_file fn);;
 end;;


module Sctermite=struct (*

#use"Php_analizer/termite.ml";;

A termite object is the first decomposing a list of tokens
during the parsing process.
Each termite scans according to just one predefined pattern.
This pattern is decomposed into several intervals, some of
which are retained (for further analysis later) and other which
are discarded (because they correspond to expected keywords in the
pattern).
A termite scanning according to pattern if()then{} for example,
will retain the () and {}´s contents and discard the "if" and the "then".
 

*)

type t=Trmt of (Scglued_or_not.t*Scphp_constructible_recognizer.t) list;;

let left_paren_for_retaining="##(";;
let right_paren_for_retaining=")##";;

let parens_for_retaining=(left_paren_for_retaining,right_paren_for_retaining);;

let default_embedding wh=
   if Scphp_constructible_recognizer.is_constant wh
   then (Scglued_or_not.Not_retained_not_glued,wh)
   else (Scglued_or_not.Retained_not_glued,wh);;

let rewriter (opt,t)=
      let better_t=Sccull_string.trim_spaces t in
      if better_t="" then [] else
      let wh=Scphp_constructible_recognizer.of_string better_t in
      if opt<>None 
      then [Scglued_or_not.Glued,wh]
      else (
                match Scphp_constructible_recognizer.chain_content wh with
                 None->[default_embedding wh]
                |Some(l)->
                  Scimage.image default_embedding l                
           
            );;
            
let of_string s=
   let temp1=Scparenthesed_block.decompose_without_taking_blanks_into_account 
     [parens_for_retaining] 
     (Sccull_string.trim_spaces s) in
   let temp2=Scimage.image rewriter temp1 in
   let temp3=List.flatten temp2 in
   Trmt(temp3);;
   
exception Stepper_for_parsing_exn;;

let pusher_for_parsing x=
   let ((graet,da_ober,lexings,l),opt)=x in
   if opt<>None 
   then x
   else 
   match da_ober with
  []->let temp1=List.rev_map Scphp_char_range.fst lexings 
      and temp2=Scimage.image Scphp_char_range.snd lexings in
      let temp3=List.filter (fun x->x<>Scphp_char_range.dummy_lexing) temp1
      and temp4=List.filter (fun x->x<>Scphp_char_range.dummy_lexing) temp2 in
      let u=Scphp_char_range.select_head temp3
      and v=Scphp_char_range.select_head temp4 in
      let cr=Scphp_char_range.make u v in
       (([],[],[],Scpositioned_php_token_list.empty),Some(Some(List.rev(graet),cr,l)))
  |(ret,wh)::da_ober2->
     (
       match Scphp_constructible_recognizer.recognize wh l with
       None->(([],[],[],Scpositioned_php_token_list.empty),Some(None))
       |Some(cr,peurrest)->
          let d=Scpositioned_php_token_list.length(l)-Scpositioned_php_token_list.length(peurrest) in
          let part=Scpositioned_php_token_list.big_head d l in
          let graet2=(if ret=Scglued_or_not.Not_retained_not_glued then graet else part::graet) in
          ((graet2,da_ober2,cr::lexings,peurrest),None)
     );;

 
let rec iterator_for_parsing x=
     let y=snd(x) in
     if y<>None 
     then Scoption.unpack y
     else iterator_for_parsing(pusher_for_parsing x);;

let parse (Trmt(trmt))=
  let f=(fun l->iterator_for_parsing (([],trmt,[],l),None) ) in
  (f: Scpositioned_php_token_list.t  list Scphp_parser.t);;

let eat s t=parse (of_string s) (Scphp_lexer.parse_string t);;


   
 
 end;;


module Scphp_class_modifier=struct (*

#use"Php_analizer/Php_syntax_types/php_class_modifier.ml";;

*)

type t=
    Abstract
   |Final
   |Usual;;
  
   
 end;;


module Scphp_script_includer=struct (*

#use"Php_analizer/Php_syntax_types/script_includer.ml";;

*)

type t=
    Include
   |Include_once
   |Require
   |Require_once;;
   
exception Not_a_script_includer of Scphp_token.t;;   
   
let from_lexeme lxm=
  if lxm=Scphp_token.kwd"include"      then Include      else
  if lxm=Scphp_token.kwd"include_once" then Include_once else   
  if lxm=Scphp_token.kwd"require"      then Require      else
  if lxm=Scphp_token.kwd"require_once" then Require_once else   
  raise(Not_a_script_includer(lxm));;
    
let from_luxume lxm=
  try(Some(from_lexeme lxm)) with _->None;; end;;


module Scbeaver_for_statement=struct (*

#use"Php_analizer/Beavers/beaver_for_statement.ml";;

*)

type plex=Scpositioned_php_token.t;;
type plexl=Scpositioned_php_token_list.t;;

type php_var=Scpositioned_php_token.t;;
type php_assign_op=Scpositioned_php_token.t;;
type php_class=Scpositioned_php_token.t;;
type php_static_method=Scpositioned_php_token.t;;
type php_class_property=Scpositioned_php_token.t;;
type php_fun_name=Scpositioned_php_token.t;;
type php_interface_name=Scpositioned_php_token.t;;
type php_trait_name=Scpositioned_php_token.t;;
type php_namespace=Scpositioned_php_token.t;;
type php_index=Scpositioned_php_token.t;;


type t=
    External_echo of string*Scphp_char_range.t
   |Comment of string*Scphp_char_range.t
   |Class_decl of Scphp_class_modifier.t*plexl*plexl*Scphp_char_range.t
   |Ivy of plexl*plexl*plexl*Scphp_char_range.t
   |Script_inclusion of Scphp_script_includer.t*plexl*Scphp_char_range.t
   |Assignment of php_var*php_assign_op*plexl*Scphp_char_range.t
   |Assignment_on_class_property of php_var*php_class_property*php_assign_op*plexl*Scphp_char_range.t
   |Static_assignment of php_var*php_assign_op*plexl*Scphp_char_range.t
   |Yuze_decl of plexl*Scphp_char_range.t
   |Static_method_call of php_class*php_static_method*plexl*Scphp_char_range.t
   |Nonstatic_method_call of plexl*php_static_method*plexl*Scphp_char_range.t
   |Class_property_call of plexl*php_class_property*Scphp_char_range.t
   |TryCatch of plexl*plexl*plexl*Scphp_char_range.t
   |Fun_def of php_fun_name*plexl*plexl*Scphp_char_range.t
   |Returning of plexl*Scphp_char_range.t
   |Interface_decl of php_interface_name*plexl*plexl*Scphp_char_range.t
   |Fun_call of bool*php_fun_name*plexl*Scphp_char_range.t
   |Namespace_def of php_namespace*Scphp_char_range.t
   |Appending of php_var*plexl*Scphp_char_range.t
   |Cell_assignment of php_var*php_index*plexl*Scphp_char_range.t
   |Trait_decl of php_trait_name*plexl*Scphp_char_range.t
   |Echo of plexl*Scphp_char_range.t
   |Exit of Scphp_char_range.t
   |WhileLoop of plexl*plexl*Scphp_char_range.t
   |ForeachLoop of plexl*plexl*Scphp_char_range.t
   |NamespaceBlock of (plex option)*plexl*Scphp_char_range.t
   |Declare of plexl*Scphp_char_range.t
   |Switch of plex*plexl*Scphp_char_range.t;;
   
let char_range=function
    External_echo(_,cr)->cr
   |Comment(_,cr)->cr
   |Class_decl(_,_,_,cr)->cr
   |Ivy(_,_,_,cr)->cr 
   |Script_inclusion(_,_,cr)->cr 
   |Assignment(_,_,_,cr)->cr
   |Assignment_on_class_property(_,_,_,_,cr)->cr
   |Static_assignment(_,_,_,cr)->cr
   |Yuze_decl(_,cr)->cr
   |Static_method_call(_,_,_,cr)->cr 
   |Nonstatic_method_call(_,_,_,cr)->cr 
   |Class_property_call(_,_,cr)->cr
   |TryCatch(_,_,_,cr)->cr
   |Fun_def(_,_,_,cr)->cr
   |Returning(_,cr)->cr
   |Interface_decl(_,_,_,cr)->cr
   |Fun_call(_,_,_,cr)->cr
   |Namespace_def(_,cr)->cr
   |Appending(_,_,cr)->cr
   |Cell_assignment(_,_,_,cr)->cr
   |Trait_decl(_,_,cr)->cr
   |Echo(_,cr)->cr
   |Exit(cr)->cr
   |WhileLoop(_,_,cr)->cr
   |ForeachLoop(_,_,cr)->cr
   |NamespaceBlock(_,_,cr)->cr
   |Declare(_,cr)->cr
   |Switch(_,_,cr)->cr;;
     
let dummy=External_echo("",Scphp_char_range.dummy);;     

type element={
    name               : string;
    content            : string;
    unadbriged_content : string;
    catalyser          : string;
    helper             : (plexl list -> Scphp_char_range.t -> t);
};;   
   
let element_cmp elt1 elt2=
   let step1=Scdictionary_order.dictionary_order elt1.name elt2.name in
   if step1<>Sctotal_ordering.Equal
   then step1
   else Sctidel.cmp
         (elt1.content,elt1.unadbriged_content,elt1.catalyser)     
         (elt2.content,elt2.unadbriged_content,elt2.catalyser);;
          
let element_order=(element_cmp: element Sctotal_ordering.t);; 


let classical_parser elt=
   let f=(fun l->
      let opt2=Sctermite.parse (Sctermite.of_string elt.unadbriged_content) l in
      if opt2=None then None else
      let (l2,cr2,peurrest)=Scoption.unpack opt2 in
      let catalyser_check=(
        if elt.catalyser=""
        then true
        else (Sctermite.parse (Sctermite.of_string elt.catalyser) peurrest)<>None
      ) in
      if catalyser_check
      then Some(elt.helper l2 cr2,cr2,peurrest)
      else None
   ) in
   (f : t Scphp_parser.t);;
   
let current_data_list=ref ([]:element list);;
let shortcuts_list=ref ([]:(string*string) list);;

let expand_element elt={
    name               = elt.name;
    content            = elt.content;
    unadbriged_content = Screplace_inside.replace_several_inside_string
      						(!shortcuts_list) elt.content;
    catalyser          = elt.catalyser;
    helper             = elt.helper;
};;   

let add_data a b c d=
   let old_version=Scordered.safe_set element_order (!current_data_list) in
   let elt={
    name               = a;
    content            = b;
    unadbriged_content = Screplace_inside.replace_several_inside_string
      						(!shortcuts_list) b;
    catalyser          = c;
    helper             = d;
   }  in
   let new_set=Scordered.insert element_order elt old_version in
   let new_version=Scordered.forget_order new_set in 
   (current_data_list:=new_version);;
    
let update_data_list ()=
   let new_list=Scimage.image expand_element (!current_data_list) in
   current_data_list:=new_list;;    
    
let add_shortcut x y=
   (
     shortcuts_list:=(x,y)::(!shortcuts_list);
     update_data_list ()
   );;    
    
let current_main_parser=
  (
    fun l->Scoption.find_and_stop 
    (fun tr->classical_parser tr l) (!current_data_list)
  );;

let display_data ()=
   let m1=snd(Scmax.maximize_it(fun elt->String.length(elt.name) 
   ) (!current_data_list)) in  
   let temp1=Scimage.image (fun 
      elt->
         let d=m1-(String.length(elt.name)) in
         let filler=String.make d ' ' in
         "\""^elt.name^"\""^filler^"   ,   \""^elt.content^"\""
   )(!current_data_list) in
   let temp2="\n\n\n"^(String.concat "\n" temp1)^"\n\n\n" in
   print_string temp2;;
   
   

let helper_for_byref_append l1 cr=
   let a=Scpositioned_php_token_list.hd(List.hd l1) in
   Appending(a,List.nth l1 1,cr);;

add_data 
	"append_byref"
	"variable [ ] = ##( & _l_no_semicolon _r*_ )## ;"
	""
	helper_for_byref_append
	;;
   
let helper_for_assignment l1 cr=
  let a=Scpositioned_php_token_list.hd(List.nth l1 0) 
  and b=Scpositioned_php_token_list.hd(List.nth l1 1)  in
  Assignment(a,b,List.nth l1 2,cr);;   

add_data 
	"assign1"
	"variable assign ##( () ?  _l_ id _u_ sqs _rd_  :  _l_ id _u_ variable _rd_  )## ;"
	""
	helper_for_assignment
	;;

add_data 
	"assign2"
	"variable assign  ##( sqs . id . dqs . variable -> id () . dqs . variable -> id () . dqs )## ;"
	""
	helper_for_assignment
	;;  

add_data 
	"assign_byref"
	"variable assign ##( & _l_loose= _r*_ )## ;"
	""
	helper_for_assignment;;
	
let helper_for_servant_assign l1 cr=
   let tf=(fun j->Scpositioned_php_token_list.hd(List.nth l1 j)) in
   Assignment_on_class_property(tf 0,tf 1,tf 2,List.nth l1 3,cr);;

add_data 
	"assign_on_servant"
	"variable -> id_or_var ##( assign )## _l_ loose= _r*_ ;"
	""
	helper_for_servant_assign;;

add_data 
	"assign_on_static"
	"id :: id_or_var ##( assign )## _l_ loose= _r*_ ;"
	""
	helper_for_servant_assign
	;;

add_data 
	"assign_to_cell"
	"variable assign ##( variable [ sqs ] )## ;"
	""
	helper_for_assignment
	;;
	
	

let assignables=
[
  "coerce           id ()";
  "nmspc            _l_ :: id _r?_ _l_ () _r?_";
  "id ::            id ()";
  "id () ?          _l_ no_ternary _r+_ : no_semicolon";
  "id () .          sqs";
  "id ()            ";
  "heredoc ";
  "include_like     _l_ loose= _r*_ ";
  "integer          ";
  "new id           ()";
  "new nmspc        ()";
  "sqs .            variable . sqs";
  "sqs";
  "variable .       sqs";
  "variable =       sqs";
  "variable ->      id _l_ () _r?_ _l_ -> id _l_ () _r?_ _r*_";
  "variable +       _l_ loose= _r*_ ";
  "variable";
  "@                id ()";
];;


let assignable=" _l_ "^(String.concat " _u_ " assignables)^" _rd_";;


add_shortcut "assignable" assignable;;


add_data 
  "assign_to_simple"
  "variable ##( assign )## assignable ;"
  ""
  helper_for_assignment
  ;;

add_data 
	"assign_to_terna"
	"variable ##( assign )## ##( () ? : new nmspc () )## ;"
	""
	helper_for_assignment
	;;

let helper_for_cell_assign l1 cr=
   let a=Scpositioned_php_token_list.hd(List.hd l1) in
   Cell_assignment(a,Scpositioned_php_token_list.hd(List.nth l1 1),List.nth l1 2,cr);;


add_data 
	"cell_assign"
	"variable [ ##( int_or_string_or_var )## ] = _l_loose= _r*_ ;"
	""
	helper_for_cell_assign
	;;

add_data 
	"cell_assign_byref"
	"variable [  ##( int_or_string_or_var )##  ]  =  ##( & _l_loose= _r*_ )## ;"
	""
	helper_for_cell_assign
	;;


let helper_for_abstract_class l1 cr=
   Class_decl(Scphp_class_modifier.Abstract,List.nth l1 0,List.nth l1 1,cr);;
   
add_data
  "class_abstract"
  "abstract class _l_ no_left_brace _r*_ {}"
  ""
  helper_for_abstract_class
  ;;   
  
let helper_for_final_class l1 cr=
   Class_decl(Scphp_class_modifier.Final,List.nth l1 0,List.nth l1 1,cr);;
   
add_data 
   "class_final"
   "final class _l_ no_left_brace _r*_ {}"
   ""
   helper_for_final_class;;   
  
let helper_for_usual_class l1 cr=
   Class_decl(Scphp_class_modifier.Usual,List.nth l1 0,List.nth l1 1,cr);;
   
add_data
  "class_usual"
  "class _l_ no_left_brace _r*_ {}"
  ""
  helper_for_usual_class
  ;;   

let helper_for_decl l1 cr=Declare(List.hd l1,cr);;

add_data 
	"decl"
	"declare () ;"
	""
	helper_for_decl
	;;


let helper_for_echo l1 cr=Echo(List.hd l1,cr);;


add_data 
	"echo1"
	"echo _l_ no_semicolon _r*_ ;"
	""
	helper_for_echo
	;;

add_data 
	"echo2"
	"echo variable"
	"ext"
	helper_for_echo
	;;
	
let helper_for_exit l1 cr=Exit(cr);;

add_data 
	"exit"
	"exit ;"
	""
	helper_for_exit
	;;

let helper_for_foreach1 l1 cr=ForeachLoop(List.nth l1 0,List.nth l1 1,cr);;

add_data 
	"foreach1"
	"foreach () {}"
	""
	helper_for_foreach1
	;;





let helper_for_foreach2 l1 cr=ForeachLoop(List.nth l1 0,List.nth l1 1,cr);;

add_data 
	"foreach2"
	"foreach () : ##( _l_no_breach _r*_ )## endforeach ;"
	""
	helper_for_foreach2
	;;

let helper_for_fun_call l1 cr=Fun_call(
not(Scpositioned_php_token_list.is_empty(List.hd(l1))),
Scpositioned_php_token_list.hd(List.nth l1 1),List.nth l1 2,cr);;

add_data 
	"fun_call"
	"_l_ @ _r?_ ##(id)## () ;"
	""
	helper_for_fun_call
	;;

let helper_for_fun_def l1 cr=
   let a=Scpositioned_php_token_list.hd(List.hd l1) in
   Fun_def(a,List.nth l1 1,List.nth l1 2,cr);;

add_data 
	"fun_def"
	"function id () {}"
	""
	helper_for_fun_def
	;;



let helper_for_fun_returning l1 cr=Returning(List.hd(l1),cr);;

add_data 
	"fun_returning"
	"return ##( function () {} )## ;"
	""
	helper_for_fun_returning
	;;


let helper_for_include_like l1 cr=
      let a=Scpositioned_php_token_list.hd(List.hd l1) in
      let fa=Scpositioned_php_token.fst a in
      let na=Scphp_script_includer.from_lexeme fa in
      Script_inclusion(na,List.nth l1 1,cr);; 

add_data 
   "include_like"
   "include_like _l_stringy _r*_ ;"
   ""
   helper_for_include_like
   ;; 

let helper_for_interface_decl l1 cr=
     let temp1=List.hd l1 in
     Interface_decl(Scpositioned_php_token_list.hd temp1,
       Scpositioned_php_token_list.tl temp1,List.nth l1 1,cr);;

add_data 
	"interface_decl"
	"interface _l_ no_left_brace _r*_ {}"
	""
	helper_for_interface_decl
	;;



let helper_for_ivy l1 cr=
  Ivy(List.nth l1 0,List.nth l1 1,List.nth l1 2,cr);;

add_data 
   "ivy1"
   "if () ##( {} )## ##( _l_else if () {} _r*__l_else {} _r?_ )##"
   ""
   helper_for_ivy
   ;;
     

let helper_for_ivy2 l1 cr=Ivy(List.nth l1 0,List.nth l1 1,Scpositioned_php_token_list.empty,cr);; 

add_data 
  "ivy2"
  "if () ##( exit ; )##"
  ""
  helper_for_ivy2
  ;;    

add_data 
  "ivy3"
  "if () ##( {} )## else ##(if () {})## "
  ""
  helper_for_ivy
  ;;    


add_data 
   "ivy4"
   "if () : ##( _l_no_ivies _r*_ )## endif ;"
   ""
   helper_for_ivy2
   ;; 

add_data 
  "ivy5"
  "if () : ##( _l_no_ivies _r*_ if () : _l_no_ivies _r*_ endif _l_no_ivies _r*_ )## endif ;"
  ""
  helper_for_ivy2
  ;; 
  


add_data 
  "ivy6"
  "if () : ##( _l_no_ivies _r*_ if () : _l_no_ivies _r*_ else : _l_no_ivies _r*_ endif ; _l_no_ivies _r*_ )## endif ;"
  ""
  helper_for_ivy2
  ;; 
  
let helper_for_meth_call_on_snake l1 cr=
  Nonstatic_method_call(List.nth l1 0,Scpositioned_php_token_list.hd(List.nth l1 1),List.nth l1 2,cr);;


 add_data 
	"meth_call_on_snake"
	"##( id :: id () _l_-> id () _r~_ )## -> id () ;"
	""
	helper_for_meth_call_on_snake
	;;

 
let helper_for_nmspc_block l1 cr=
   let optionized=(fun x->if Scpositioned_php_token_list.is_empty x
    					  then None 
    					  else Some(Scpositioned_php_token_list.hd x) )(List.hd l1) in
   NamespaceBlock(optionized,List.nth l1 1,cr);;

add_data 
	"nmspc_block"
	"namespace _l_ _l_ id_u_nmspc _rd_ _r?_ {}"
	""
	helper_for_nmspc_block
	;;


let helper_for_nmspc_definition l1 cr=
   let a=Scpositioned_php_token_list.hd(List.hd l1) in
   Namespace_def(a,cr);;

add_data 
	"nmspc_definition"
	"namespace nmspc ;"
	""
	helper_for_nmspc_definition
	;;



let helper_for_nmspc_lonely l1 cr=
   let a=Scpositioned_php_token_list.hd(List.hd l1) in
   Namespace_def(a,cr);;

add_data 
	"nmspc_lonely"
	"namespace id ;"
	""
	helper_for_nmspc_lonely
	;;

let helper_for_returning l1 cr=Returning(List.hd(l1),cr);;

add_data 
	"returning"
	"return _l_ no_semicolon _r*_ ;"
	""
	helper_for_returning
	;;






	
exception Singleton_exn;; 
   
let helper_for_singleton l1 cr=
     let tok=Scpositioned_php_token.fst(Scpositioned_php_token_list.hd(List.hd l1)) in
     if Scphp_token.form tok=Scphp_projected_token.External_echo
     then External_echo(Scphp_token.content tok,cr)
     else raise(Singleton_exn);;
   
add_data 
  "singleton"
  "ext"
  ""
  helper_for_singleton
  ;;   


let helper_for_snake_call l1 cr=
  let temp1=Scpositioned_php_token_list.concat (List.hd l1) (List.nth l1 1) 
  and last_leader=Scpositioned_php_token_list.hd(List.nth l1 2) in
  if Scpositioned_php_token_list.is_empty (List.nth l1 3)
  then Class_property_call(temp1,last_leader,cr)
  else Nonstatic_method_call(temp1,last_leader,List.nth l1 3,cr);;

add_data 
	"snake_call"
	"id_or_var _l_ -> id_or_var _l_() _r?_  _r~_ -> id_or_var _l_() _r?_ ;"
	""
	helper_for_snake_call
	;;




let helper_for_static_assignment l1 cr=
  let a=Scpositioned_php_token_list.hd(List.nth l1 0) 
  and b=Scpositioned_php_token_list.hd(List.nth l1 1)  in
  Static_assignment(a,b,List.nth l1 2,cr);; 

add_data 
  "static_assignment"
  "static variable assign ##( id () )## ;"
  ""
  helper_for_static_assignment
  ;;



let helper_for_static_meth l1 cr=
   let a1=Scpositioned_php_token_list.hd(List.hd l1) 
   and a2=Scpositioned_php_token_list.hd(List.nth l1 1) in
   Static_method_call(a1,a2,List.nth l1 2,cr);;

add_data 
	"static_meth"
	"id :: id_or_var () ;"
	""
	helper_for_static_meth
	;;



let helper_for_statmeth_call l1 cr=
  Static_method_call(Scpositioned_php_token_list.hd(List.nth l1 0),
  Scpositioned_php_token_list.hd(List.nth l1 1),List.nth l1 2,cr);;

add_data 
	"statmeth_call"
	"_l_ id_u_nmspc _rd_ :: ##(id)## () ;"
	""
	helper_for_statmeth_call
	;;


let helper_for_switch l1 cr=Switch(Scpositioned_php_token_list.hd(List.hd l1),List.nth l1 1,cr);;

add_data 
	"switch"
	"switch () {}"
	""
	helper_for_switch
	;;

let helper_for_trait_decl l1 cr=
  let temp1=List.hd l1 in
  Trait_decl(Scpositioned_php_token_list.hd temp1,List.nth l1 1,cr);;

add_data 
	"trait_decl"
	"trait id {}"
	""
	helper_for_trait_decl
	;;



let helper_for_trycatch l1 cr=TryCatch(List.nth l1 0,List.nth l1 1,List.nth l1 2,cr);;

add_data 
	"trycatch"
	"try {} catch () {}"
	""
	helper_for_trycatch
	;;



let helper_for_while_loop l1 cr=WhileLoop(List.nth l1 0,List.nth l1 1,cr);;

add_data 
	"while_loop"
	"while () {}"
	""
	helper_for_while_loop
	;;


let helper_for_yuze_decl l1 cr=Yuze_decl(List.hd l1,cr);;

add_data 
	"yuze_decl"
	"use _l_no_semicolon _r*_ ;"
	""
	helper_for_yuze_decl
	;;



let parser=(current_main_parser:t Scphp_parser.t);; 


 
 



 end;;




let example1=
  [((((Some Scphp_keyword.T_IF, None, None), Scphp_projected_token.Int), ""),
  ({Lexing.pos_fname =
     "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
    pos_lnum = 4; pos_bol = 274; pos_cnum = 275},
   {Lexing.pos_fname =
     "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
    pos_lnum = 4; pos_bol = 274; pos_cnum = 276}));
  ((((None, Some Scphp_punctuator.T_LPARENTHESIS, None), Scphp_projected_token.Int),
   ""),
  ({Lexing.pos_fname =
     "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
    pos_lnum = 4; pos_bol = 274; pos_cnum = 278},
   {Lexing.pos_fname =
     "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
    pos_lnum = 4; pos_bol = 274; pos_cnum = 278}));
  ((((None, None, Some Scphp_operator.T_EXCLAMATION), Scphp_projected_token.Int),
   ""),
  ({Lexing.pos_fname =
     "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
    pos_lnum = 4; pos_bol = 274; pos_cnum = 279},
   {Lexing.pos_fname =
     "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
    pos_lnum = 4; pos_bol = 274; pos_cnum = 279}));
  ((((None, None, None), Scphp_projected_token.Ident), "isset"),
  ({Lexing.pos_fname =
     "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
    pos_lnum = 4; pos_bol = 274; pos_cnum = 280},
   {Lexing.pos_fname =
     "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
    pos_lnum = 4; pos_bol = 274; pos_cnum = 284}));
  ((((None, Some Scphp_punctuator.T_LPARENTHESIS, None), Scphp_projected_token.Int),
   ""),
  ({Lexing.pos_fname =
     "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
    pos_lnum = 4; pos_bol = 274; pos_cnum = 285},
   {Lexing.pos_fname =
     "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
    pos_lnum = 4; pos_bol = 274; pos_cnum = 285}));
  ((((None, None, None), Scphp_projected_token.Variable), "$_SERVER"),
  ({Lexing.pos_fname =
     "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
    pos_lnum = 4; pos_bol = 274; pos_cnum = 286},
   {Lexing.pos_fname =
     "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
    pos_lnum = 4; pos_bol = 274; pos_cnum = 293}));
  ((((None, None, Some Scphp_operator.T_LBRACKET), Scphp_projected_token.Int), ""),
  ({Lexing.pos_fname =
     "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
    pos_lnum = 4; pos_bol = 274; pos_cnum = 294},
   {Lexing.pos_fname =
     "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
    pos_lnum = 4; pos_bol = 274; pos_cnum = 294}));
  ((((None, None, None), Scphp_projected_token.Single_quoted), "HTTP_HOST"),
  ({Lexing.pos_fname =
     "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
    pos_lnum = 4; pos_bol = 274; pos_cnum = 295},
   {Lexing.pos_fname =
     "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
    pos_lnum = 4; pos_bol = 274; pos_cnum = 305}));
  ((((None, None, Some Scphp_operator.T_RBRACKET), Scphp_projected_token.Int), ""),
  ({Lexing.pos_fname =
     "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
    pos_lnum = 4; pos_bol = 274; pos_cnum = 306},
   {Lexing.pos_fname =
     "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
    pos_lnum = 4; pos_bol = 274; pos_cnum = 306}));
  ((((None, Some Scphp_punctuator.T_RPARENTHESIS, None), Scphp_projected_token.Int),
   ""),
  ({Lexing.pos_fname =
     "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
    pos_lnum = 4; pos_bol = 274; pos_cnum = 307},
   {Lexing.pos_fname =
     "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
    pos_lnum = 4; pos_bol = 274; pos_cnum = 307}))]
  @
  [((((None, Some Scphp_punctuator.T_RPARENTHESIS, None),
  Scphp_projected_token.Int),
  ""),
  ({Lexing.pos_fname =
   "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
  pos_lnum = 4; pos_bol = 274; pos_cnum = 308},
  {Lexing.pos_fname =
   "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
  pos_lnum = 4; pos_bol = 274; pos_cnum = 308}));
  ((((None, Some Scphp_punctuator.T_LBRACE, None), Scphp_projected_token.Int), ""),
  ({Lexing.pos_fname =
   "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
  pos_lnum = 4; pos_bol = 274; pos_cnum = 310},
  {Lexing.pos_fname =
   "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
  pos_lnum = 4; pos_bol = 274; pos_cnum = 310}));
  ((((Some Scphp_keyword.T_EXIT, None, None), Scphp_projected_token.Int), ""),
  ({Lexing.pos_fname =
   "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
  pos_lnum = 5; pos_bol = 311; pos_cnum = 316},
  {Lexing.pos_fname =
   "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
  pos_lnum = 5; pos_bol = 311; pos_cnum = 319}));
  ((((None, Some Scphp_punctuator.T_LPARENTHESIS, None),
  Scphp_projected_token.Int),
  ""),
  ({Lexing.pos_fname =
   "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
  pos_lnum = 5; pos_bol = 311; pos_cnum = 320},
  {Lexing.pos_fname =
   "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
  pos_lnum = 5; pos_bol = 311; pos_cnum = 320}));
  ((((None, None, None), Scphp_projected_token.Single_quoted),
  "This script cannot be run from the CLI. Run it from a browser."),
  ({Lexing.pos_fname =
   "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
  pos_lnum = 5; pos_bol = 311; pos_cnum = 321},
  {Lexing.pos_fname =
   "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
  pos_lnum = 5; pos_bol = 311; pos_cnum = 384}));
  ((((None, Some Scphp_punctuator.T_RPARENTHESIS, None),
  Scphp_projected_token.Int),
  ""),
  ({Lexing.pos_fname =
   "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
  pos_lnum = 5; pos_bol = 311; pos_cnum = 385},
  {Lexing.pos_fname =
   "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
  pos_lnum = 5; pos_bol = 311; pos_cnum = 385}));
  ((((None, Some Scphp_punctuator.T_SEMICOLON, None), Scphp_projected_token.Int),
  ""),
  ({Lexing.pos_fname =
   "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
  pos_lnum = 5; pos_bol = 311; pos_cnum = 386},
  {Lexing.pos_fname =
   "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
  pos_lnum = 5; pos_bol = 311; pos_cnum = 386}));
  ((((None, Some Scphp_punctuator.T_RBRACE, None), Scphp_projected_token.Int), ""),
  ({Lexing.pos_fname =
   "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
  pos_lnum = 6; pos_bol = 387; pos_cnum = 388},
  {Lexing.pos_fname =
   "/Users/roparzhhemon/Documents/Sites/Symblog/Symblogproject/Web/config.php";
  pos_lnum = 6; pos_bol = 387; pos_cnum = 388}))];;
  
  
let example2=Screcreating_tools.decode_postokenlist example1;;
  
let prsr=Scbeaver_for_statement.parser;;


let answer=Scbeaver_for_statement.parser example2;;


