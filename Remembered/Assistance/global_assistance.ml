(*

#load "unix.cma";;
#load "str.cma";;

#use"/Users/ewandelanoy/Documents/OCaml/Ordinary/Remembered/Assistance/global_assistance.ml";;

This file is supposed to be self-contained and therefore usable when in great
compiler trouble.
The help_me function defined at the end of this file has type
unit -> unit. apply_assistance dirname will recompile from scratch (and in the correct order) 
all the mlfiles contained in "directory_to_be_cured" dirname (and its subdirectories) except for
those in the Remembered and Forgotten subdirectories.


*)

let directory_to_be_cured="/Users/ewandelanoy/Documents/OCaml/Ordinary";;

module Assistance=struct

module Total_ordering=struct


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






module Image=struct

(*

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






module Basic=struct

let delta_list l=
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






module Listennou=struct


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
|a::peurrest->tempf(peurrest,graet@(Image.image(function y->a::y)(graet)))
) in
tempf(List.rev(l),[[]]);;

let big_head=Basic.big_head;; 

let big_tail=Basic.big_tail;;


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
   
   
   

let hi=List.length;;
let rev=List.rev;;




end;;






module Ordered=struct

(*
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

let kreskus (kenver:'a Total_ordering.t) l=
  if List.length(l)<2 then true else
  let rec tempf=(function
  (a,da_ober)->match da_ober with
   []->true
   |b::peurrest->if (kenver(a)(b)<>Total_ordering.Greater)
                 then tempf(b,peurrest)
                 else false
  ) in
  tempf(List.hd l,List.tl l);;
  
let kreskus_strizh (kenver:'a Total_ordering.t) l=
  if List.length(l)<2 then true else
  let rec tempf=(function
  (a,da_ober)->match da_ober with
   []->true
   |b::peurrest->if (kenver(a)(b)=Total_ordering.Lower)
                 then tempf(b,peurrest)
                 else false
  ) in
  tempf(List.hd l,List.tl l);;

let rec elfenn (kenver:'a Total_ordering.t) x ol=
   let rec elfenn0=(function
    []->false
    |a::peurrest->match kenver(x)(a) with
       Total_ordering.Lower->false
       |Total_ordering.Equal->true
       |Total_ordering.Greater->elfenn0 peurrest
   )  in
   elfenn0 (forget_order ol);;
		
            
let teuzin (kenver:'a Total_ordering.t) ox oy=
let rec teuzin0=
(function (u,v,accu)->
if u=[] then (List.rev_append(accu)(v)) else
if v=[] then (List.rev_append(accu)(u)) else
let xu=List.hd(u) and yu=List.tl(u) 
and xv=List.hd(v) and yv=List.tl(v) in
match kenver(xu)(xv) with
   Total_ordering.Lower->teuzin0(yu,v,xu::accu)
   |Total_ordering.Equal->teuzin0(yu,yv,xu::accu)
   |Total_ordering.Greater->teuzin0(u,yv,xv::accu)

) in
unsafe_set(teuzin0(forget_order ox,forget_order oy,[]));;

let rec diforchan (kenver:'a Total_ordering.t) x=
  if List.length(x)<2
  then unsafe_set(x)
  else let temp1=Listennou.didrochan(x) in
       let y1=diforchan(kenver)(fst temp1)
       and y2=diforchan(kenver)(snd temp1) in
       teuzin kenver y1 y2;;
  
  
  let lemel (kenver:'a Total_ordering.t) ox oy=
let rec lemel0=
(function (u,v,accu)->
if u=[] then (List.rev(accu)) else
if v=[] then (List.rev_append(accu)(u)) else
let xu=List.hd(u) and yu=List.tl(u) 
and xv=List.hd(v) and yv=List.tl(v) in
match kenver(xu)(xv) with
   Total_ordering.Lower->lemel0(yu,v,xu::accu)
   |Total_ordering.Equal->lemel0(yu,yv,accu)
   |Total_ordering.Greater->lemel0(u,yv,accu)

) in
unsafe_set(lemel0(forget_order ox,forget_order oy,[]));;

let kengeij (kenver:'a Total_ordering.t) ox oy=
let rec kengeij0=
(function (u,v,accu)->
if u=[] then (List.rev(accu)) else
if v=[] then (List.rev(accu)) else
let xu=List.hd(u) and yu=List.tl(u) 
and xv=List.hd(v) and yv=List.tl(v) in
match kenver(xu)(xv) with
   Total_ordering.Lower->kengeij0(yu,v,accu)
   |Total_ordering.Equal->kengeij0(yu,yv,xu::accu)
   |Total_ordering.Greater->kengeij0(u,yv,accu)

) in
unsafe_set(kengeij0(forget_order ox,forget_order oy,[]));;

let kengeij_goullo (kenver:'a Total_ordering.t) ox oy=
let rec kengeij_goullo0=
(function (u,v)->
if (u=[])||(v=[]) then true else
let xu=List.hd(u) and yu=List.tl(u) 
and xv=List.hd(v) and yv=List.tl(v) in
match kenver(xu)(xv) with
   Total_ordering.Lower->kengeij_goullo0(yu,v)
   |Total_ordering.Equal->false
   |Total_ordering.Greater->kengeij_goullo0(u,yv)
) in
kengeij_goullo0(forget_order ox,forget_order oy);;


let ental (kenver:'a Total_ordering.t) ox oy=
let rec ental0=
(function (u,v)->
if u=[] then true else
if v=[] then false else
let xu=List.hd(u) and yu=List.tl(u) 
and xv=List.hd(v) and yv=List.tl(v) in
match kenver(xu)(xv) with
   Total_ordering.Lower->false
   |Total_ordering.Equal->ental0(yu,yv)
   |Total_ordering.Greater->ental0(u,yv)
) in
ental0(forget_order ox,forget_order oy);;

let min=((fun kenver x->match x with
  []->failwith("The empty set has no min")
  |a::b->
    let rec tempf=(fun 
     (trecher,da_ober)->match da_ober with
      []->trecher
      |c::peurrest->
        if kenver(c)(trecher)=Total_ordering.Lower
        then tempf(c,peurrest)
        else tempf(trecher,peurrest)
    ) in
    tempf(a,b)):> ('a Total_ordering.t -> 'a list->'a));;

let max=((fun kenver x->match x with
  []->failwith("The empty set has no max")
  |a::b->
    let rec tempf=(fun 
     (trecher,da_ober)->match da_ober with
      []->trecher
      |c::peurrest->
        if kenver(c)(trecher)=Total_ordering.Greater
        then tempf(c,peurrest)
        else tempf(trecher,peurrest)
    ) in
    tempf(a,b)):> ('a Total_ordering.t -> 'a list->'a));;

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
let image f ox=Image.image(f)(forget_order ox);;
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






module Tidel=struct

(* file created using the "write_contents_of_ordered_list_module" function *)
(* in Creators/ordered_list_creator.ml *)

type 'a set='a Ordered.old_set;;
let lt x y=x<y;;
let cmp=Total_ordering.standard;;
let unsafe_set=(Ordered.unsafe_set:>('a list-> 'a set));;
let forget_order=(Ordered.forget_order:>('a set->'a list));;

let kreskus_strizh x=Ordered.kreskus_strizh cmp x;;
let kreskus x=Ordered.kreskus cmp x;;

let elfenn=((fun a ox->Ordered.elfenn cmp a ox):>('a->'a set->bool));;
let teuzin=((fun ox oy->Ordered.teuzin cmp ox oy):>( 'a set->'a set->'a set));;
let diforchan=((fun x->Ordered.diforchan cmp x):>('a list->'a set));;
let lemel=((fun ox oy->Ordered.lemel cmp ox oy):>('a set->'a set->'a set));;
let ental=((fun ox oy->Ordered.ental cmp ox oy):>('a set->'a set->bool));;
let kengeij=((fun ox oy->Ordered.kengeij cmp ox oy):>'a set->'a set->'a set);;
let kengeij_goullo=((fun ox oy->Ordered.kengeij_goullo cmp ox oy):>'a set->'a set->bool);;
let min=((fun x->Ordered.min cmp x):>'a list->'a);;
let max=((fun x->Ordered.max cmp x):>'a list->'a);;

let hd ox=List.hd(forget_order ox);;
let image f ox=Image.image f (forget_order ox);;
let rev_map f ox=Image.image f (forget_order ox);;
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
 ((fun x->Ordered.expand_boolean_algebra cmp x):>('a set list->('a set list)));; 
 
 


end;;






module Option=struct



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






module Ennig=struct

 
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






module Sliced_string=struct

(*

#use"sliced_string.ml";;

A string is sliced when \n's are introduced at suitable places
in it, so that the display will have shorter lines and will 
therefore be more readable.

The type is abstract (see the mli) because displaying a very long of strings is
not very useful in the toplevel.

*)

type t=Sl of string list;;

let to_string_list (Sl l)=l;;
let of_string_list l=Sl l;;

let concat_two (Sl l1) (Sl l2)=Sl(l1@l2);;

let concat=function
[]->Sl[]
|a::peurrest->List.fold_left concat_two a peurrest;;
 
let print (Sl l)=String.concat "\n" l;;


 let itemize (shower:'a->string) l=
  let n=List.length(l) in
  let d=String.length(string_of_int(n)) in
  let temp1=Image.image(shower)(l) in
  let temp2=Ennig.index_everything(temp1) in
  let tempf=(function (j,v)->
     let s1=string_of_int(j) in
     let dd=d-String.length(s1) in
     let s2=(function ()->if dd>0 then String.make(dd)(' ') else "")() in
     s2^s1^"/  "^v
  ) in
  let temp3=Image.image tempf temp2 in
  of_string_list temp3;;
 
let max_line_length_ref=ref(70);;

type inner_separator=string;;
  
 let make_aggregates (sep:inner_separator)=function
 []->Sl[]
 |a::b->
  let rec tempf=(function
   (accu,breman,pouez,da_ober)->match da_ober with
    []->List.rev((List.rev breman)::accu)
	|y::peurrest->
	     let py=String.length y in
	     let pouez_nevez=pouez+py in
	     if (pouez_nevez>(!max_line_length_ref))
		 then tempf((List.rev breman)::accu,[y],py,peurrest)
		 else tempf(accu,y::breman,pouez_nevez,peurrest)
   ) in
   let temp1=tempf([],[a],String.length a,b) in
   Sl(Image.image (String.concat sep) temp1);;


end;;






module Three_parts=struct

(*

#use"three_parts.ml";;

*)




let generic=function
[]->[]
|u::v->
let rec tempf=
(function
((graet,x,da_ober),accu)->
let accu2=(graet,x,da_ober)::accu in
match da_ober with
[]->accu2
|a::b->tempf((x::graet,a,b),accu2)
) in
tempf(([],u,v),[]);;

let complemented_points l=List.rev_map(function (kleiz,x,dehou)->
(x,List.rev_append(kleiz)(dehou)))
(generic l);;

let beheaded_tails l=List.rev_map (function (kleiz,x,dehou)->(x,dehou) )(generic l);;

let select_center_element_and_reverse_left f l=
  let rec tempf=(fun (graet,da_ober)->match da_ober with
  []->(graet,None,[])
  |x::peurrest->if f x 
                then (graet,Some(x),peurrest)
                else tempf(x::graet,peurrest)
  ) in
  tempf([],l);;

let select_center_element f l=
  let (temp1,opt,after)=select_center_element_and_reverse_left f l in 
  (List.rev temp1,opt,after);;

let select_left_interval f l=
  (* note that the "interval" is returned is reverse form *)
  let rec tempf=(fun (graet,da_ober)->match da_ober with
  []->(graet,[])
  |x::peurrest->if f x 
                then tempf(x::graet,peurrest) 
                else (graet,da_ober)
  ) in
  tempf([],l);;

let select_center_interval f l=
  let rec tempf=(fun (graet,da_ober)->match da_ober with
  []->(List.rev graet,[],[])
  |x::peurrest->if f x 
                then let (temp1,temp2)=select_left_interval f da_ober in
                     (List.rev graet,List.rev(temp1),temp2)
                else tempf(x::graet,peurrest)
  ) in
  tempf([],l);;


let replace_in_list replacee replacer l=
  let (temp1,opt,temp2)=select_center_element (fun t->t=replacee) l in
  if opt=None then l else List.rev_append temp1 (replacer@temp2);;
   


end;;






module Cartesian=struct



let product a b=
if (a=[])||(b=[]) then [] else
let rec sub_f=(function
(accu,variable_a,constant_b)->match variable_a with
[]->List.rev(accu)
|u::v->sub_f(List.rev_append(List.rev(List.rev_map(function t->(u,t))(constant_b)))
(accu),v,constant_b)
) in
sub_f([],a,b);;

let square x=product x x;;

let tproduct a b c=List.rev_map(function ((x,y),z)->(x,y,z))
(List.rev(product(product(a)(b))(c)));;

let pproduct a b c d=List.rev_map(function ((x,y,z),t)->(x,y,z,t))
(List.rev(product(tproduct a b c)(d)));;

let cube x=tproduct x x x;;

let fourth_power x=pproduct x x x x;;

let general_product x=
let rec sub_f=(function
([],accu)->accu
|(a::b,accu)->sub_f(b,List.rev_map(function (x,l)->x::l)(List.rev(product(a)(accu)))))
in
sub_f(List.rev(x),[[]]);;

let power x n=general_product (Ennig.doyle (fun j->x) 1 n);;


end;;






module Memoized=struct


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






module No_slashes=struct

(*

#use"no_slashes.ml";;

*)

type t=NS of string;;

exception Slash_at of string*int;;

let to_string(NS s)=s;;

let of_string s=
  let n=String.length s in
  let rec tempf=(fun i->
  if i>n
  then NS s
  else if (String.get s (i-1))='/'
       then raise(Slash_at(s,i))
       else tempf(i+1)
  ) in
  tempf 1;;
  


end;;






module Father_and_son=struct

(*

#use"father_and_son.ml";;

The son is invasive by default.

*)

let father_and_son s c=
   let i=(try String.rindex(s)(c) with _->(-1)) in
   if i<0
   then ("",s)
   else (String.sub s 0 i,String.sub s (i+1) ((String.length s)-i-1) );;

let father s c=fst(father_and_son s c);;
let son s c=snd(father_and_son s c);;

let invasive_father s c=
   let i=(try String.rindex(s)(c) with _->(-1)) in
   if i<0
   then s
   else String.sub s 0 i;;
    
  

end;;






module Tools_for_absolute_path=struct

(*

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
  
 let of_string s=
  let s0=absolute_path_of_string0(s) in
  if Sys.file_exists(s0)
  then if s0="/" then s0 else
       let s1=remove_trailing_slash s0 in
       if Sys.is_directory s1
       then s1^"/"
       else s1
  else raise(Inexistent_file(s));;
  
 

end;;






module Directory_name=struct

(*

Directories name, with the trailing slash removed.

#use"directory_name.ml";;

*)

type t=D of string;;

let unsafe_from_string s=D s;;

exception Non_directory of string;;

let of_string s=
  let temp1=Tools_for_absolute_path.of_string s in
  if Sys.is_directory temp1
  then D(Tools_for_absolute_path.remove_trailing_slash temp1)
  else raise(Non_directory(s));;

let to_string (D s)=s^"/";;

exception Nonexistent_file of string;;

module Private=struct
let check_filename t=
  if Sys.file_exists t
  then t
  else raise(Nonexistent_file(t));;
end;;

let join (D s) w=Private.check_filename(s^"/"^w);;

let force_join (D s) w=
   let t=s^"/"^w in
   if Sys.file_exists t
   then t
   else let _=Sys.command("touch "^t) in
        t;;

exception Cut_error of t*string;;

let cut_beginning (D s) w=
   let ns=String.length(s)
   and nw=String.length(w) in
   if (ns+1)>nw then raise(Cut_error(D s,w)) else
   if (String.sub w 0 (ns+1))<>(s^"/") then raise(Cut_error(D s,w)) else
   String.sub w (ns+1) (nw-ns-1);;
   


let ocaml_name (D s)="Directory_name"^"."^"unsafe_from_string(\""^s^"\")";;

end;;






module Absolute_path=struct

(*

#use"absolute_path.ml";;

*)

type t=AP of string;;

let of_string s=AP(Tools_for_absolute_path.of_string s);;
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
    let _=Sys.command ("rm -f "^g1) in
    let _=cr g1 in
    let _=Sys.command ("mv "^g1^" "^w) in
    let _=Sys.command ("rm -f "^g1) in
    of_string w;;
    
let print_out (dummy:Format.formatter) ap=
   let x=to_string ap in
   Format.open_box 0;
   Format.print_string(x);
   Format.close_box();;



end;;






module Unjoin_path=struct


(* 


#use"unjoin_path.ml";;



*)

let unjoin_path ap=
  let (t1,t2)=Father_and_son.father_and_son (Absolute_path.to_string ap) '/' in
  (Directory_name.of_string(t1),
   No_slashes.of_string(t2));; 
  



   
   
   

end;;






module Substring=struct

(*

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
      Ennig.exists tester 0 (String.length(y)-lx);; 
      
  let leftmost_index_of_in x y=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) in
      try (Option.unpack(Ennig.find_it tester 0 (String.length(y)-lx))+1) with
      _->(-1);;
  
  let rightmost_index_of_in x y=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) 
      and temp1=List.rev(Ennig.ennig(0)(String.length(y)-lx)) in
      try ((Option.find_really tester temp1)+1) with
      _->(-1);;
  
   let leftmost_index_of_in_from x y i=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) in
      match Ennig.find_it tester (i-1) (String.length(y)-lx) with
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

 let show ()=Sys.command "ocamlc -i substring.ml";;  
   

end;;






module Subdirectory=struct

(*

Subdirectories name, with the trailing slash removed.

#use"subdirectory.ml";;

*)

type t=SD of string;;

let unveil (SD s)=s;;


let of_string s=SD s;;

let depth (SD s)=
 if s="" then 0 else
 (List.length(Substring.occurrences_of_in "/" s))+1;;

let to_string (SD s)=if s="" then "" else s^"/";;


let ocaml_name (SD s)="Subdirectory"^"."^"of_string(\""^s^"\")";;

end;;






module Strung=struct

(*

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
    Ennig.doyle (String.get s) 0 (n-1);;
    
 
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
  Ennig.doyle (fun i->(i,String.get s (i-1)) ) 1 n;;   
   
let number_of_lines_before s i=
   if i<1 then 0 else
   let m=min i (String.length s) in
   List.length(List.filter(fun j->(get s j)='\n')(Ennig.ennig 1 m));;
     
let split c s=
   let n=String.length s in
   let temp1=List.filter (fun j->(String.get s (j-1))=c) (Ennig.ennig 1 n) in
   if temp1=[] then [s] else
   let i1=List.hd(temp1) and i2=List.hd(List.rev temp1) in
   let  leftmost_helper=(if i1=1 then [] else [0,i1])
   and rightmost_helper=(if i2=n then [] else [i2,n+1]) in
   let temp2=leftmost_helper@(Listennou.universal_delta_list temp1)@rightmost_helper in
   Image.image (fun (i,j)->String.sub s i (j-i-1)) temp2;;
   
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
          match Option.find_it(fun t->
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






module Stabilize=struct


let one_more_time f (an_holl,da_ober)=
 let l_da_ober=Tidel.forget_order(da_ober) in
 let temp1=List.flatten(List.rev_map(f)(l_da_ober)) in
 let temp2=Tidel.diforchan(temp1) in
 let re_nevez=Tidel.lemel(temp2)(an_holl) in
 let hollad_nevez=Tidel.teuzin(an_holl)(re_nevez) in
 (hollad_nevez,re_nevez);; 
  
let rec morzholan f (an_holl,da_ober)=
  if da_ober=Tidel.empty_set
  then an_holl
  else morzholan f (one_more_time f (an_holl,da_ober));;
  
let father f l=let tl=Tidel.diforchan(l) in morzholan f (tl,tl);;

let one_more_time2 f (an_holl,graet,da_ober)=
 let l_graet=Tidel.forget_order(graet) 
 and l_da_ober=Tidel.forget_order(da_ober) in
 let zz1=Cartesian.product(l_graet)(l_da_ober)
 and zz2=Cartesian.product(l_da_ober)(l_graet) 
 and zz3=Cartesian.product(l_da_ober)(l_da_ober) in
 let zz=List.flatten [zz1;zz2;zz3] in
 let temp1=List.flatten(List.rev_map (function (x,y)->[f x y]) zz ) in
 let temp2=Tidel.diforchan(temp1) in
 let re_nevez=Tidel.lemel(temp2)(an_holl) in
 let hollad_nevez=Tidel.teuzin(an_holl)(re_nevez) in
 (hollad_nevez,an_holl,re_nevez);; 
  
let rec morzholan2 f (an_holl,graet,da_ober)=
  if da_ober=Tidel.empty_set
  then an_holl
  else morzholan2 f (one_more_time2 f (an_holl,graet,da_ober));; 
  
let binary_operation f l=let tl=Tidel.diforchan(l) in morzholan2 f (tl,Tidel.empty_set,tl);;  

let explore_tree f l0=
 let modified_l0=List.rev_map(function x->(x,0))(l0) in
 let modified_f=(function (x,j)->
   List.rev_map(function y->(y,j+1))(f x)
 ) in
 let rec tempf=(function (j,graet,da_ober)->
 match da_ober with
    []->graet
    |(xa,ia)::peurrest->let temp1=modified_f(xa,ia) in
                  let temp2=(j+1,xa::graet,List.rev_append temp1 peurrest) in
                  tempf(temp2)
 ) in
 tempf(0,[],modified_l0);;
 
 let explore_tree_explicitly f l0=
 let modified_l0=List.rev_map(function x->(x,0))(l0) in
 let modified_f=(function (x,j)->
   List.rev_map(function y->(y,j+1))(f x)
 ) in
 let rec tempf=(function (j,graet,da_ober)->
 match da_ober with
    []->graet
    |(xa,ia)::peurrest->let temp1=modified_f(xa,ia) in
                  let temp2=(j+1,xa::graet,List.rev_append temp1 peurrest) in
                  let _=(print_string("("^string_of_int(ia)^","^string_of_int(j+1)^")\n");
                  flush stdout) in
                  tempf(temp2)
 ) in
 tempf(0,[],modified_l0);;
 
type 'a hierarchize_data=
   Normal of  'a Tidel.set list * 'a Tidel.set * ('a * 'a Tidel.set) list
  |Failed of  'a Tidel.set list * 'a Tidel.set * ('a * 'a Tidel.set) list
  |Succeeded of 'a list list;;
   

let pusher_for_hierarchization (graet,hollad,da_ober)=
  if da_ober=[]
  then Succeeded(List.rev_map Ordered.forget_order graet)
  else 
  let temp1=Image.image (fun (x,y)->(x,Tidel.lemel y hollad)) da_ober in
  let (temp2,temp3)=List.partition (fun (x,z)->Tidel.length z=0) temp1 in
  if temp2=[]
  then Failed(graet,hollad,da_ober)
  else
  let temp4=Image.image fst temp2 in
  let o_temp4=Tidel.diforchan(temp4) in
  let temp5=Image.image (fun (x,z)->(x,Tidel.lemel z o_temp4)) temp3 in
  (Normal(o_temp4::graet,Tidel.teuzin hollad o_temp4,temp5));;
  
type 'a list_of_ancestors_map=('a -> 'a list);;  
  
let try_hierarchizing (f: 'a list_of_ancestors_map) l=
  let temp1=Image.image (fun t->(t,Tidel.diforchan(f t))) l in
  let rec tempf=(fun x->
    let y=pusher_for_hierarchization x in
    match y  with
    Normal(a,b,c)->tempf(a,b,c)
    |_->y) in
  tempf ([],Tidel.empty_set,temp1);;
  
let hierarchize f l=
  match try_hierarchizing f l with
   Succeeded(l)->l
  |_->failwith("Direct hierarchizing fails, there is a cycle. Try hierarchize instead");;
  
  



end;;






module Prepared=struct

let filter f=function
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






module Cull_string=struct

(*

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
  let j=Substring.leftmost_index_of_in(w)(x) in
  if j=(-1) then None else 
   Some(  beginning (j-1) x,
    cobeginning (j+String.length(w)-1) x);;


 let trim_spaces s=
   let n=String.length s in
   let opt1=Option.find_it(fun j->not(List.mem(String.get s (j-1)) [' ';'\t';'\n']))(Ennig.ennig 1 n) in
   if opt1=None then "" else
   let i1=Option.unpack opt1 in
   let k1=Option.find_really(fun j->not(List.mem(String.get s (n-j)) [' ';'\t';'\n']))(Ennig.ennig 1 n) in 
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
    if Substring.begins_with s le 
    then Some(cobeginning (String.length le) s)
    else None;; 

let try_remove_right_encloser s (re:right_encloser)=
    if Substring.ends_with s re 
    then Some(coending (String.length re) s)
    else None;;    
   
let try_remove_both_enclosers s (le,re)=
    match try_remove_left_encloser s le with
     None->None
    |Some(t)->try_remove_right_encloser t re;;
      
 let closeup_around_index s j=
   let n=String.length s in
   let temp1=List.filter(fun j->(String.get s (j-1))='\n')(Ennig.ennig 1 n) in
   let (temp2,temp3)=Prepared.partition_in_two_parts(fun k->k<j) temp1 in
   let a=(if List.length(temp2)<6 then 1 else List.nth(List.rev temp2)(5))
   and b=(if List.length(temp3)<6 then n else List.nth(temp3)(5)) in
   String.sub s (a-1) (b-a);;
   
exception Absent_beginning_marker of string;;
exception Absent_ending_marker of string;; 
 
let between_markers (bm,em) s=
     if (bm,em)=("","") then s else
     let i1=Substring.leftmost_index_of_in_from bm s 1  in
     if i1<1 then raise(Absent_beginning_marker(bm)) else
     let j1=i1+(String.length bm) in
     let i2=Substring.leftmost_index_of_in_from em s (j1+1) in
     if i2<1 then raise(Absent_ending_marker(bm)) else
     interval s j1 (i2-1);; 
 
let optional_between_markers p s=
   try Some(between_markers p s) with _->None;; 
   
(*

between_markers ("aaa","bb") "123aaa45bb678";;

*)     
   

  
  

end;;






module Industrial_separator=struct

(*

#use"industrial_separator.ml";;

Separators used in encoding of data types by strings.
The main key should not appear in any string inside the data types,
change it if necessary.

*)

let key="mpdykruvueaoqhkt";;

let d=3;;
let bound=
  let tens=Ennig.doyle (fun i->10) 1 d in
  (List.fold_left (fun u v->u*v ) 1 tens)-1;;

let counter=ref 0;;

exception Separator_overflow;;

let number_of_separators_used_so_far ()=(!counter);;

let new_separator ()=
  let i=(!counter)+1 in
  if i>bound
  then raise(Separator_overflow)
  else 
  counter:=i;
  key^(Cull_string.resize_from_right (string_of_int i) d '0');;
  

  
  

end;;






module Io=struct


let make_filename_complete s=
  let home=Sys.getenv("HOME") in
  if s="" then Absolute_path.of_string(home) else
  let c=String.get s 0 in
  if c='/' then Absolute_path.of_string(s) else
  if c='~' then Absolute_path.of_string(home^(String.sub s 1 (String.length(s)-1))) else
  Absolute_path.of_string((Sys.getcwd ())^"/"^s);;

let open_in_locally x=try open_in(x) with 
_->failwith("File "^x^" cannot be opened in");;

let open_out_locally x=try open_out(x) with 
_->failwith("File "^x^" cannot be opened out");;  

let put_whole_content_of_file_in_buffer s=
  let x=Absolute_path.to_string(make_filename_complete(s)) in
  let janet=open_in_locally(x) in
  let n=in_channel_length(janet) in
  let b=Buffer.create(n) in
  let _=Buffer.add_channel(b)(janet)(n) in
  let _=close_in janet in
  b;;
  
type filename=string;;
  
let erase_file_and_fill_it_with_contents_of_buffer (fn:filename) b=
   let x=Absolute_path.to_string(make_filename_complete(fn)) in
  let john=open_out_locally(x) in
  (Buffer.output_buffer(john)(b);close_out john);;
  
let erase_file_and_fill_it_with_string ap s=
   let fn=Absolute_path.to_string ap in
   let n=String.length(s) in
   let b=Buffer.create(n) in
   let _=Buffer.add_string b s in
   erase_file_and_fill_it_with_contents_of_buffer fn b;;
   
let read_whole_file ap=   
   let s=Absolute_path.to_string ap in
   let b=put_whole_content_of_file_in_buffer(s) in
   Buffer.contents b;;

let append_string_to_file s ap=
  let new_content=(read_whole_file ap)^s in
  erase_file_and_fill_it_with_string ap new_content;; 

     
   
   
  


end;;






module Rename_file=struct


(* 


#use"rename_file.ml";;

We separate renaming from relocating. The latter changes
directories while the former changes only the name, staying in the
same directory. 



*)

exception Failed of Absolute_path.t*No_slashes.t;;

let rename ap new_name=
  let old_path=Absolute_path.to_string ap
  and (dir,_)=Unjoin_path.unjoin_path ap in
  let new_path=(Directory_name.to_string dir)^(No_slashes.to_string new_name) in
  let i=Sys.command("mv "^old_path^" "^new_path) in
  if i<>0
  then raise(Failed(ap,new_name))
  else Absolute_path.of_string new_path;;



   
   
   

end;;






module Relocate_file=struct


(* 


#use"relocate_file.ml";;

We separate renaming from relocating. The latter changes
directories while the former changes only the name, staying in the
same directory. 



*)

exception Failed of Absolute_path.t*Directory_name.t;;

let relocate ap new_dir=
  let old_path=Absolute_path.to_string ap
  and (_,fn)=Unjoin_path.unjoin_path ap in
  let new_path=(Directory_name.to_string new_dir)^(No_slashes.to_string fn) in
  let i=Sys.command("mv "^old_path^" "^new_path) in
  if i<>0
  then raise(Failed(ap,new_dir))
  else Absolute_path.of_string new_path;;



   
   
   

end;;






module Reconstruct_linear_poset=struct

(*

#use"reconstruct_linear_poset.ml";;

Computes the (canonical) maximal acyclic sub-poset of a given poset, returns
it as a list L where each element of L is a triple (a,anc_a,a_is_clean)
where anc_a is the list of all ancestors of a, ordered as in L, and a_is_clean
is a boolean indicating if a is the ancestor or a descendant of an "active"
element.

The input is a list of vertex*bool elements,  where the boolean indicates
if the vertex is "active". If you don't need that functionality, you can set
those booleans to any arbitrary value.


Also returns a (non-canonical,non-exhaustive) set of cycles.


*)


let iterator coat 
  (checked,checked_union,cycles,cycles_union,between,not_yet_checked,opt1)=
    (* 
    between is a "chained" list of pairs (x1,x2),(x2,x3), ...
    (stocked in reverse actually)
    that will possibly lead to a cycle
    *)
    if opt1<>None then ([],Tidel.empty_set,[],Tidel.empty_set,[],[],opt1) else
    if (between,not_yet_checked)=([],[]) 
    then ([],Tidel.empty_set,[],Tidel.empty_set,[],[],
          Some(cycles,Listennou.rev_map (fun (z,p)->(z,fst p)) checked)) 
    else
    let a=
    	  (if between=[] 
    	   then List.hd(not_yet_checked)
           else snd(List.hd(between))
          ) in
    let not_yet_checked2=List.filter (fun z->z<>a) not_yet_checked in
    let coat_a=coat(a) in
    let coatoms_of_a=Tidel.safe_set(coat_a) in
    let temp1=Tidel.lemel coatoms_of_a checked_union in
    if Tidel.length(temp1)=0
    then let temp3=coatoms_of_a::(Image.image (fun z->snd(List.assoc z checked)) 
                      (coat_a)) in
         let ordered_set_version=Tidel.big_teuzin(temp3) in
         let temp4=Option.filter_and_unpack (
           fun (b,_)->if Tidel.elfenn b ordered_set_version
             then Some(b)
             else None
         ) checked in
         let list_version=List.rev(temp4) in
         let data_for_a=(list_version,ordered_set_version) in
         ((a,data_for_a)::checked,Tidel.insert a checked_union,
         cycles,cycles_union,[],not_yet_checked2,None)
    else
    if Tidel.elfenn a temp1
    then ([],Tidel.empty_set,[a]::cycles,Tidel.insert a cycles_union,
         [],not_yet_checked2,None) 
    else 
    if (not(Tidel.kengeij_goullo temp1 cycles_union))
    then (checked,checked_union,cycles,Tidel.insert a cycles_union,
         [],not_yet_checked2,None) 
    else 
    (*see if we can close the cycle *)
    match Option.find_it(fun (x,y)->Tidel.elfenn x temp1) between with
     None->(checked,checked_union,cycles,cycles_union,
     		(a,Tidel.hd temp1)::between,not_yet_checked,None)
    |Some(p)->
        let (before,_,after)=Three_parts.select_center_element_and_reverse_left (fun x->x=p) between in
        let temp2=Image.image fst before in
        let new_cycle=(fst p)::(temp2@[a]) in
        let ordered_cycle=Tidel.diforchan new_cycle in
        let not_yet_checked3=List.filter (fun z->Tidel.nelfenn z ordered_cycle) not_yet_checked in
        (checked,checked_union,new_cycle::cycles,
        Tidel.teuzin ordered_cycle cycles_union,
        [],not_yet_checked3,None);;

let reconstruct_linear_poset coat l=
  let rec tempf=(fun
  (checked,checked_union,cycles,cycles_union,between,not_yet_checked,opt)->
    if opt<>None
    then Option.unpack opt
    else tempf(iterator coat 
    (checked,checked_union,cycles,cycles_union,between,not_yet_checked,opt))
    ) in
    tempf([],Tidel.empty_set,[],Tidel.empty_set,[],l,None);;
    
(*

let sugar i=
   if (i<4)||(i>20) then [] else
   if i=11 then [5] else [i+1];;
    
reconstruct_linear_poset sugar (ennig 1 30);;  

let some_edges=
  [
    (1,4);(1,16);(2,6);(2,7);(3,16);(7,11);(7,19);(8,11);(8,15);(9,19);
    (10,1);(10,18);(11,2);(12,16);(13,5);(15,13);(16,17);(17,10);(17,14);(19,20);
    (20,21);(21,9)
  
  ];;

let brown j=image fst (List.filter (fun x->snd(x)=j) some_edges);;

reconstruct_linear_poset brown (ennig 1 21);;  


*)
 
    
    
    
    

end;;






module Is_an_ending_or_not=struct

(*

#use"is_an_ending_or_not.ml";;

*)

type t=
    Yes
   |No;;

    

end;;






module Preserve_initial_ordering=struct

(*

#use"preserve_initial_ordering.ml";;

*)

let preserve_initial_ordering l=
    let rec tempf=(fun
    (treated_part,ordered_treated_part,yet_untreated)->
      match yet_untreated with
      []->List.flatten(List.rev(treated_part))
      |x::remains->
        let better_x=List.filter 
        (fun y->Tidel.nelfenn y ordered_treated_part) x in
        if better_x=[]
        then tempf(treated_part,ordered_treated_part,remains)
        else
        let temp1=Tidel.teuzin(Tidel.diforchan x) ordered_treated_part in
        tempf(better_x::treated_part,temp1,remains)
    ) in
   tempf([],Tidel.empty_set,l);;

let and_mark_endings l=
	 let rec tempf=(fun
    (treated_part,ordered_treated_part,yet_untreated)->
      match yet_untreated with
      []->List.flatten(List.rev(treated_part))
      |x::remains->
        let better_x=List.filter 
        (fun y->Tidel.nelfenn y ordered_treated_part) x in
        if better_x=[]
        then tempf(treated_part,ordered_treated_part,remains)
        else
        let temp1=Tidel.teuzin(Tidel.diforchan x) ordered_treated_part in
        let temp2=List.rev(better_x) in
        let temp3=(List.hd temp2,Is_an_ending_or_not.Yes)::
        (Image.image (fun t->(t,Is_an_ending_or_not.No)) (List.tl temp2)) in
        tempf((List.rev temp3)::treated_part,temp1,remains)
    ) in
   tempf([],Tidel.empty_set,l);;

(*

preserve_initial_ordering
  [[18; 4; 14]; [17; 10; 16]; [16; 19]; [13; 19]; [13; 18; 3]];;

and_mark_endings
  [[18; 4; 14]; [17; 10; 16]; [16; 19]; [13; 19]; [13; 18; 3]];;

*)

    

end;;






module Chronometer=struct


let rewrite_days=function
0->""
|1->"1 day,"
|x->string_of_int(x)^" days,";;

let rewrite_hours=function
0->""
|1->"1 hour,"
|x->string_of_int(x)^" hours,";;

let rewrite_minutes=function
0->""
|1->"1 minute,"
|x->string_of_int(x)^" minutes,";;

let rewrite_seconds=function
0->""
|1->"1 second."
|x->string_of_int(x)^" seconds.";;

let rewrite_float x=
   let i=int_of_float(x) in
   let v_sec=(i mod 60) and q_sec=(i/60) in
   let v_min=(q_sec mod 60) and q_min=(q_sec/60) in
   let v_hour=(q_min mod 24) and q_hour=(q_min/24) in
   let s_day=rewrite_days(q_hour)
   and s_hour=rewrite_hours(v_hour)
   and s_min=rewrite_minutes(v_min)
   and s_sec=rewrite_seconds(v_sec) in
   s_day^s_hour^s_min^s_sec;;
  
 let timer=ref(0.000);;  
 
 let duration_of_computation f x=
   let t0=Unix.time() in
   let _=f(x) in
   let _=(timer:=Unix.time()-.t0) in
   (print_string("Computation lasted "^(rewrite_float (!timer))^"\n");flush stdout);;
 
 let duration_of_last_computation ()=
  (print_string("Computation lasted "^(rewrite_float (!timer))^"\n");flush stdout);;
   
   
 let  it f x=
  let t0=Unix.time() in
   let y=f(x) in
   let _=(timer:=Unix.time()-.t0) in
   let _=(print_string("Computation lasted "^(rewrite_float (!timer))^"\n");flush stdout) in
   y;;
 
   


end;;






module Explicit=struct

let iter0 (f:'a->unit) l addenda=
  let n=List.length(l)
  and accu=ref(l) in
  let s0=" of "^string_of_int(n)^" "^addenda^"\n" in
  for j=1 to n
               do
               ( f(List.hd(!accu));
                 accu:=List.tl(!accu);
                 print_string(string_of_int(j)^s0);
                 flush stdout)
               done;;


let iter1 (f:'a->'a) initial_value a_priori_size addenda=
  let accu=ref(initial_value) in
  let s0=" of "^string_of_int(a_priori_size)^" "^addenda^"\n" in
  let _=(for j=1 to a_priori_size
               do
               ( 
                 accu:=f(!accu);
                 print_string(string_of_int(j)^s0);
                 flush stdout;
               )
               done) in
  !accu;;

let iter2 (f:'a->'a) initial_value tester (shower:'a->string)  addenda=
  let accu=ref(initial_value) in
  let _=(while tester(!accu)
               do
               ( 
                 accu:=f(!accu);
                 print_string((shower (!accu))^addenda);
                 flush stdout;
               )
               done) in
  !accu;;

let unverbose_iter (f:'a->'a) initial_value tester=
  let accu=ref(initial_value) in
  let _=(while tester(!accu)
               do
               ( 
                 accu:=f(!accu);
               )
               done) in
  !accu;;

let iter f l=iter0 f l "";;

  
let iter_on_ennig f a b=
  let n=b-a+1 in
  let s0=" of "^string_of_int(n)^"\n" in
  for j=1 to n
               do
               (f(a-1+j);print_string(string_of_int(j)^s0);flush stdout)
               done;;

let e_rev l=
   let accu=ref([]) in 
   let f=(fun x->accu:=x::(!accu)) in
   let _=iter0(f)(l)(" (rev part)") in
   !accu;;  

 let unchronometered_filter f l=
   let accu=ref([]) in 
   let g=(fun x->if f(x) then accu:=x::(!accu) else ()) in
   let _=iter0(g)(l)(" (filter part)") in
   e_rev(!accu);;    
 
 let filter f l=Chronometer.it (unchronometered_filter f) l;; 
   
 let unchronometered_image f l=
   let accu=ref([]) in 
   let g=(fun x->accu:=f(x)::(!accu)) in
   let _=iter0(g)(l)(" (rev_image part)") in
   e_rev(!accu);;  
   
 let image f l=Chronometer.it (unchronometered_image f) l;;  
 
 let unchronometered_image_computed_backwards f l=
   let temp1=e_rev(l) in
    let accu=ref([]) in 
   let g=(fun x->accu:=f(x)::(!accu)) in
   let _=iter0(g)(temp1)(" (image part)") in
   (!accu);;     
 
  let image_computed_backwards f l=Chronometer.it 
   	(unchronometered_image_computed_backwards f) l;;  
 
 let unchronometered_map_on_cartesian_product f l1 l2=
   let iterator=(
      fun (graet,x0,y_da_ober,x_da_ober,y_klok)->
        if y_da_ober<>[]
        then let y0=List.hd(y_da_ober) and peurrest_y=List.tl(y_da_ober) in
             let opt=f(x0,y0) in
             if opt=None
             then (graet,x0,peurrest_y,x_da_ober,y_klok) 
             else ((Option.unpack opt)::graet,x0,peurrest_y,x_da_ober,y_klok)
        else
        if x_da_ober=[]
        then (graet,x0,y_da_ober,x_da_ober,y_klok)
        else
        let x1=List.hd(x_da_ober) and peurrest_x=List.tl(x_da_ober) in
        (graet,x1,y_klok,peurrest_x,y_klok)
   )
   and n=(List.length l1)*((List.length l2)+1)-1
   and initial_value=([],List.hd l1,l2,List.tl l1,l2) in
   let (graet,_,_,_,_)=iter1 iterator initial_value n " (cartesian part)" in
   e_rev graet;;
   
 let map_on_cartesian_product f l1 l2=Chronometer.it (unchronometered_map_on_cartesian_product f l1) l2;;
 
 let unchronometered_find_it f l=
    let g=(fun (graet,da_ober)->
       match da_ober with
       []->(graet,[])
       |a::peurrest->
         if f(a)
         then (Some a,[])
         else (None,peurrest)
    ) in
    fst(iter1 g (None,l) (List.length l) " (finder part)");;
 
 let find_it f l=Chronometer.it (unchronometered_find_it f) l;;  
 
 let unchronometered_explore_tree f l=
    let g=(fun (graet,p,q,da_ober)->
       match da_ober with
       []->(graet,0,0,[])
       |a::peurrest->
         let temp1=f a in
         if temp1=[]
         then (a::graet,p-1,q+1,peurrest)
         else (a::graet,p-1+List.length(temp1),q+1,temp1@peurrest)
    ) and 
    tester=(fun (graet,p,q,da_ober)->da_ober<>[]) 
    and
    shower=(
     fun (graet,p,q,da_ober)->
       (string_of_int p)^" to be explored ; "^
       (string_of_int q)^" already explored\n"
    )
    in
    let initial_value=([],List.length l,0,l) in
    let _=(print_string(shower(initial_value));flush stdout) in
    let (ans,_,_,_)=iter2 g initial_value tester shower  "" in
    ans;;

let pusher_in_leaf_finding f (graet,p,q,da_ober)=
  let a=List.hd(da_ober) and peurrest=List.tl(da_ober) in
  let temp1=f a in
  let new_walker=(fun ()->
    if temp1=[]
    then (a::graet,p+1,q+List.length(temp1)-1,peurrest) 
    else (graet,p+1,q+List.length(temp1)-1,temp1@peurrest) 
  )()
  in
  let (_,new_p,new_q,_)=new_walker in   
  let s=(string_of_int new_q)^" to be explored ; "^
       (string_of_int new_p)^" found\n" in  
  let _=(print_string s;flush stdout) in
      new_walker;;
 
let rec unchronometered_find_leaves 
  (f:('a -> 'a list)) 
  (g: ('a list * int * int * 'a list -> unit)) 
  (watcher,accu_ref)=
   let walker=(!accu_ref) in
   let (_,_,q,_)=walker in
   if (q>0)&&(Sys.file_exists watcher)
   then let new_walker=pusher_in_leaf_finding f walker in
        let _=(accu_ref:=new_walker) in
        unchronometered_find_leaves f g (watcher,accu_ref)
   else let _=g walker in walker;;
  
 
 let unchronometered_unverbose_explore_tree f l=
    let g=(fun (graet,da_ober)->
       match da_ober with
       []->(graet,[])
       |a::peurrest->
         let temp1=f a in
         if temp1=[]
         then (a::graet,peurrest)
         else (a::graet,temp1@peurrest)
    ) and 
    tester=(fun (graet,da_ober)->da_ober<>[]) 
    in
    let initial_value=([],l) in
    let (ans,_)=unverbose_iter g initial_value tester  in
    ans;;
 
 let explore_tree f l=Chronometer.it (unchronometered_explore_tree f) l;;  
 
 let find_leaves f g x=Chronometer.it (unchronometered_find_leaves f g) x;;  
 
 let unchronometered_explore_tree_with_stop checker f l=
    let g=(fun (p,q,da_ober,optional)->
       match da_ober with
       []->(0,0,[],None)
       |a::peurrest->
         let temp1=f a in
         let temp2=Option.find_it checker temp1 in
         if temp2<>None
         then (p+1,0,[],temp2)
         else 
         if temp1=[]
         then (p-1,q+1,peurrest,None)
         else (p-1+List.length(temp1),q+1,temp1@peurrest,None)
    ) and 
    tester=(fun (p,q,da_ober,optional)->(da_ober<>[])&&(optional<>None)) 
    and
    shower=(
     fun (p,q,da_ober,optional)->
       (string_of_int p)^" to be explored ; "^
       (string_of_int q)^" already explored\n"
    )
    in
    let initial_value=(List.length l,0,l,None) in
    let _=(print_string(shower(initial_value));flush stdout) in
    let (_,_,_,ans)=iter2 g initial_value tester shower  "" in
    ans;;
    
 let unchronometered_unverbose_explore_tree_with_stop checker f l=
    let g=(fun (da_ober,optional)->
       match da_ober with
       []->([],None)
       |a::peurrest->
         let temp1=f a in
         let temp2=Option.find_it checker temp1 in
         if temp2<>None
         then ([],temp2)
         else 
         if temp1=[]
         then (peurrest,None)
         else (temp1@peurrest,None)
    ) and 
    tester=(fun (da_ober,optional)->(da_ober<>[])&&(optional<>None)) 
    in
    let initial_value=(l,None) in
    let (_,ans)=unverbose_iter g initial_value tester  in
    ans;;
       
    
 
 let explore_tree_and_stop f l=Chronometer.it 
 (unchronometered_explore_tree_with_stop f) l;;  
 
 type verbose_bool=bool;;
 type stopped_bool=bool;;
 
 let explore_tree_with_options checker translator f l 
  (verbose:verbose_bool)
   (stopped:stopped_bool)=
   if stopped
   then let opt=
        (fun bowl->
          if bowl
          then unchronometered_explore_tree_with_stop 
                   checker f l
          else unchronometered_unverbose_explore_tree_with_stop 
                   checker f l
        )(verbose) in
        if opt=None then "" else
        "\n\n\n"^(Option.unpack opt)^"\n\n\n"
   else let temp1=
        (fun bowl->
          if bowl
          then unchronometered_explore_tree f l
          else unchronometered_unverbose_explore_tree f l
        )(verbose) in
        let temp2=Image.image translator temp1 in 
        "\n\n\n"^(String.concat "\n" temp2)^"\n\n\n";;     
 
 
 let hard_big_concat ll=
   let accu=ref([]) in 
   let f=(fun x->accu:=x::(!accu)) in
   let _=iter0(f)(ll)(" (rev part before concatenating)") in
   let temp1=(!accu) in
   let last_one=List.hd(temp1) and others=List.tl(temp1) in
   let accu2=ref(last_one) in 
   let g=(fun x->accu2:=x@(!accu2)) in
   let _=iter0(g)(others)(" (concatenating)") in
   !accu2;;
   
 let partition f l=
  let hanterenn1=ref([])
  and hanterenn2=ref([]) in
  let h=(function x->
    if f(x)
    then hanterenn1:=x::(!hanterenn1)
    else hanterenn2:=x::(!hanterenn2)
  ) in
  let _=iter(h)(l) in
 (!hanterenn1,!hanterenn2);;



type 'a tester=('a -> bool);;
type ('a,'b) displayer=('a->'b);;
type 'a hammer=('a->'a);;

let morzholan 
  (tester:'a tester) 
  (displayer:('a,'b) displayer) 
  (hammer:'a hammer)
  (x0:'a)=
    let rec tempf=(
     fun (j,x)->
        if tester(x)
        then displayer(x)
        else
          let xx=hammer(x) in
          let _=(print_string("Iteration number "^string_of_int(j)^" done \n");flush stdout) in 
          tempf(j+1,xx)
    ) in
    tempf(1,x0);;
    
  



end;;






module More_unix=struct

(*

#use"more_unix.ml";;

*)


module Private=struct
 
let naive_extension ap=
   let s=Absolute_path.to_string ap in
   let i=String.rindex s '.' in
   (Cull_string.cobeginning (i+1) s);; 
   
let extension x=try (naive_extension x) with 
  any_exception->"";;
  
 let is_a_directory ap=
   let s=Absolute_path.to_string ap in
   try (function x->true)(Sys.readdir s) with any_exception->false;;
 
 let father ap=
   let s=Absolute_path.to_string ap in
   let i=String.rindex s '/' in
   if i=0 then Directory_name.of_string"/" else
   Directory_name.of_string (Cull_string.beginning i s);; 
   
 let son dir=
   let s=Directory_name.to_string dir in
   let i=String.rindex s '/' in
   if i=0 then "" else
   (Cull_string.cobeginning (i+1) s);; 
  
 let is_a_nondirectory_or_a_nib x=
  if is_a_directory(x)
  then extension(x)="nib"
  else not(Substring.is_a_substring_of(".nib/")(Absolute_path.to_string x));;
  
 let naive_ls dir=
   let s=Directory_name.to_string dir in
   let s_with_slash=(function ()->
    if String.get(s)(String.length(s)-1)='/'
    then s
    else s^"/"
   )() in
   let temp1=Array.to_list(Sys.readdir(s)) in
   let tempf=(function w->try (Some(Absolute_path.of_string(s_with_slash^w))) with
   any_exception->None) in
   Option.filter_and_unpack tempf temp1;;
   
 let ls x=try (naive_ls x) with any_exception->[];;  
 
 let test_for_cleaniness=function ap->
  let s=Absolute_path.to_string ap in
  Father_and_son.son(s)('/')<>".DS_Store";;
 
 let cleaned_ls x=
   List.filter test_for_cleaniness (ls x);;
   
 let dirty_ones_in_ls x=
   List.filter (function u->not(test_for_cleaniness u) )(ls x);; 
 
 let adhoc_ls ap=
   let s=Absolute_path.to_string ap in
   if not(is_a_directory ap) 
   then []
   else 
   let dir=Directory_name.of_string s in
   ls dir;;
 

 
let complete_ls dir=
   let s_dir=Directory_name.to_string dir in
   let x=Absolute_path.of_string s_dir in
   Explicit.explore_tree adhoc_ls [x];;   


 let complete_ls_with_nondirectories_only x=
  List.filter(is_a_nondirectory_or_a_nib)(complete_ls x);;
  
  
 let beheaded_ls_with_nondirectories_only x=
  let n0=String.length(Absolute_path.to_string x) in
  let temp1=List.filter(is_a_nondirectory_or_a_nib)(adhoc_ls x) in
  let temp2=Image.image (fun ap->Cull_string.cobeginning n0 (Absolute_path.to_string ap)) temp1 in
  temp2;; 
 
 let dir_substructure x=
    let n0=String.length(Absolute_path.to_string x) in
    let temp1=(Stabilize.explore_tree adhoc_ls (adhoc_ls x)) in
    let temp2=List.filter(function x->extension(x)<>"nib")(temp1) in
    List.rev_map(function ap->Cull_string.cobeginning n0 (Absolute_path.to_string ap))(temp2);;
  
 let endfiles x=
    let n0=String.length(Absolute_path.to_string x)+1(*because of the slash!*) in
    let temp1=(Stabilize.explore_tree adhoc_ls (adhoc_ls x)) in
    let temp2=List.filter(is_a_nondirectory_or_a_nib)(temp1) in
    List.rev_map(function ap->Cull_string.cobeginning n0 (Absolute_path.to_string ap))(temp2);;
    
let quick_complete_ls s=
  let x=Directory_name.of_string s in
  let temp1=complete_ls x in
  Image.image Absolute_path.to_string temp1;;  
  
 

let quick_beheaded_complete_ls s=
  let x=Directory_name.of_string s in
  let n=String.length(Directory_name.to_string x) in
  let temp1=complete_ls x in
  Image.image (fun ap->Cull_string.cobeginning n (Absolute_path.to_string ap)) temp1;; 
  
end;;    

 
let complete_ls=Private.complete_ls;;
let quick_beheaded_complete_ls=Private.quick_beheaded_complete_ls;;
let complete_ls_with_nondirectories_only=Private.complete_ls_with_nondirectories_only;;
   


end;;






module Path_is_in_directory=struct

(*

#use"path_is_in_directory.ml";;

*)

let path_is_in_directory ap dir=
  Substring.begins_with
   (Absolute_path.to_string ap)
   (Directory_name.to_string dir)
   ;;


end;;






module Outside_comments_and_strings=struct

(*

#use"outside_comments_and_strings.ml";;

Detect in a text the parts which can possibly contain module
names, i.e. those parts which are outside comments and outside
strings.

Comments are a little more complicated than strings because they
can be nested. Also, note that we can have strings inside comments :
for example (* a "(*" b *) is a valid OCaml code snippet.

*)

(*
To keep the automaton simple, changes are notified as soon as
possible. Thus, the automaton toggles string_mode or changes
nesting comment depth as soon as the terminating character is
encountered.

*)


type state={
    depth : int;
    string_mode         : bool;
    lastchar_is_a_left_paren        : bool;
    lastchar_is_a_star              : bool;
    penultchar_is_a_left_paren      : bool;
    interval_start : int;
    accumulator : (int*int*string) list;
};;


let one_more_step s n j c x=
   let d=x.depth in 
   let comment_opened_now=(x.lastchar_is_a_left_paren)&&(c='*')&&(not(x.string_mode))
   and comment_closed_now=
            (not(x.penultchar_is_a_left_paren))
            &&(x.lastchar_is_a_star)
            &&(c=')')
            &&(not(x.string_mode)) in
   let new_depth=(
   		if x.string_mode
        then d
        else
        if comment_opened_now
        then d+1
        else  
        if comment_closed_now
        then max 0 (d-1)
        else  d
   
   ) in
   let new_start=
      ((x.depth=0)&&(x.string_mode)&&(c='"'))
      ||
      ((x.depth=1)&&comment_closed_now)
      ||
      ((x.depth=0)&&comment_opened_now) in
    let opt_upper_bound=(
       if x.depth>0
       then None
       else
       if (c='"')&&(not(x.string_mode))
       then Some(j-1)
       else
       if comment_opened_now
       then Some(j-2)
       else 
       if (j=n)&&(not(x.string_mode))
       then Some(j)
       else None
    ) in
    let old_accu=x.accumulator in  
    let new_accu=(
       match opt_upper_bound with
       None->old_accu
       |Some(upper_bound)->
           let lower_bound=x.interval_start in
           if lower_bound>upper_bound
           then old_accu
           else let new_itv=Cull_string.interval s lower_bound upper_bound in 
               (lower_bound,upper_bound,new_itv)::old_accu
    ) in  
      
  {
    depth =new_depth;
    string_mode    =(if c='\"' 
                     then not(x.string_mode ) 
                     else x.string_mode);
    lastchar_is_a_left_paren   =(c='(');
    lastchar_is_a_star         =(c='*');
    penultchar_is_a_left_paren =x.lastchar_is_a_left_paren;
    interval_start=(if new_start then j+1 else x.interval_start);
    accumulator=new_accu;
};;

let initial_state=  
 {
    depth =0;
    string_mode    =false;
    lastchar_is_a_left_paren   =false;
    lastchar_is_a_star         =false;
    penultchar_is_a_left_paren =false;
    interval_start=1;
    accumulator=[];
};;

let rec iterator (s,n,j,st)=
    if j>n
    then List.rev(st.accumulator)
    else iterator(s,n,j+1,one_more_step s n j (String.get s (j-1)) st);;
    
let good_substrings s=iterator(s,String.length s,1,initial_state);;    

(*


good_substrings "abcdef";;
good_substrings "(*abc*)def";;
good_substrings "ab(*cde*)f";;
good_substrings "ab\"cde\"f";;
good_substrings "\"abc\"def";;
good_substrings "ghi(*a(*b*)c*)def";;
good_substrings "ghi(**a(*b*)c**)def";;
good_substrings "ghi(**a\"b\"c**)def";;
good_substrings "123\"(*\"890\"*)\"567";;
good_substrings "123(*67\"90\"23*)67";;

let nachste (s,n,j,st)=(s,n,j+1,one_more_step s n j (String.get s (j-1)) st);;
let s0="123\"(*\"890\"*)\"567";;
let n0=String.length s0;;
let v0=(s0,n0,1,initial_state);;
let ff=Memoized.small nachste v0;;
let gg n=match ff n with (_,_,_,st)->st;;


*)      



end;;






module Ocaml_ending=struct

(*

#use"ocaml_ending.ml";;

*)

type t=Ml |Mli |Mll |Mly;;

let ml=Ml and mli=Mli and mll=Mll and mly=Mly;;



let exhaustive_uple f=(f Ml,f Mli,f Mll,f Mly);;

(*
Caution! The order is important in the all_endings list below.
It says in which order of priority one should look for
a file to read in order to extract dependency information
about the module. See the find_suitable_ending below.
*)


let all_endings=[mll;mly;ml;mli];;
let all_string_endings=[".mll";".mly";".ml";".mli"];;
let correspondances=List.combine all_endings all_string_endings;;

exception Unknown_ending of string;;

let of_string s=
  try (fst(Option.find_really (fun (x,y)->y=s) correspondances))
  with _->raise(Unknown_ending(s));;

let to_string edg=snd(Option.find_really (fun (x,y)->x=edg) correspondances);;  



let ocaml_name w=
 let c="Ocaml_ending"^"." in
 match w with
 Ml->c^"Ml"
|Mli->c^"Mli"
|Mll->c^"Mll"
|Mly->c^"Mly";;


end;;






module Nonblank=struct


(* 

Useful to avoid empty strings between two successive separators in
an archive string. To see example of how it is used, look at the
archive/unarchive function in the new_modulesystem_data module for
example.

#use"swoon.ml";;

*)

let make s=if s="" then "#" else s;;
let decode s=if s="#" then "" else s;;
  


end;;






module Ocaml_library=struct


(* 




#use"Makefile_makers/ocaml_library.ml";;


*)


type t=NumLib |StrLib |UnixLib;;

let correspondances=[NumLib,"num";StrLib,"str";UnixLib,"unix"];;

exception Unknown_lib of string;;

let of_string s=
  try (fst(Option.find_really (fun (x,y)->y=s) correspondances))
  with _->raise(Unknown_lib(s));;

let to_string lib=snd(Option.find_really (fun (x,y)->x=lib) correspondances);;  


let short_name=function
   NumLib->"NumLib" 
  |StrLib->"StrLib" 
  |UnixLib->"UnixLib";;

let ocaml_name lib=
  (*cutting the name as always, to avoid a circular definition *)
  "Ocaml"^"_library."^(short_name lib);;

let file_for_library=function 
  NumLib->"nums" |StrLib->"str" |UnixLib->"unix";;  

let modules_telling_a_library_away=function
NumLib->["num";"big_int";"arith_status"] 
|StrLib->["str"] 
|UnixLib->["unix"];;    


let all_libraries=[NumLib;StrLib;UnixLib];;  

let compute_needed_libraries_from_uncapitalized_modules_list l=
   List.filter (
      fun lib->List.exists(
        fun z->List.mem z (modules_telling_a_library_away lib)
      ) l
   ) all_libraries;;


end;;






module Naked_module=struct

(*

#use"naked_module.ml";;

A module name, or a candidate for one. Should contain no slashes.

*)

type t=N of string;;

let of_string s=N s;; 
let to_string (N s)=s;;


let ocaml_name w=
  let s=to_string w in
  "Naked_module"^".of_string(\""^s^"\")";;    

  
   

end;;






module Half_dressed_module=struct

(*

#use"half_dressed_module.ml";;

A module name, or a candidate for one. Can contain  slashes.
Should not contain dots.
Starts with an uncapitalized letter.
Designates a relative path.

*)

type t=HD of string*Directory_name.t;;

exception Inexistent_module of string;;
 
let of_string_and_root old_s dir=
        let s=Father_and_son.invasive_father old_s '.' in
        let s_dir=Directory_name.to_string dir in
	    if List.for_all (fun edg->not(Sys.file_exists(s_dir^s^edg)) ) Ocaml_ending.all_string_endings
	    then raise(Inexistent_module(s_dir^s))
	    else
	   HD(Father_and_son.invasive_father s '.',dir) ;;   
   
   
let to_string (HD (s,dir))=s;;

let unveil (HD (s,dir))=(s,dir);;

exception FileOutsideDirectory of Absolute_path.t*Directory_name.t;;


let of_path_and_root ap dir=
    if (not(Path_is_in_directory.path_is_in_directory ap dir))
    then raise(FileOutsideDirectory(ap,dir))
    else 
    let s_dir=Directory_name.to_string dir in
    let n_dir=String.length s_dir in
    let subpath=Cull_string.cobeginning n_dir (Absolute_path.to_string ap) in
    HD(Father_and_son.invasive_father subpath '.',dir) ;;    
    
    
let undress (HD (s,dir))=
   try ((fun r->Naked_module.of_string(String.sub s (r+1) (String.length(s)-(r+1))) )
   (String.rindex s '/'))
   with
   _->Naked_module.of_string s;;

let is_optional (HD (s,dir))=
  if String.length(s)<9 then false else
  String.sub s 0 9="Optional/";;

let is_forgotten (HD (s,dir))=
  if String.length(s)<10 then false else
  String.sub s 0 10="Forgotten/";;

let is_remembered (HD (s,dir))=
  if String.length(s)<11 then false else
  String.sub s 0 11="Remembered/";;

let is_archived hd=(is_optional hd)||(is_forgotten hd)||(is_remembered hd);;

let is_executable (HD (s,dir))=
  let n=String.length s in
  if String.length(s)<10 then false else
  String.sub s (n-10) 10="executable";;


let optional_base_directory (HD (s,dir))=try (
   let r=String.rindex s '/' in
   let dir=Directory_name.of_string(String.sub s 0 r) in
   Some dir )
   with
   _->None;;

let subdirectory hm=
       let s_hm=to_string hm in
       let s_dir=Father_and_son.father s_hm '/' in
       Subdirectory.of_string s_dir;;

let root_directory (HD(s,dir))= dir;;

let module_name (HD (s,dir))=
  let t=Father_and_son.son s '/'  in
  (String.capitalize_ascii t);;

let ocaml_name (HD (s,dir))=
  "Half_dressed_module"^".of_string_and_index(\""^s^"\")("^
  (Directory_name.ocaml_name dir)^")";;    

let industrial_separator=Industrial_separator.new_separator ();;  

let archive x=
   let (s,dir)=unveil x in
   String.concat industrial_separator [s;Directory_name.to_string dir];;
  
let unarchive s=
   let l1=Str.split (Str.regexp_string industrial_separator) s in
   let dir=Directory_name.of_string(List.nth l1 1) in
   HD(List.nth l1 0, dir);;
   
  
          
   


end;;






module Mlx_filename=struct

(*

#use"mlx_filename.ml";;

*)

type t=
  ML of string*Directory_name.t
 |MLI of string*Directory_name.t
 |MLL of string*Directory_name.t
 |MLY of string*Directory_name.t;;

exception Unknown_ending of string;;
exception Unpointed_filename of string;;

exception Inexistent_filename of string;;

let unveil=function
   ML(s,dir)->(s^".ml",dir)
  |MLI(s,dir)->(s^".mli",dir)
  |MLL(s,dir)->(s^".mll",dir)
  |MLY(s,dir)->(s^".mly",dir);;
  
let short_path mlx=fst(unveil mlx);;  

let of_string_and_root s dir= 
  if not(String.contains s '.') then raise(Unpointed_filename(s)) else
  let (core,ending)=Father_and_son.father_and_son s '.' in
  let s_dir=Directory_name.to_string dir in
  if (not(Sys.file_exists(s_dir^s)))
  then raise(Inexistent_filename(s_dir^s))
  else
  if ending="ml"  then ML  (core,dir) else
  if ending="mli" then MLI (core,dir) else
  if ending="mll" then MLL (core,dir) else
  if ending="mly" then MLY (core,dir) else
  raise(Unknown_ending(s));;

let try_from_string_and_root s dir=
  try (Some(of_string_and_root s dir)) with _->None;;

let to_string=function
   ML(s,dir)->(s^".ml")
  |MLI(s,dir)->(s^".mli")
  |MLL(s,dir)->(s^".mll")
  |MLY(s,dir)->(s^".mly");;

let root=function
   ML(s,dir)->dir
  |MLI(s,dir)->dir
  |MLL(s,dir)->dir
  |MLY(s,dir)->dir;;

exception FileOutsideDirectory of Absolute_path.t*Directory_name.t;;


let of_path_and_root ap dir=
    if (not(Path_is_in_directory.path_is_in_directory ap dir))
    then raise(FileOutsideDirectory(ap,dir))
    else 
    let s_dir=Directory_name.to_string dir in
    let n_dir=String.length s_dir in
    let subpath=Cull_string.cobeginning n_dir (Absolute_path.to_string ap) in
    of_string_and_root subpath dir;;    

let try_from_path_and_root ap dir=
    try (Some(of_path_and_root ap dir)) with _->None;;

let decompose=function
   ML(s,dir)->(Half_dressed_module.of_string_and_root s dir,Ocaml_ending.Ml)
  |MLI(s,dir)->(Half_dressed_module.of_string_and_root s dir,Ocaml_ending.Mli)
  |MLL(s,dir)->(Half_dressed_module.of_string_and_root s dir,Ocaml_ending.Mll)
  |MLY(s,dir)->(Half_dressed_module.of_string_and_root s dir,Ocaml_ending.Mly);;

let half_dressed_core mlx=fst(decompose mlx);;
let ending mlx=snd(decompose mlx);;

let to_path mlx=
  let (hm,edg)=decompose mlx in
  let dir=root mlx in
  let s_hm=Half_dressed_module.to_string hm 
  and s_dir=Directory_name.to_string dir in
  Absolute_path.of_string( s_dir^s_hm^(Ocaml_ending.to_string edg) );;

let join hs ending=
  let (s,dir)=Half_dressed_module.unveil hs in
  match ending with
   Ocaml_ending.Ml-> ML (s,dir)
  |Ocaml_ending.Mli-> MLI (s,dir)
  |Ocaml_ending.Mll-> MLL (s,dir)
  |Ocaml_ending.Mly-> MLY (s,dir);;

  
exception Failed_File_Renaming of t*string;;  
  
(*

notice that the ending is preserved below. If the user
unwittingly puts a wrong ending, this will have no effect.

*)  
  
let do_file_renaming mlx new_name=
  let core=Father_and_son.invasive_father (No_slashes.to_string new_name) '.' in
  let checked_name=No_slashes.of_string(core^(Ocaml_ending.to_string(ending mlx))) in
  let ap=to_path mlx in
  let new_ap=Rename_file.rename ap checked_name in
  of_path_and_root new_ap (root mlx);;   
  
let do_file_displacing mlx new_subdir=
  let s_new_subdir=Subdirectory.to_string new_subdir
  and dir=root mlx in
  let s_dir=Directory_name.to_string dir in
  let new_dir=Directory_name.of_string(s_dir^s_new_subdir) in
  let ap=to_path mlx in
  let new_ap=Relocate_file.relocate ap new_dir in
  of_path_and_root new_ap (root mlx);;  
  
  
let is_optional x=Half_dressed_module.is_optional(half_dressed_core x);;  
let is_archived x=Half_dressed_module.is_archived(half_dressed_core x);;  

let complete_ls dir=
  let temp1=Directory_name.to_string dir in
  let temp2=More_unix.quick_beheaded_complete_ls temp1 in
  let temp3=Option.filter_and_unpack(
     fun s->try_from_string_and_root s dir
  ) temp2 in
  List.filter (fun mlx->not(is_archived mlx)) temp3;;

let to_absolute_path mlx=
 let (s,dir)=unveil mlx in
 let s_dir=Directory_name.to_string dir in
 Absolute_path.of_string(s_dir^s);;   


let ocaml_name w=
  let (s,dir)=unveil w in
  "Mlx_file"^"name"^".of_string_and_index("^
  (Strung.enclose s)^
  ")("^(Directory_name.to_string dir)^")";;    

let industrial_separator1=Industrial_separator.new_separator ();;  
 


let prepare_archive=function
   ML(s,dir)->["ml";s;Directory_name.to_string dir]
  |MLI(s,dir)->["mli";s;Directory_name.to_string dir]
  |MLL(s,dir)->["mll";s;Directory_name.to_string dir]
  |MLY(s,dir)->["mly";s;Directory_name.to_string dir];;

  
exception Unrecognized_constructor of string;;   
  
let archive x=String.concat industrial_separator1 (prepare_archive x);;
 

let unarchive s=
   let l1=Str.split (Str.regexp_string industrial_separator1) s in
   let c=List.hd l1 and s=List.nth l1 1 and dir=Directory_name.of_string(List.nth l1 2) in
   if c="ml" then    ML(s,dir) else
   if c="mli" then  MLI(s,dir) else
   if c="mll" then  MLL(s,dir) else
   if c="mly" then  MLY(s,dir) else
   raise(Unrecognized_constructor(c));;


end;;






module Modulesystem_data=struct


(* 

Gathers all (ml/mli/mll/mly) corresponding to the same module.

#use"Makefile_makers/modulesystem_data.ml";;

*)


 

type t={
    name : Half_dressed_module.t;
    ml_present  : bool;
    mli_present : bool;
    mll_present : bool;
    mly_present : bool;
    ml_modification_time : float;
    mli_modification_time : float;
    mll_modification_time : float;
    mly_modification_time : float;
    needed_libraries : Ocaml_library.t list;
    direct_fathers : Half_dressed_module.t list;
    all_ancestors : Half_dressed_module.t list;
    needed_directories : Subdirectory.t list;
};;
   
   

let name x=x.name;;
let ml_present x=x.ml_present;;
let mli_present x=x.mli_present;;
let mll_present x=x.mll_present;;
let mly_present x=x.mly_present;;
let presences x=(x.ml_present,x.mli_present,x.mll_present,x.mly_present);;
let ml_modification_time x=x.ml_modification_time;;
let mli_modification_time x=x.mli_modification_time;;
let mll_modification_time x=x.mll_modification_time;;
let mly_modification_time x=x.mly_modification_time;;
let needed_libraries x=x.needed_libraries;;
let direct_fathers x=x.direct_fathers;;
let all_ancestors x=x.all_ancestors;;
let needed_directories x=x.needed_directories;;

let modification_time x edg=match edg with
   Ocaml_ending.Ml->ml_modification_time x
  |Ocaml_ending.Mli->mli_modification_time x
  |Ocaml_ending.Mll->mll_modification_time x
  |Ocaml_ending.Mly->mll_modification_time x;;

let modification_times x=
  (
   x.ml_modification_time,
   x.mli_modification_time,
   x.mll_modification_time,
   x.mly_modification_time
  );;

let make (nam,mlp,mlip,mllp,mlyp,mlmt,mlimt,mllmt,mlymt,libned,dirfath,allanc,dirned)=
  {
    name=nam;
    ml_present=mlp;
    mli_present=mlip;
    mll_present=mllp;
    mly_present=mlyp;
    ml_modification_time=mlmt;
    mli_modification_time=mlimt;
    mll_modification_time=mllmt;
    mly_modification_time=mlymt;
    needed_libraries=libned;
    direct_fathers=dirfath;
    all_ancestors=allanc;
    needed_directories=dirned;

};;

let compact_make (dir,nam,mlp,mlip,mllp,mlyp,mlmt,mlimt,mllmt,mlymt,libned,dirfath,allanc,dirned)=
  make (Half_dressed_module.of_string_and_root nam dir,
  		mlp,mlip,mllp,mlyp,
  		mlmt,mlimt,mllmt,mlymt,
  		Image.image Ocaml_library.of_string libned,
  		Image.image (fun s->Half_dressed_module.of_string_and_root s dir) dirfath,
  		Image.image (fun s->Half_dressed_module.of_string_and_root s dir) allanc,
  		Image.image Subdirectory.of_string dirned);;

let make_ml_present x=
 {
    name=x.name;
    ml_present=true;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;

};;

let make_mli_present x=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=true;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;


};;

let make_mll_present x=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=true;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;


};;


let make_mly_present x=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=true;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;


};;




let make_ml_absent x=
 {
    name=x.name;
    ml_present=false;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;

};;
  
let make_mli_absent x=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=false;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;

};;

let make_mll_absent x=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=false;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;

};;


let make_mly_absent x=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=false;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;

};;  
  
let check_presence ending dt=match ending with
   Ocaml_ending.Ml->dt.ml_present
  |Ocaml_ending.Mli->dt.mli_present
  |Ocaml_ending.Mll->dt.mll_present
  |Ocaml_ending.Mly->dt.mly_present;;  
  
let make_presence ending dt=match ending with
   Ocaml_ending.Ml->make_ml_present dt
  |Ocaml_ending.Mli->make_mli_present dt
  |Ocaml_ending.Mll->make_mll_present dt
  |Ocaml_ending.Mly->make_mly_present dt;;  

let make_absence ending dt=match ending with
   Ocaml_ending.Ml->make_ml_absent dt
  |Ocaml_ending.Mli->make_mli_absent dt
  |Ocaml_ending.Mll->make_mll_absent dt
  |Ocaml_ending.Mly->make_mly_absent dt;;  
  

let acolytes dt=
  let name=dt.name in
  Option.filter_and_unpack (fun 
    edg->
       if check_presence edg dt 
       then Some(Mlx_filename.join name edg)
       else None
  ) Ocaml_ending.all_endings;;
  

let registered_endings dt=
  List.filter (fun edg->
    check_presence edg dt 
  ) Ocaml_ending.all_endings;;

let short_paths dt=Image.image Mlx_filename.short_path (acolytes dt);;
  

let compute_modification_times hm=
  let dir=Half_dressed_module.root_directory hm in
  Ocaml_ending.exhaustive_uple (fun edg->
    let mlx=Mlx_filename.join hm edg in
    let file=(Directory_name.to_string dir)^(Mlx_filename.to_string mlx) in
    if not(Sys.file_exists file) then 0. else
    let st=Unix.stat file in
    st.Unix.st_mtime 
  );;

let rename1 new_name x=
   let (ml_mt,mli_mt,mly_mt,mll_mt)=compute_modification_times new_name in
   {
    name=new_name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=ml_mt;
    mli_modification_time=mli_mt;
    mll_modification_time=mll_mt;
    mly_modification_time=mly_mt;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;

   };;
   
let rename (old_name,new_name) x=
  if x.name=old_name
  then rename1 new_name x
  else if not(List.mem old_name (x.all_ancestors)) 
       then x
       else 
       let renamer=(fun t->if t=old_name then new_name else t) in
       let renamed_fathers=Image.image renamer x.direct_fathers
       and renamed_ancestors=Image.image renamer x.all_ancestors in
       let renamed_directories=Option.filter_and_unpack(
         fun mlx->
         let s=Half_dressed_module.to_string mlx in
         if String.contains s '/' 
         then Some(Subdirectory.of_string(Father_and_son.father s '/') )
         else None
       ) renamed_ancestors in
       {
    		name=x.name;
    		ml_present=x.ml_present;
   			mli_present=x.mli_present;
    		mll_present=x.mll_present;
    		mly_present=x.mly_present;
    		ml_modification_time=x.ml_modification_time;
    		mli_modification_time=x.mli_modification_time;
    		mll_modification_time=x.mll_modification_time;
    		mly_modification_time=x.mly_modification_time;
    		needed_libraries=x.needed_libraries;
    		direct_fathers=renamed_fathers;
    		all_ancestors=renamed_ancestors;
    		needed_directories=renamed_directories;
	   };;
       
let update_anclibdir changer l_data x=
   if not(List.mem changer.name (x.all_ancestors))
   then x
   else 
   let (anc,llib,dir)=(changer.all_ancestors,changer.needed_libraries,changer.needed_directories) in
   let new_ancestors=Option.filter_and_unpack(
     fun fd->
       let hm=name fd in
       if (List.mem hm x.all_ancestors)||(List.mem hm anc)
       then Some(hm)
       else None
   ) l_data in
   let new_lib=List.filter (
      fun lib->(List.mem lib llib)||(List.mem lib x.needed_libraries)
   ) Ocaml_library.all_libraries in
   let temp1=Option.filter_and_unpack(
     fun hm->
       let s_hm=Half_dressed_module.to_string hm in
       let s_dir=Father_and_son.father s_hm '/' in
       if s_dir="" then None else
       Some(Subdirectory.of_string s_dir)
   )  new_ancestors in
  let new_dir=Ordered.forget_order(Tidel.diforchan temp1) in
   {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=new_lib;
    direct_fathers=x.direct_fathers;
    all_ancestors=new_ancestors;
    needed_directories=new_dir;

   };;       
       
let is_optional x=Half_dressed_module.is_optional(x.name);;
let is_not_optional x=not(is_optional x);;

let is_executable x=Half_dressed_module.is_executable(x.name);;
let is_not_executable x=not(is_executable x);;

let compute_needed_directories l_md=
  let temp1=Image.image(
     fun md->Tidel.safe_set(md.needed_directories)
  ) l_md in
  let temp2=Tidel.big_teuzin temp1 in
  Ordered.forget_order temp2;;
  
let compute_needed_libraries l_md=
  List.filter 
  (
   fun lib->
   List.exists(fun md->List.mem lib md.needed_libraries)
     l_md
  ) 
  Ocaml_library.all_libraries;;  
  
let outdated_acolytes dt=
  let hm=dt.name in
  let (n_ml,n_mli,n_mll,n_mly)=compute_modification_times hm in
  let temp1=[
    Ocaml_ending.mll,dt.mll_modification_time,n_mll;
    Ocaml_ending.mly,dt.mly_modification_time,n_mly;
    Ocaml_ending.ml ,dt.ml_modification_time,n_ml  ;
    Ocaml_ending.mli,dt.mli_modification_time,n_mli;
  ] in
  Option.filter_and_unpack (
    fun (edg,x,y)->
      if x<>y
      then Some(Mlx_filename.join hm edg)
      else None
  ) temp1;;
 
let is_outdated  dt=((outdated_acolytes  dt)<>[]);;


let force_ml_modification_time x new_val=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=new_val;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;

};;


let force_mli_modification_time x new_val=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=new_val;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;

};;

let force_mll_modification_time x new_val=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=new_val;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;

};;

let force_mly_modification_time x new_val=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=new_val;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;

};;

let force_modification_time x edg new_val=match edg with
   Ocaml_ending.Ml->force_ml_modification_time x new_val
  |Ocaml_ending.Mli->force_mli_modification_time x new_val
  |Ocaml_ending.Mll->force_mll_modification_time x new_val
  |Ocaml_ending.Mly->force_mly_modification_time x new_val;; 


let fix_ancestors_and_libs_and_dirs x anc=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=compute_needed_libraries(x::anc);
    direct_fathers=x.direct_fathers;
    all_ancestors=Image.image (fun md->md.name) anc;
    needed_directories=compute_needed_directories(x::anc);
};;

let fix_ancestors x anc=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=anc;
    needed_directories=x.needed_directories;
};;




let needed_dirs_and_libs is_optimized dt=
   let extension=(if is_optimized then ".cmxa" else ".cma") in
   let dirs=String.concat(" ")
    (Image.image(fun y->let z=Subdirectory.to_string(y) in
     if z="" then "" else "-I "^z )
    dt.needed_directories)
	and libs=String.concat(" ")
    (Image.image(fun z->Ocaml_library.file_for_library(z)^extension)
    dt.needed_libraries) in
    String.concat " " ["";dirs;libs;""];;

let needed_dirs_and_libs_for_several is_optimized l_dt=
   let extension=(if is_optimized then ".cmxa" else ".cma") in
   let pre_dirs1=Image.image (fun dt->Tidel.diforchan(dt.needed_directories)) l_dt in
   let pre_dirs2=Ordered.forget_order (Tidel.big_teuzin pre_dirs1) in
   let dirs=String.concat(" ")
    (Image.image(fun y->let z=Subdirectory.to_string(y) in 
    if z="" then "" else "-I "^z )
    pre_dirs2) in
   let pre_libs1=Image.image (fun dt->Tidel.diforchan(dt.needed_libraries)) l_dt in
   let pre_libs2=Ordered.forget_order (Tidel.big_teuzin pre_libs1) in 
   let libs=String.concat(" ")
    (Image.image(fun z->Ocaml_library.file_for_library(z)^extension)
    pre_libs2) in
    String.concat " " ["";dirs;libs;""];;

let principal_mlx x=
   if x.mll_present then Mlx_filename.join x.name Ocaml_ending.mll else
   if x.mly_present then Mlx_filename.join x.name Ocaml_ending.mly else
   if x.ml_present then Mlx_filename.join x.name Ocaml_ending.ml else
   Mlx_filename.join x.name Ocaml_ending.mli;;
   
let principal_path x=Mlx_filename.to_path (principal_mlx x);;  

let ml_path x=Mlx_filename.to_path (Mlx_filename.join x.name Ocaml_ending.ml);;   

let unprefixed_compact_ocaml_name x=
   let enc=Strung.enclose in
   let (s,dir)=Half_dressed_module.unveil x.name in
  "("^
  (Directory_name.ocaml_name dir)^","^
  (enc(s))^","^
  (string_of_bool x.ml_present)^","^
  (string_of_bool x.mli_present)^","^
  (string_of_bool x.mll_present)^","^
  (string_of_bool x.mly_present)^","^
  (string_of_float x.ml_modification_time)^","^
  (string_of_float x.mli_modification_time)^","^
  (string_of_float x.mll_modification_time)^","^
  (string_of_float x.mly_modification_time)^","^
  "["^(String.concat ";" (Image.image (fun w->enc(Ocaml_library.to_string w)) x.needed_libraries))^"],"^
  "["^(String.concat ";" (Image.image (fun w->enc(Half_dressed_module.to_string w)) x.direct_fathers))^"],"^
  "["^(String.concat ";" (Image.image (fun w->enc(Half_dressed_module.to_string w)) x.all_ancestors))^"],"^
  "["^(String.concat ";" (Image.image (fun w->enc(Subdirectory.to_string w)) x.needed_directories))^"]"^
  ")";;    
  
let compact_ocaml_name x=
  "New_modulesystem"^"_data.compact_make"^
  (unprefixed_compact_ocaml_name x);;
     
  
let ocaml_name x=
   let (s,dir)=Half_dressed_module.unveil x.name in
  "New_modulesystem"^"_data.make("^
  (Directory_name.ocaml_name dir)^","^
  (Strung.enclose s)^","^
  (string_of_bool x.mli_present)^","^
  (string_of_bool x.mli_present)^","^
  (string_of_bool x.mll_present)^","^
  (string_of_bool x.mly_present)^","^
  (string_of_float x.ml_modification_time)^","^
  (string_of_float x.mli_modification_time)^","^
  (string_of_float x.mll_modification_time)^","^
  (string_of_float x.mly_modification_time)^","^
  "["^(String.concat ";" (Image.image Ocaml_library.ocaml_name x.needed_libraries))^"],"^
  "["^(String.concat ";" (Image.image Half_dressed_module.ocaml_name x.direct_fathers))^"],"^
  "["^(String.concat ";" (Image.image Half_dressed_module.ocaml_name x.all_ancestors))^"],"^
  "["^(String.concat ";" (Image.image Subdirectory.ocaml_name x.needed_directories))^"]"^
  ")";;  

let directories_from_list l=
  let temp2=Image.image (
    fun dt->
       let hm=name dt in
       Half_dressed_module.subdirectory hm
  ) l in
  let temp3=Tidel.diforchan(temp2) in
  Ordered.forget_order temp3;;

  
let industrial_separator1=Industrial_separator.new_separator ();;  
let industrial_separator2=Industrial_separator.new_separator ();;    

  
  
let archive x=
   String.concat industrial_separator1
   [
     Half_dressed_module.archive x.name;
     string_of_bool x.ml_present;
     string_of_bool x.mli_present;
     string_of_bool x.mll_present;
     string_of_bool x.mly_present;
     string_of_float x.ml_modification_time;
     string_of_float x.mli_modification_time;
     string_of_float x.mll_modification_time;
     string_of_float x.mly_modification_time;
     Nonblank.make(String.concat industrial_separator2 (Image.image Ocaml_library.to_string x.needed_libraries));
     Nonblank.make(String.concat industrial_separator2 (Image.image Half_dressed_module.to_string x.direct_fathers));
     Nonblank.make(String.concat industrial_separator2 (Image.image Half_dressed_module.to_string x.all_ancestors));
     Nonblank.make(String.concat industrial_separator2 (Image.image Subdirectory.unveil x.needed_directories));
   ];;

let unarchive s=
   let l1=Str.split (Str.regexp_string industrial_separator1) s in
   let v1=Str.split (Str.regexp_string industrial_separator2) (Nonblank.decode(List.nth l1  9))
   and v2=Str.split (Str.regexp_string industrial_separator2) (Nonblank.decode(List.nth l1 10))
   and v3=Str.split (Str.regexp_string industrial_separator2) (Nonblank.decode(List.nth l1 11))
   and v4=Str.split (Str.regexp_string industrial_separator2) (Nonblank.decode(List.nth l1 12)) in
   let hm=Half_dressed_module.unarchive(List.hd l1) in
   let dir=Half_dressed_module.root_directory hm in
{
    name = hm;
    ml_present  = bool_of_string(List.nth l1 1);
    mli_present = bool_of_string(List.nth l1 2);
    mll_present = bool_of_string(List.nth l1 3);
    mly_present = bool_of_string(List.nth l1 4);
    ml_modification_time = float_of_string(List.nth l1 5);
    mli_modification_time = float_of_string(List.nth l1 6);
    mll_modification_time = float_of_string(List.nth l1 7);
    mly_modification_time = float_of_string(List.nth l1 8);
    needed_libraries =Image.image Ocaml_library.of_string v1;
    direct_fathers = Image.image (fun s->Half_dressed_module.of_string_and_root s dir) v2;
    all_ancestors = Image.image (fun s->Half_dressed_module.of_string_and_root s dir) v3;
    needed_directories = Image.image Subdirectory.of_string v4;
};;
     


end;;






module Ocaml_target=struct

(*

A make-style type for compiler management.
There are built-in targets, plus a variant for
manually built special targets.

Note that  when the ml file is present, the ocamlc -c command produces the
.cmi and .cmo at the same time, so that there is no need to compile the
.cmi separately.

#use"Makefile_makers/ocaml_target.ml";;

*)

type target_name=string;;

type t=
  NO_DEPENDENCIES of Mlx_filename.t
 |ML_FROM_MLL of Half_dressed_module.t
 |ML_FROM_MLY of Half_dressed_module.t 
 |CMI of Half_dressed_module.t
 |CMO of Half_dressed_module.t
 |DCMO of Half_dressed_module.t
 |CMA of Half_dressed_module.t
 |CMX of Half_dressed_module.t
 |EXECUTABLE of Half_dressed_module.t
 |DEBUGGABLE of Half_dressed_module.t
 |TOPLEVEL of target_name*(Half_dressed_module.t list);;
 
 
let to_string =function
  NO_DEPENDENCIES(mlx)->Mlx_filename.to_string mlx
 |ML_FROM_MLL(hm)->(Half_dressed_module.to_string hm)^".ml"
 |ML_FROM_MLY(hm)->(Half_dressed_module.to_string hm)^".ml" 
 |CMI(hm)->(Half_dressed_module.to_string hm)^".cmi"
 |CMO(hm)->(Half_dressed_module.to_string hm)^".cmo"
 |DCMO(hm)->(Half_dressed_module.to_string hm)^".d.cmo"
 |CMA(hm)->(Half_dressed_module.to_string hm)^".cma"
 |CMX(hm)->(Half_dressed_module.to_string hm)^".cmx"
 |EXECUTABLE(hm)->(Half_dressed_module.to_string hm)^".caml_executable"
 |DEBUGGABLE(hm)->(Half_dressed_module.to_string hm)^".caml_debuggable"
 |TOPLEVEL(name,l)->name;;

let test_path dir tgt=
  let d=Directory_name.to_string dir in
  Sys.file_exists(d^(to_string tgt));; 
 
let path dir tgt=
 let d=Directory_name.to_string dir in
 Absolute_path.of_string(d^(to_string tgt));;

let is_a_debuggable=function
  DEBUGGABLE(_)->true
 |_->false;;

let is_not_a_debuggable x=not(is_a_debuggable x);; 
 

let toplevel_data=function
  NO_DEPENDENCIES(mlx)->None
 |ML_FROM_MLL(hm)-> None
 |ML_FROM_MLY(hm)-> None 
 |CMI(hm)-> None
 |CMO(hm)-> None
 |DCMO(hm)-> None
 |CMA(hm)-> None
 |CMX(hm)-> None
 |EXECUTABLE(hm)-> None
 |DEBUGGABLE(hm)-> None
 |TOPLEVEL(name,l)->Some(name,l);;

let toplevel_name tgt=match toplevel_data tgt with
None->None |Some(name,_)->Some(name);;

let is_a_toplevel tgt=match toplevel_data tgt with
None->false |Some(_,_)->true;;

let is_a_nodep tgt=function
  NO_DEPENDENCIES(_)->true
  |_->false;;

let adhoc_test_for_renaming old_name=function
  NO_DEPENDENCIES(mlx)->(Mlx_filename.half_dressed_core mlx)<>old_name
 |_->true;;

let naive_main_module=function
  NO_DEPENDENCIES(mlx)->Some(Mlx_filename.half_dressed_core mlx)
 |ML_FROM_MLL(hm)-> Some(hm)
 |ML_FROM_MLY(hm)-> Some(hm) 
 |CMI(hm)-> Some(hm)
 |CMO(hm)-> Some(hm)
 |DCMO(hm)-> Some(hm)
 |CMA(hm)-> Some(hm)
 |CMX(hm)-> Some(hm)
 |EXECUTABLE(hm)-> Some(hm)
 |DEBUGGABLE(hm)-> Some(hm)
 |TOPLEVEL(name,l)->None;;

let main_module tgt=try naive_main_module tgt with _->None;;

let no_dependencies mlx=NO_DEPENDENCIES(mlx);;
let ml_from_mll hm=ML_FROM_MLL(hm);; 
let ml_from_mly hm=ML_FROM_MLY(hm);;
let cmi hm=CMI(hm);;
let cmo hm=CMO(hm);;
let dcmo hm=DCMO(hm);;
let cma hm=CMA(hm);; 
let cmx hm=CMX(hm);;
let executable hm=EXECUTABLE(hm);; 
let debuggable hm=DEBUGGABLE(hm);; 
let toplevel name l=TOPLEVEL(name,l);;

let direct_connection hm0=function
  NO_DEPENDENCIES(mlx)->(Mlx_filename.half_dressed_core mlx)=hm0
 |ML_FROM_MLL(hm)-> hm=hm0
 |ML_FROM_MLY(hm)-> hm=hm0
 |CMI(hm)-> hm=hm0
 |CMO(hm)-> hm=hm0
 |DCMO(hm)->hm=hm0
 |CMA(hm)-> hm=hm0
 |CMX(hm)-> hm=hm0
 |EXECUTABLE(hm)-> hm=hm0
 |DEBUGGABLE(hm)-> hm=hm0
 |TOPLEVEL(name,l)->List.mem hm0 l;;


 
let ml_from_lex_or_yacc_data=function 
    ML_FROM_MLL(hm)->Some(Mlx_filename.join hm Ocaml_ending.ml)
   |ML_FROM_MLY(hm)->Some(Mlx_filename.join hm Ocaml_ending.ml)
   |_->None;;
 
let complexity_level=function
  NO_DEPENDENCIES(_)->0 
 |ML_FROM_MLL(_)
 |ML_FROM_MLY(_)->1
 |CMI(_)
 |CMO(_)
 |DCMO(_)
 |CMA(_)
 |CMX(_)->2
 |EXECUTABLE(_)
 |DEBUGGABLE(_)
 |TOPLEVEL(_,_)->3;;

let sliced_ocaml_name tgt=
  let sl=Sliced_string.of_string_list in
  match tgt with
  NO_DEPENDENCIES(mlx)-> sl ["Ocaml"^"_target"^".no_dependencies ("^(Mlx_filename.ocaml_name mlx)^")"]
 |ML_FROM_MLL(hm)-> sl ["Ocaml"^"_target"^".ml_from_mll ("^(Half_dressed_module.ocaml_name hm)^")"]
 |ML_FROM_MLY(hm)-> sl ["Ocaml"^"_target"^".ml_from_mly ("^(Half_dressed_module.ocaml_name hm)^")"]
 |CMI(hm)-> sl ["Ocaml"^"_target"^".cmi ("^(Half_dressed_module.ocaml_name hm)^")"]
 |CMO(hm)-> sl ["Ocaml"^"_target"^".cmo ("^(Half_dressed_module.ocaml_name hm)^")"]
 |DCMO(hm)->sl ["Ocaml"^"_target"^".dcmo("^(Half_dressed_module.ocaml_name hm)^")"]
 |CMA(hm)-> sl ["Ocaml"^"_target"^".cma ("^(Half_dressed_module.ocaml_name hm)^")"]
 |CMX(hm)-> sl ["Ocaml"^"_target"^".cmx ("^(Half_dressed_module.ocaml_name hm)^")"]
 |EXECUTABLE(hm)-> sl ["Ocaml"^"_target"^".executable ("^(Half_dressed_module.ocaml_name hm)^")"]
 |DEBUGGABLE(hm)-> sl ["Ocaml"^"_target"^".debuggable ("^(Half_dressed_module.ocaml_name hm)^")"]
 |TOPLEVEL(name,l)->
   let temp1=Image.image 
   (fun hm->(Half_dressed_module.ocaml_name hm)^";") l in
   let temp2=Sliced_string.make_aggregates "" ("["::temp1@["])"]) in
 	Sliced_string.concat_two 
 	(Sliced_string.of_string_list
 	(["(Ocaml"^"_target"^".toplevel "^(Strung.enclose name)^" "]))
 	temp2;;
 

let ocaml_name tgt=Sliced_string.print (sliced_ocaml_name tgt);;
  
let still_up_to_date_test hms_to_be_updated=function
   TOPLEVEL(name,l_hm)->List.for_all
                    (
                      fun hm2->
                      not(List.mem hm2 hms_to_be_updated)
                    ) l_hm
  |tgt2->not(List.mem (Option.unpack(main_module tgt2)) hms_to_be_updated);;
  
let  still_up_to_date_targets hms_to_be_updated l=
  List.filter (
     still_up_to_date_test hms_to_be_updated
  ) l;; 
  
let from_modulesystem_data dt=
  let hm=Modulesystem_data.name dt 
  and (mlp,mlip,mllp,mlyp)=Modulesystem_data.presences dt in
  let temp1=[
                mllp,ml_from_mll hm;
                mlyp,ml_from_mly hm;
           mlp||mlip,cmi hm;
     	   mlp||mlip,cmo hm;
           mlp||mlip,cma hm;
           mlp||mlip,cmx hm;
                 mlp,executable hm;
  ] in
Option.filter_and_unpack (fun x->if fst x then Some(snd x) else None) temp1;;  

  
  
let industrial_separator1=Industrial_separator.new_separator ();;  
let industrial_separator2=Industrial_separator.new_separator ();;    


let prepare_archive=function
  NO_DEPENDENCIES(mlx)->["nodep";Mlx_filename.archive mlx]
 |ML_FROM_MLL(hm)-> ["mll";Half_dressed_module.archive hm]
 |ML_FROM_MLY(hm)-> ["mly";Half_dressed_module.archive hm]  
 |CMI(hm)->  ["cmi";Half_dressed_module.archive hm]
 |CMO(hm)->  ["cmo";Half_dressed_module.archive hm]
 |DCMO(hm)-> ["dcmo";Half_dressed_module.archive hm]
 |CMA(hm)->  ["cma";Half_dressed_module.archive hm]
 |CMX(hm)->  ["cmx";Half_dressed_module.archive hm]
 |EXECUTABLE(hm)-> ["exe";Half_dressed_module.archive hm]
 |DEBUGGABLE(hm)-> ["dbg";Half_dressed_module.archive hm]
 |TOPLEVEL(name,l)->["top";name;
  Nonblank.make(String.concat industrial_separator1 (Image.image Half_dressed_module.archive l))];;
  
  
  
exception Unrecognized_constructor of string;;   
  
let archive x=String.concat industrial_separator2 (prepare_archive x);;
 

let unarchive s=
   let l1=Str.split (Str.regexp_string industrial_separator2) s in
   let c=List.hd l1 and ms=List.nth l1 1 in
   if c="nodep" then NO_DEPENDENCIES(Mlx_filename.unarchive ms) else
   if c="mll"  then  ML_FROM_MLL(Half_dressed_module.unarchive ms) else
   if c="mly"  then  ML_FROM_MLY(Half_dressed_module.unarchive ms) else
   if c="cmi"  then          CMI(Half_dressed_module.unarchive ms) else
   if c="cmo"  then          CMO(Half_dressed_module.unarchive ms) else
   if c="dcmo" then         DCMO(Half_dressed_module.unarchive ms) else
   if c="cma"  then          CMA(Half_dressed_module.unarchive ms) else
   if c="cmx"  then          CMX(Half_dressed_module.unarchive ms) else
   if c="exe"  then   EXECUTABLE(Half_dressed_module.unarchive ms) else
   if c="dbg"  then   DEBUGGABLE(Half_dressed_module.unarchive ms) else
   if c="top" 
   then let v1=Str.split (Str.regexp_string industrial_separator1) (Nonblank.decode(List.nth l1  2)) in 
        TOPLEVEL(ms,Image.image Half_dressed_module.unarchive v1) 
   else
   raise(Unrecognized_constructor(c));;

end;;






module Debugger_name=struct

(*

#use"debugger_name.ml";;

*)


let debugger_name="debugger";;

end;;






module Alaskan_data=struct


(* 

#use"Country/Alaska/alaskan_data.ml";;


*)

let all_mlx_files mdata=
  List.flatten
  (Image.image Modulesystem_data.acolytes mdata);; 

let all_mlx_paths mdata=Image.image Mlx_filename.to_absolute_path 
  (all_mlx_files mdata);;  

let all_short_paths mdata=List.flatten(
  Image.image Modulesystem_data.short_paths mdata
);;

let default_toplevel main_toplevel_name mdata=
  let temp2=List.filter Modulesystem_data.is_not_optional mdata in
  let temp3=Image.image Modulesystem_data.name temp2 in
  let temp4=List.filter (fun hm->
     Half_dressed_module.to_string(hm)<>Debugger_name.debugger_name
  ) temp3
  in
  Ocaml_target.toplevel main_toplevel_name temp4;; 
 
let find_module_registration mdata hm=
  Option.find_it(fun a->Modulesystem_data.name a=hm) mdata;;   

end;;






module Shell_command=struct

(*

#use"shell_command.ml";;

*)



type dir_string=string;;
type command_string=string;;
type announce_string=string;;
type failure_string=string;;

type command=
  C of command_string*announce_string*failure_string
 |ChangeDir of dir_string*announce_string*failure_string;;


let of_strings s ann fai=C (s,ann,fai);;
let change_dir s_dir ann fai=ChangeDir(s_dir,ann,fai);;

let do_and_notice_failure s=
 let i=Sys.command s in
 if i<>0
 then print_string("Failed during "^s);flush stdout;;

let unveil=function
 (C (s,ann,fai))->(s,ann,fai)
 |ChangeDir(dir_string,ann,fai)->("cd "^dir_string,ann,fai);;

exception Content_of_cd of string;;

let command_content=function 
 (C (s,ann,fai))->s
 |ChangeDir(dir_string,ann,fai)->raise(Content_of_cd(dir_string));;

let usual s=C(s,"","Failed during "^s);;

let semi_usual (s,t)=C(s,"","Failed during "^t);;

let usual_change_dir s=ChangeDir(s,"","Failed during cd "^s);;

let cd (d:dir_string)=(try (fun _->0)(Sys.chdir(d)) with 
   _->2);;
 

let execute_without_commenting=function
 (C (s,ann,fai))->Sys.command s
 |ChangeDir(dir_string,ann,fai)->cd dir_string;;

let print_if_nonempty s=
  if s="" 
  then ()
  else print_string (s^"\n");flush stdout;;

let minimal_announce_and_do=function
 (C(s,ann,fai))->
  let _=print_if_nonempty ann in
  let bowl=((Sys.command s)=0) in
  let _=(if not(bowl) then print_if_nonempty fai) in
  bowl
 |(ChangeDir(dir_string,ann,fai))->
  let _=print_if_nonempty ann in
  let bowl=((cd dir_string)=0) in
  let _=(if not(bowl) then print_if_nonempty fai) in
  bowl ;;

let maximal_announce_and_do=function
 (C(s,ann,fai))->
  let _=print_if_nonempty s in
  ((Sys.command s)=0)
 |(ChangeDir(dir_string,ann,fai))->
  let _=print_if_nonempty ("cd "^dir_string) in
  ((cd dir_string)=0);;

let display_all_commands=ref(false);;

let announce_and_do x=
  if (!display_all_commands)
  then maximal_announce_and_do x
  else minimal_announce_and_do x;;
    
let take_care_of_root_directory root l=
  let s_root=Directory_name.to_string root 
  and s_cwd=Sys.getcwd() in
  if Absolute_path.test_equal_paths s_root s_cwd
  then l
  else (usual_change_dir s_root)::
     (l@[usual_change_dir s_cwd]);;     
    
let rec try_successively l=match l with
[]->true
|x::others->
    if announce_and_do x
    then try_successively others
    else false;;    
    


end;;






module My_str=struct

(*

An adptation of OCaml's Str module :

String indices are now from 1 to n instead of 0 to (n-1).
Operations on regexps are encoded as functions in the module.

#use"my_str.ml";;

*)

type backtrack_length=int;;

type regexp=M of string*(Str.regexp)*backtrack_length;;

let unveil (M(s,_,b))=s;;
let veil s=M(s,Str.regexp s,0);;

let set_backtrack (b:backtrack_length) (M(s,rgxp,_))=M(s,rgxp,b);;

let regexp_string s=let quote=Str.quote s in veil quote;;

let plus (M (s,_,_))=let new_s="\\("^s^"\\)+" in veil new_s;;
let star (M (s,_,_))=let new_s="\\("^s^"\\)*" in veil new_s;;

let concat (M(s1,_,_)) (M(s2,_,_))=veil (s1^s2);;
   
let big_concat l=
  if l=[]
  then veil ""
  else let temp1=List.map (fun w->"\\("^(unveil w)^"\\)") l in
       let new_s=String.concat "" temp1 in
       veil new_s;;   

let big_or l=
  let temp1=List.map (fun (M(s,_,_))->"\\("^s^"\\)") l in
  let new_s=String.concat "\\|" temp1 in
  veil new_s;;

let ore a b=big_or [a;b];;

let string_match (M(_,rgxp,b)) s i0=
   let bowl=(Str.string_match rgxp s (i0-1)) in
   if bowl
   then Some(i0+(String.length(Str.matched_string s))-b-1)
   else None;;
   
(* Functions from the option or ennig s *)  

let index_everything l=
  let rec tempf=
   (function (j,graet,da_ober)->
     match da_ober with
      []->graet
     |a::b->tempf(j-1,(j,a)::graet,b)
    )    in
    tempf(List.length(l),[],List.rev(l));;

let unpack=function
None->failwith("Void is not unpackable")
|Some(x)->x;;   
   
let rec find_and_stop f l=
 let rec find_and_stop0=(function
  da_ober->match da_ober with
   []->None
   |a::peurrest->match f(a) with
		None->find_and_stop0(peurrest)
		|Some(x)->Some(x)
 ) in
 find_and_stop0(l);;
   
(* End of functions from the option or ennig modules *)     
      
   
type left_regexp=regexp;;
type center_regexp=regexp;;   
type right_regexp=regexp;;   
   
type centered_regexp=left_regexp*center_regexp*right_regexp;;   
 
let create_centered_regexp a b c=((a,b,c):centered_regexp);;   
   
let centered_regexp_match ((a,b,c):centered_regexp) s i0=
  let opt1=string_match a s i0 in
  if opt1=None then None else
  let i1=unpack(opt1)+1 in
  let opt2=string_match b s i1 in
  if opt2=None then None else   
  let i2=unpack(opt2)+1 in 
  let opt3=string_match c s i2 in
  if opt3=None then None else   
  Some(i1,i2-1);;
   
 
let centered_regexp_list_match l s i0=
  let temp1=index_everything l in
  find_and_stop(fun (i,rgxp)->
    match centered_regexp_match rgxp s i0 with
     Some(j,k)->Some(i,(j,k))
    |None->None
  ) temp1;;   
   
 let find_all_occurrences l s i0=
  let n=String.length s in
  let rec tempf=(fun (graet,j)->
    if j>n then List.rev(graet) else
    let opt=centered_regexp_list_match l s j in
    if opt=None then tempf(graet,j+1) else
    let (k,(l,m))=unpack(opt) in
    tempf((k,(l,m))::graet,m+2)
  ) in
  tempf([],i0);;

   
   
 (*  
 
 centered_regexp_match (veil "12.",veil"4.6",veil"78") "123456789abcdef" 1;;
 
 let s1="123456789abcdef";;
 let a1=veil "12." and b1=veil"4.6" and c1=veil"78";;
 
 let w1=string_match a1 s1 1;;
 
 
 
 string_match (regexp_string "456") "123456789abcdef" 4;; 

let test_centered_regexp  
 
let search_forward (M(_,rgxp,b)) s i0=
   let j1=(Str.search_forward rgxp s (i0-1))+1 in
   let j2=j1+(String.length(Str.matched_string s))-b-1 in
   (j1,j2);;
  *) 

end;;






module My_str_example=struct

(*

Concrete values of type My_str.regexp.

#use"my_str_example.ml";;

*)

let capital_letter=My_str.veil "[A-Z]";;
let letters=My_str.veil "[A-Za-z1-9_']*";;
let nonletter=My_str.veil "[^A-Za-z1-9_']";;
let white=My_str.veil "[ \n\r\t]";;
let maybe_whites=My_str.star white;;
let some_whites=My_str.plus white;;
 

let delimited_module_name=My_str.big_concat
  [
    nonletter;capital_letter;letters;nonletter
  ];;

let bare_module_name=My_str.big_concat
  [
    capital_letter;letters
  ];;

let include_case=
  let left_part=My_str.veil "[ \n\r\t]+include[ \n\r\t(]+"
  and center_part=bare_module_name 
  and right_part=nonletter in
  My_str.create_centered_regexp left_part center_part right_part;; 

let open_case=
  let left_part=My_str.veil "[ \n\r\t]+open[ \n\r\t(]+"
  and center_part=bare_module_name 
  and right_part=nonletter in
  My_str.create_centered_regexp left_part center_part right_part;; 

let moodle_case=
  let left_part=My_str.big_concat 
  [some_whites;My_str.veil"module";some_whites;
   bare_module_name;My_str.veil"=";maybe_whites]
  and center_part=bare_module_name 
  and right_part=nonletter in
  My_str.create_centered_regexp left_part center_part right_part;; 

let pointed_case=
  let left_part=nonletter
  and center_part=bare_module_name 
  and right_part=My_str.regexp_string "." in
  My_str.create_centered_regexp left_part center_part right_part;; 

let moodle_cases=[include_case;open_case;moodle_case;pointed_case];;
let index_for_include_case=1;;
let index_for_open_case=2;;
let index_for_moodle_case=3;;
let index_for_pointed_case=4;;


(*
My_str.centered_regexp_match include_case " include Peggy;; " 1;;
My_str.centered_regexp_match include_case " include_once;; " 1;;
My_str.centered_regexp_match moodle_case " module Amy=Lawson " 1;;
My_str.centered_regexp_match pointed_case " 57+Everybody.talking-78 " 4;;
*)

 let capital_letter=My_str.veil "[A-Z]";;
 
 let alphanumeric=
    My_str.big_or
      [ 
     	My_str.veil "[a-z]";
     	My_str.veil "[A-Z]";
     	My_str.veil "[0-9]";
     	My_str.regexp_string "_";
      ];;
 
 let alphanumerics=My_str.plus alphanumeric;;
 
 let beginning_of_module_definition=
    My_str.set_backtrack 1
    (My_str.big_concat
      [
         white;
         My_str.regexp_string "module";
         some_whites;
         capital_letter;
         alphanumerics;
         some_whites;
         My_str.regexp_string "=";
         some_whites;
         My_str.regexp_string "struct";
         white;
          
      ]);;
      
 let beginning_of_module_reminder=
    My_str.set_backtrack 1
    (My_str.big_concat
      [
         white;
         My_str.regexp_string "module";
         some_whites;
         capital_letter;
         alphanumerics;
         some_whites;
         My_str.regexp_string ":";
         some_whites;
         My_str.regexp_string "sig";
         white;
          
      ]);;
      
 let beginning_of_module_type_definition=
    My_str.set_backtrack 1
    (My_str.big_concat
      [
         white;
         My_str.regexp_string "module";
         some_whites;
         My_str.regexp_string "type";
         some_whites;
         capital_letter;
         alphanumerics;
         some_whites;
         My_str.regexp_string "=";
         some_whites;
         My_str.regexp_string "sig";
         white;
          
      ]);;           
      
      
 let the_end=
    My_str.set_backtrack 1
    (My_str.big_concat
      [
         white;
         My_str.regexp_string "end";
         white;
          
      ]);;       
      

end;;






module Look_for_module_names=struct

(*

#use"look_for_module_names.ml";;

*)

let indices_in_string s=
  let temp1=Outside_comments_and_strings.good_substrings s in
  let temp2=Image.image (fun (a,b,t)->
     let ttemp3=My_str.find_all_occurrences My_str_example.moodle_cases t 1 in
     Image.image (fun (case_index,(u,v))->
        (case_index,(u+a-1,v+a-1))
     ) ttemp3
  ) temp1 in
  List.flatten temp2;;

let names_in_string z=
  let temp1=indices_in_string z in
  let temp2=Image.image (fun (_,(a,b))->String.sub z (a-1) (b-a+1) ) temp1 in
  let temp3=Three_parts.generic temp2 in
  let temp4=List.filter (fun (x,y,z)->not(List.mem y x)) temp3 in
  let temp5=Image.image (fun (x,y,z)->Naked_module.of_string (String.uncapitalize_ascii y)) temp4 in
  temp5;;

let indices_in_file file=indices_in_string(Io.read_whole_file file);;  
let names_in_file file=names_in_string(Io.read_whole_file file);;

type module_name=string;;

let change_module_name_in_string
   (old_name:module_name)
   (new_name:module_name) s=
   let itv=(fun a b->String.sub s (a-1) (b-a+1)) in
   let temp1=indices_in_string s in
   let temp2=List.filter (fun (j,(a,b))->(itv a b)=old_name ) temp1 in
   if temp2=[]
   then s
   else
   let (_,(a1,b1))=List.hd(temp2) in
   let rec sub_f=(fun (graet,ax,bx,da_ober)->
     match da_ober with
      []->List.rev((itv (bx+1) (String.length s))::graet)
     |(_,(ay,by))::peurrest->
       let s1=itv (bx+1) (ay-1) in
       sub_f(new_name::s1::graet,ay,by,peurrest)
   ) in
   let temp3=sub_f([new_name;itv 1 (a1-1)],a1,b1,List.tl(temp2)) in
   String.concat "" temp3;;
   
 let change_module_name_in_file old_name new_name file=
   let s=Io.read_whole_file file in
   let new_s=change_module_name_in_string old_name new_name s in
   Io.erase_file_and_fill_it_with_string file new_s;;  
   
(*   
   
indices_in_string "123 Haag.012 open Garfield;8";;

indices_in_string "(* Haag. *)234 Dog.\"open Garfield;\"67 Corn.4";;

   
*)   

end;;






module Read_info_on_file_in_system=struct


(* 


Recompute the characteristics of a module
stored in memory.

#use"Makefile_makers/read_info_on_file_in_system.ml";;


*)


let find_needed_data_for_file l fn=
   let temp1=Look_for_module_names.names_in_file fn in
   let selecter=(fun info->
     let hm=Modulesystem_data.name info in
     let name=Half_dressed_module.undress hm in
     if List.mem name temp1
     then Some(info)
     else None
   ) in
   Option.filter_and_unpack selecter l;;

let find_needed_data l mlx=
   let fn=Mlx_filename.to_path mlx in
   find_needed_data_for_file l fn;;

let find_needed_names l mlx=
   let temp1=find_needed_data l mlx in
   Image.image Modulesystem_data.name temp1;;

 let find_needed_libraries mlx genealogy=
   let fn=Mlx_filename.to_path mlx in
   let temp1=Look_for_module_names.names_in_file fn in
   List.filter
   (
     fun lib->
       if List.exists 
          (fun mdl->List.mem(Naked_module.of_string mdl)(temp1))
            (Ocaml_library.modules_telling_a_library_away lib)
       then true
       else List.exists 
            (fun info->List.mem lib (Modulesystem_data.needed_libraries info) ) 
            genealogy
   )
   Ocaml_library.all_libraries;;

 let find_needed_directories mlx genealogy=
   let temp1=Image.image 
     (fun t->Tidel.diforchan(Modulesystem_data.needed_directories t)) 
       genealogy in
   let s_mlx=Mlx_filename.to_string mlx in
   let temp2=(fun bowl->
       if bowl 
       then let new_subdir=Subdirectory.of_string(Father_and_son.father s_mlx '/') in
            Tidel.singleton(new_subdir)::temp1
       else temp1
   )(String.contains s_mlx '/') in    
   let temp3=Tidel.big_teuzin temp2 in
   Ordered.forget_order temp3;;
 
 let check_presences l hm=
 match Option.find_it (fun a->Modulesystem_data.name a=hm) l with
    None->Ocaml_ending.exhaustive_uple (fun _->false)
    |Some(dt)->Ocaml_ending.exhaustive_uple 
     (fun edg->Modulesystem_data.check_presence edg dt);;
 
 
 let complete_info l mlx=
   let (hm,edg)=Mlx_filename.decompose mlx in
   let genealogy=find_needed_data l mlx in
   let (mlp,mlip,mllp,mlyp)=check_presences l hm
   and (mlmt,mlimt,mllmt,mlymt)=Modulesystem_data.compute_modification_times hm in
   let dirfath=Image.image (Modulesystem_data.name) genealogy in
   let temp1=Image.image 
   			(fun t->Tidel.diforchan(Modulesystem_data.all_ancestors t)) 
   			genealogy in
   let temp2=Tidel.big_teuzin ((Tidel.diforchan(dirfath) )::temp1) in
   let tempf=(fun t->
   					let nam_t=Modulesystem_data.name t in
   					if Tidel.elfenn nam_t temp2
   					then Some(nam_t)
   					else None) in
   let allanc=Option.filter_and_unpack tempf l in
   let libned=find_needed_libraries mlx genealogy
   and dirned=find_needed_directories mlx genealogy in
   Modulesystem_data.make
   (hm,mlp,mlip,mllp,mlyp,mlmt,mlimt,mllmt,mlymt,libned,dirfath,allanc,dirned);;
   
let recompute_complete_info_for_module l hm=
  let opt=Option.find_it(fun a->Modulesystem_data.name a=hm) l in
  let dt=Option.unpack opt in
  let edg=List.hd(Modulesystem_data.registered_endings dt) in
  let mlx=Mlx_filename.join hm edg in
  complete_info l mlx;;
        
    
let quick_update l x=
  let hm=Modulesystem_data.name (x) in
  if (Half_dressed_module.to_string hm)=Debugger_name.debugger_name
  then None
  else
  let new_values=Modulesystem_data.compute_modification_times hm 
  and old_values=Modulesystem_data.modification_times x in
  if old_values=new_values
  then None
  else
  let (n_ml,n_mli,n_mll,n_mly)=new_values in
  let edg=List.hd(Modulesystem_data.registered_endings x) in
  let mlx=Mlx_filename.join hm edg in
  let fathers=find_needed_names l mlx in
  Some(
  {
    Modulesystem_data.name=x.Modulesystem_data.name;
    ml_present=x.Modulesystem_data.ml_present;
    mli_present=x.Modulesystem_data.mli_present;
    mll_present=x.Modulesystem_data.mll_present;
    mly_present=x.Modulesystem_data.mly_present;
    ml_modification_time=n_ml;
    mli_modification_time=n_mli;
    mll_modification_time=n_mll;
    mly_modification_time=n_mly;
    needed_libraries=x.Modulesystem_data.needed_libraries;
    direct_fathers=fathers;
    all_ancestors=x.Modulesystem_data.all_ancestors;
    needed_directories=x.Modulesystem_data.needed_directories;
   }   
   )   
  ;;
  
  

end;;






module Alaskan_ingredients_for_ocaml_target=struct

(*

It is assumed that no "manual tampering" is made,
e.g. manual rewriting of a ml coming from a mll, etc.


#use"Country/Germany/german_ingredients_for_ocaml_target.ml";;

*)



exception Unregistered_cmo  of Half_dressed_module.t;;
exception Unregistered_dcmo of Half_dressed_module.t;;
exception Unregistered_cmi  of Half_dressed_module.t;;
exception Unregistered_cma  of Half_dressed_module.t;;
exception Unregistered_cmx  of Half_dressed_module.t;;
exception Unregistered_ml_from_mll of Half_dressed_module.t;;
exception Unregistered_ml_from_mly of Half_dressed_module.t;;
exception Unregistered_executable of Half_dressed_module.t;;
exception Unregistered_debuggable of Half_dressed_module.t;;
exception Unregistered_module_in_toplevel of string*(Half_dressed_module.t);;
exception NonMarkedIngredientsForToplevel of string;;


let targets_from_ancestor_data dt=
  let hm=Modulesystem_data.name dt in
  if Modulesystem_data.mll_present dt
  then let mll_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mll) in
       [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm;Ocaml_target.cmo hm]
  else 
  if Modulesystem_data.mly_present dt
  then let mly_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mly) in
       [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm;Ocaml_target.cmo hm]
  else
  if Modulesystem_data.ml_present dt
  then 
       let ml_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.ml) in
       [ml_target;Ocaml_target.cmi hm;Ocaml_target.cmo hm]
  else let mli_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mli) in
       [mli_target;Ocaml_target.cmi hm];;  


let debuggable_targets_from_ancestor_data dt=
  let hm=Modulesystem_data.name dt in
  if Modulesystem_data.mll_present dt
  then let mll_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mll) in
       [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm;Ocaml_target.dcmo hm]
  else 
  if Modulesystem_data.mly_present dt
  then let mly_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mly) in
       [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm;Ocaml_target.dcmo hm]
  else
  if Modulesystem_data.ml_present dt
  then 
       let ml_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.ml) in
       [ml_target;Ocaml_target.cmi hm;Ocaml_target.dcmo hm]
  else let mli_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mli) in
       [mli_target;Ocaml_target.cmi hm];;  

let targets_from_ancestors mdata dt=
     let ancestors=Modulesystem_data.all_ancestors dt in
     let temp1=Image.image (fun hm2->
            let opt2=Alaskan_data.find_module_registration mdata hm2 in
            let dt2=Option.unpack opt2 in
            targets_from_ancestor_data dt2
          ) ancestors in
     Preserve_initial_ordering.preserve_initial_ordering temp1;;

let debuggable_targets_from_ancestors mdata ancestors=
     let temp1=Image.image (fun hm2->
            let opt2=Alaskan_data.find_module_registration mdata hm2 in
            let dt2=Option.unpack opt2 in
            debuggable_targets_from_ancestor_data dt2
          ) ancestors in
     Preserve_initial_ordering.preserve_initial_ordering temp1;;

let optimized_targets_from_ancestor_data dt=
  let hm=Modulesystem_data.name dt in
  if Modulesystem_data.mll_present dt
  then let mll_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mll) in
       [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm;Ocaml_target.cmx hm]
  else 
  if Modulesystem_data.mly_present dt
  then let mly_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mly) in
       [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm;Ocaml_target.cmx hm]
  else
  if Modulesystem_data.ml_present dt
  then 
       let ml_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.ml) in
       [ml_target;Ocaml_target.cmi hm;Ocaml_target.cmx hm]
  else let mli_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mli) in
       [mli_target;Ocaml_target.cmi hm];;  

let optimized_targets_from_ancestors mdata dt=
     let ancestors=Modulesystem_data.all_ancestors dt in
     let temp1=Image.image (fun hm2->
            let opt2=Alaskan_data.find_module_registration mdata hm2 in
            let dt2=Option.unpack opt2 in
            optimized_targets_from_ancestor_data dt2
          ) ancestors in
     Preserve_initial_ordering.preserve_initial_ordering temp1;;

let immediate_ingredients_for_ml_from_mll hm=
  let mll_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mll) in
  [mll_target];;

let immediate_ingredients_for_ml_from_mly hm=
  let mly_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mly) in
  [mly_target];;

let immediate_ingredients_for_cmi dt hm=
    if Modulesystem_data.mll_present dt
    then let mll_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mll) in
         [mll_target;Ocaml_target.ml_from_mll hm]
    else 
    if Modulesystem_data.mly_present dt
    then let mly_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mly) in
         [mly_target;Ocaml_target.ml_from_mly hm]
    else
  if Modulesystem_data.mli_present dt
  then let mli_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mli) in
       [mli_target]
  else let ml_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.ml) in
       [ml_target];; 

let immediate_ingredients_for_cmo dt hm=
    if Modulesystem_data.mll_present dt
    then let mll_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mll) in
         [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm]
    else 
    if Modulesystem_data.mly_present dt
    then let mly_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mly) in
         [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm]
    else
  if Modulesystem_data.ml_present dt
  then let ml_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.ml) in
       [ml_target;Ocaml_target.cmi hm]
  else let mli_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mli) in
       [mli_target;Ocaml_target.cmi hm];;  


let immediate_ingredients_for_dcmo=immediate_ingredients_for_cmo;;

let immediate_ingredients_for_cma=immediate_ingredients_for_cmo;;

let immediate_ingredients_for_cmx dt hm=
    if Modulesystem_data.mll_present dt
    then let mll_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mll) in
         [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm]
    else 
    if Modulesystem_data.mly_present dt
    then let mly_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mly) in
         [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm]
    else
  if Modulesystem_data.ml_present dt
  then let ml_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.ml) in
       [ml_target;Ocaml_target.cmi hm]
  else let mli_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mli) in
       [mli_target;Ocaml_target.cmi hm];;  


let immediate_ingredients_for_executable hm=
 [Ocaml_target.cmx hm;Ocaml_target.executable hm];;  

let immediate_ingredients_for_debuggable hm=
  [Ocaml_target.dcmo hm;Ocaml_target.debuggable hm];;  

let ingredients_for_nodep mlx=[];;

let ingredients_for_ml_from_mll mdata hm=
  let opt=Alaskan_data.find_module_registration mdata hm in
  if opt=None then raise(Unregistered_ml_from_mll(hm)) else 
  let dt=Option.unpack opt in
  (targets_from_ancestors mdata dt)@(immediate_ingredients_for_ml_from_mll hm);;

let ingredients_for_ml_from_mly mdata hm=
  let opt=Alaskan_data.find_module_registration mdata hm in
  if opt=None then raise(Unregistered_ml_from_mly(hm)) else 
  let dt=Option.unpack opt in
  (targets_from_ancestors mdata dt)@(immediate_ingredients_for_ml_from_mly hm);;

let ingredients_for_cmi mdata hm=
          let opt=Alaskan_data.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_cmi(hm)) else 
          let dt=Option.unpack opt in
          (targets_from_ancestors mdata dt)@(immediate_ingredients_for_cmi dt hm);;

let ingredients_for_cmo mdata hm=
          let opt=Alaskan_data.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_cmo(hm)) else 
          let dt=Option.unpack opt in
          (targets_from_ancestors mdata dt)@(immediate_ingredients_for_cmo dt hm);;

let ingredients_for_dcmo mdata hm=
          let opt=Alaskan_data.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_dcmo(hm)) else 
          let dt=Option.unpack opt in
          let ancestors=Modulesystem_data.all_ancestors dt in
          (debuggable_targets_from_ancestors mdata ancestors)@(immediate_ingredients_for_dcmo dt hm);;

let ingredients_for_cma mdata hm=
          let opt=Alaskan_data.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_cma(hm)) else 
          let dt=Option.unpack opt in
          (targets_from_ancestors mdata dt)@(immediate_ingredients_for_cma dt hm);;

let ingredients_for_cmx mdata hm=
          let opt=Alaskan_data.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_cmx(hm)) else 
          let dt=Option.unpack opt in
          (optimized_targets_from_ancestors mdata dt)@(immediate_ingredients_for_cmx dt hm);;
 
let ingredients_for_executable mdata hm=
  let opt=Alaskan_data.find_module_registration mdata hm in
  if opt=None then raise(Unregistered_executable(hm)) else 
  let dt=Option.unpack opt in
  (optimized_targets_from_ancestors mdata dt)
  @(immediate_ingredients_for_executable hm);; 
  
let ingredients_for_debuggable mdata hm=
  let mlfile=Mlx_filename.join hm Ocaml_ending.Ml in
  let genealogy=Read_info_on_file_in_system.find_needed_data mdata mlfile in
  let dirfath=Image.image (Modulesystem_data.name) genealogy in
  let temp1=Image.image 
   			(fun t->Tidel.diforchan(Modulesystem_data.all_ancestors t)) 
   			genealogy in
   let temp2=Tidel.big_teuzin ((Tidel.diforchan(dirfath) )::temp1) in
   let tempf=(fun t->
   					let nam_t=Modulesystem_data.name t in
   					if Tidel.elfenn nam_t temp2
   					then Some(nam_t)
   					else None) in
   let allanc=Option.filter_and_unpack tempf mdata in
  (debuggable_targets_from_ancestors mdata allanc)
  @(immediate_ingredients_for_debuggable hm);;    
  
let ingredients_for_toplevel_element mdata name hm=
   let opt=Alaskan_data.find_module_registration mdata hm in
  if opt=None then raise(Unregistered_module_in_toplevel(name,hm)) else 
  let dt=Option.unpack opt in
  if (Modulesystem_data.mli_present dt)&&(not(Modulesystem_data.ml_present dt))
  then (ingredients_for_cmi mdata hm)@[Ocaml_target.cmi hm]
  else (ingredients_for_cmo mdata hm)@[Ocaml_target.cmo hm];;  
  
let ingredients_for_toplevel mdata name l=
  let temp1=Image.image (ingredients_for_toplevel_element mdata name) l in
  Preserve_initial_ordering.preserve_initial_ordering temp1;;
      
let ingredients_for_ocaml_target mdata=function
  Ocaml_target.NO_DEPENDENCIES(mlx)->[]
 |Ocaml_target.ML_FROM_MLL(hm)->ingredients_for_ml_from_mll mdata hm
 |Ocaml_target.ML_FROM_MLY(hm)->ingredients_for_ml_from_mly mdata hm
 |Ocaml_target.CMI(hm)->ingredients_for_cmi mdata hm
 |Ocaml_target.CMO(hm)->ingredients_for_cmo mdata hm
 |Ocaml_target.DCMO(hm)->ingredients_for_dcmo mdata hm
 |Ocaml_target.CMA(hm)->ingredients_for_cma mdata hm
 |Ocaml_target.CMX(hm)->ingredients_for_cmx mdata hm
 |Ocaml_target.EXECUTABLE(hm)->ingredients_for_executable mdata hm
 |Ocaml_target.DEBUGGABLE(hm)->ingredients_for_debuggable mdata hm
 |Ocaml_target.TOPLEVEL(name,l)->ingredients_for_toplevel mdata name l;;      
 


let marked_ingredients_for_unprepared_toplevel mdata name l=
  let temp1=Image.image (ingredients_for_toplevel_element mdata name) l in
  Preserve_initial_ordering.and_mark_endings temp1;;

let module_dependency_for_nodep mlx=false;;
let module_dependency_for_ml_from_mll mdata l_hm hm1=
       if List.mem hm1 l_hm
       then true
       else  
       let dt1=Option.unpack(Alaskan_data.find_module_registration mdata hm1) in
       let anc1=Modulesystem_data.all_ancestors dt1 in
       List.exists (fun z->List.mem z anc1 ) l_hm;;
let module_dependency_for_ml_from_mly=module_dependency_for_ml_from_mll;; 
let module_dependency_for_cmi=module_dependency_for_ml_from_mll;;
let module_dependency_for_cmo=module_dependency_for_ml_from_mll;;
let module_dependency_for_dcmo=module_dependency_for_ml_from_mll;;
let module_dependency_for_cma=module_dependency_for_ml_from_mll;;                 
let module_dependency_for_cmx=module_dependency_for_ml_from_mll;;  
let module_dependency_for_executable=module_dependency_for_ml_from_mll;;  
let module_dependency_for_debuggable=module_dependency_for_ml_from_mll;;  
let module_dependency_for_toplevel mdata l_hm name l_hm2=
  List.exists(fun hm2->
  (module_dependency_for_cmo mdata l_hm hm2)||(List.mem hm2 l_hm)
  ) l_hm2;;


let module_dependency_for_ocaml_target mdata l_hm =function
  Ocaml_target.NO_DEPENDENCIES(mlx)->false
 |Ocaml_target.ML_FROM_MLL(hm)->module_dependency_for_ml_from_mll mdata l_hm hm
 |Ocaml_target.ML_FROM_MLY(hm)->module_dependency_for_ml_from_mly mdata l_hm hm
 |Ocaml_target.CMI(hm)->module_dependency_for_cmi mdata l_hm hm
 |Ocaml_target.CMO(hm)->module_dependency_for_cmo mdata l_hm hm
 |Ocaml_target.DCMO(hm)->module_dependency_for_dcmo mdata l_hm hm
 |Ocaml_target.CMA(hm)->module_dependency_for_cma mdata l_hm hm
 |Ocaml_target.CMX(hm)->module_dependency_for_cmx mdata l_hm hm
 |Ocaml_target.EXECUTABLE(hm)->module_dependency_for_executable mdata l_hm hm
 |Ocaml_target.DEBUGGABLE(hm)->module_dependency_for_debuggable mdata l_hm hm
 |Ocaml_target.TOPLEVEL(name,l)->module_dependency_for_toplevel mdata l_hm name l;;

       



let mlx_dependency_for_ocaml_target mdata mlx tgt=
  let hm=Mlx_filename.half_dressed_core mlx in
  module_dependency_for_ocaml_target mdata [hm] tgt;;

let mlx_list_dependency_for_ocaml_target mdata l_mlx tgt=
 List.exists (fun mlx->mlx_dependency_for_ocaml_target mdata mlx tgt) l_mlx;;


end;;






module Alaskan_force_modification_time=struct


(* 


#use"Country/Alaska/alaskan_force_modification_time.ml";;


*)

exception Non_existent_mtime of Mlx_filename.t;;

let update dir mdata mlx=
   let hm=Mlx_filename.half_dressed_core mlx
   and edg=Mlx_filename.ending mlx in
   let (before,opt,after)=Three_parts.select_center_element  (fun dt->
      Modulesystem_data.name dt=hm) mdata in
   if opt=None
   then raise(Non_existent_mtime(mlx))
   else 
   let dt=Option.unpack opt in
   let file=(Directory_name.to_string dir)^(Mlx_filename.to_string mlx) in
   let old_val=Modulesystem_data.modification_time dt edg 
   and new_val=(Unix.stat file).Unix.st_mtime  in
   if old_val=new_val
   then mdata
   else let new_dt=Modulesystem_data.force_modification_time dt edg new_val in
        before@(new_dt::after);;

    
   
   

end;;






module Alaskan_command_for_ocaml_target=struct

(*

#use"Country/Alaska/alaskan_command_for_ocaml_target.ml";;

*)




exception Command_called_on_nodep of Mlx_filename.t;;
exception Unregistered_cmo  of Half_dressed_module.t;;
exception Unregistered_dcmo of Half_dressed_module.t;;
exception Unregistered_cmi  of Half_dressed_module.t;;
exception Unregistered_cma  of Half_dressed_module.t;;
exception Unregistered_cmx  of Half_dressed_module.t;;
exception Unregistered_ml_from_mll of Half_dressed_module.t;;
exception Unregistered_ml_from_mly of Half_dressed_module.t;;
exception Unregistered_executable of Half_dressed_module.t;;
exception Unregistered_debuggable of Half_dressed_module.t;;
exception Unregistered_modules_in_toplevel of string*(Half_dressed_module.t list);;  

let ingr=Alaskan_ingredients_for_ocaml_target.ingredients_for_ocaml_target;;

let cmx_manager=function
 Ocaml_target.CMX(hm2)->
    let s_hm2=Half_dressed_module.to_string hm2 in
    Some(s_hm2^".cmx")
 |_->None;;

let dcmo_manager=function
 Ocaml_target.DCMO(hm2)->
    let s_hm2=Half_dressed_module.to_string hm2 in
    Some(s_hm2^".d.cmo")
 |_->None;;

let command_for_nodep mlx=[];;

let command_for_ml_from_mll dir hm=
          let s_hm=Half_dressed_module.to_string hm in
          let s_root=Directory_name.to_string dir in
          let s_fhm=s_root^s_hm in
          let s_for_display=
          "ocamllex "^
          " -o "^s_hm^".ml"^
          	 " "^s_hm^".mll" 
          and s_for_execution=
          "ocamllex "^
          " -o "^s_fhm^".ml"^
          	 " "^s_fhm^".mll" in
          [Shell_command.semi_usual (s_for_execution,s_for_display)];; 
 
let command_for_ml_from_mly dir hm=
          let s_hm=Half_dressed_module.to_string hm in
          let s_root=Directory_name.to_string(dir) in
          let s_fhm=s_root^s_hm in
          let s_for_display="ocamlyacc "^s_hm^".mly" 
          and s_for_execution="ocamlyacc "^s_fhm^".mly" in
          [Shell_command.semi_usual (s_for_execution,s_for_display)];;  

let command_for_cmi dir mdata hm=
          let opt=Alaskan_data.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_cmi(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s_root=Directory_name.to_string(dir) in
          let s_fhm=s_root^s_hm in
          let ending=(
          if Modulesystem_data.mli_present dt
          then ".mli"
          else ".ml"
          ) in
          let s1=
          "ocamlc "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -c "^s_hm^ending 
          and long_s1="ocamlc "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -c "^s_fhm^ending in
          let full_mli=s_root^s_hm^".mli" in
          if (not(Modulesystem_data.mli_present dt))
             &&(Sys.file_exists(full_mli))
          then (* 
                 in this situation the mli file exists but is not registered.
                 So the modulesystem manager must treat it as though it didn't
                 exist. We temporarily rename it so that ocamlc will ignore it.
                *)
                let dummy_mli=s_root^"uvueaoqhkt.mli" in
                let s2="mv "^full_mli^" "^dummy_mli
                and s3="mv "^dummy_mli^" "^full_mli in
                [Shell_command.usual s2;
                 Shell_command.semi_usual (long_s1,s1);
                 Shell_command.usual s3]
          else   [Shell_command.semi_usual (long_s1,s1)];;

let command_for_cmo dir mdata hm=
          let opt=Alaskan_data.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_cmo(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s_root=Directory_name.to_string(dir) in
          let s_fhm=s_root^s_hm in
          let s_for_display=
          "ocamlc "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_hm^".cmo"^
          " -c "^s_hm^".ml" 
          and s_for_execution=
          "ocamlc "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_fhm^".cmo"^
          " -c "^s_fhm^".ml"in
          [Shell_command.semi_usual (s_for_execution,s_for_display)];;

let command_for_dcmo dir mdata hm=
          let opt=Alaskan_data.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_dcmo(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s_root=Directory_name.to_string(dir) in
          let s_fhm=s_root^s_hm in
          let s_for_display=
          "ocamlc -g "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_hm^".d.cmo"^
          " -c "^s_hm^".ml" 
          and  s_for_execution=
          "ocamlc -g "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_fhm^".d.cmo"^
          " -c "^s_fhm^".ml" in
          [Shell_command.semi_usual (s_for_execution,s_for_display)];;

          
let command_for_cma dir mdata hm=
          let opt=Alaskan_data.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_cma(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s_root=Directory_name.to_string(dir) in
          let s_fhm=s_root^s_hm in
          let s_for_display=
          "ocamlopt -a "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_hm^".cma"^
          " -c "^s_hm^".ml" 
          and s_for_execution=
          "ocamlopt -a "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_fhm^".cma"^
          " -c "^s_fhm^".ml" in
          [Shell_command.semi_usual (s_for_execution,s_for_display)];;
 
let command_for_cmx dir mdata hm=
          let opt=Alaskan_data.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_cmx(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s_root=Directory_name.to_string(dir) in
          let s_fhm=s_root^s_hm in
          let s_for_display=
          "ocamlopt "^(Modulesystem_data.needed_dirs_and_libs true dt)^
          " -o "^s_hm^".cma"^
          " -c "^s_hm^".ml" 
          and s_for_execution=
          "ocamlopt "^(Modulesystem_data.needed_dirs_and_libs true dt)^
          " -o "^s_fhm^".cma"^
          " -c "^s_fhm^".ml"in
          [Shell_command.semi_usual (s_for_execution,s_for_display)];; 
 

let command_for_executable dir mdata hm=
          let opt=Alaskan_data.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_executable(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s_root=Directory_name.to_string(dir) in
          let s_fhm=s_root^s_hm in
          let temp1=ingr mdata (Ocaml_target.EXECUTABLE(hm)) in
          let temp2=Option.filter_and_unpack cmx_manager temp1 in
          let long_temp2=Image.image (fun t->s_root^t) temp2 in
          let s_for_display=
          "ocamlopt "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_hm^".caml_executable"^
          (String.concat " " temp2) 
          and s_for_execution=
          "ocamlopt "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_fhm^".caml_executable"^
          (String.concat " " long_temp2) in
          [Shell_command.semi_usual (s_for_execution,s_for_display)];; 
  
let command_for_debuggable dir mdata hm=
          let opt=Alaskan_data.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_debuggable(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s_root=Directory_name.to_string(dir) in
          let s_fhm=s_root^s_hm in
          let temp1=ingr mdata (Ocaml_target.DEBUGGABLE(hm)) in
          let temp2=Option.filter_and_unpack dcmo_manager temp1 in
          let long_temp2=Image.image (fun t->s_root^t) temp2 in
          let s_for_display=
          "ocamlc -g "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_hm^".ocaml_debuggable "^
          (String.concat " " temp2) 
          and s_for_execution=
          "ocamlc -g "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_fhm^".ocaml_debuggable "^
          (String.concat " " long_temp2) in
          [Shell_command.semi_usual (s_for_execution,s_for_display)];; 
  
let command_for_toplevel dir mdata name l=
          let temp1=Image.image (fun hm->(hm,Alaskan_data.find_module_registration mdata hm)) l  in
          let temp2=List.filter (fun x->snd(x)=None) temp1 in
          if temp2<>[]
          then let temp3=Image.image fst temp2 in
               raise(Unregistered_modules_in_toplevel(name,temp3))
          else
          let l_dt=Image.image (fun (_,y)->Option.unpack y) temp1 in
          let temp4=Image.image (fun fd->
             let hm=Modulesystem_data.name fd in
             let s_hm=(Half_dressed_module.to_string hm) in
             if Modulesystem_data.ml_present fd 
             then s_hm^".cmo"
             else " "
          ) l_dt in 
          let s_lhm=String.concat " " temp4 in
          let s_root=Directory_name.to_string(dir) in
          let long_temp4=Image.image (fun fd->
             let hm=Modulesystem_data.name fd in
             let s_hm=(Half_dressed_module.to_string hm) in
             if Modulesystem_data.ml_present fd 
             then s_root^s_hm^".cmo"
             else " "
          ) l_dt in 
          let long_s_lhm=String.concat " " long_temp4 in
          let s_for_display=
          "ocamlmktop "^(Modulesystem_data.needed_dirs_and_libs_for_several false l_dt)^
          " -o "^name^" "^
          "  "^s_lhm^" " 
          and s_for_execution=
          "ocamlmktop "^(Modulesystem_data.needed_dirs_and_libs_for_several false l_dt)^
          " -o "^s_root^name^" "^
          "  "^long_s_lhm^" " in
          [Shell_command.semi_usual (s_for_execution,s_for_display)];;   
 
let command_for_ocaml_target dir mdata tgt=
   match tgt with
  Ocaml_target.NO_DEPENDENCIES(mlx)->command_for_nodep mlx 
 |Ocaml_target.ML_FROM_MLL(hm)->command_for_ml_from_mll dir hm
 |Ocaml_target.ML_FROM_MLY(hm)->command_for_ml_from_mly dir hm
 |Ocaml_target.CMI(hm)->command_for_cmi dir mdata hm
 |Ocaml_target.CMO(hm)->command_for_cmo dir mdata hm
 |Ocaml_target.DCMO(hm)->command_for_dcmo dir mdata hm
 |Ocaml_target.CMA(hm)->command_for_cma dir mdata hm
 |Ocaml_target.CMX(hm)->command_for_cmx dir mdata hm
 |Ocaml_target.EXECUTABLE(hm)->command_for_executable dir mdata hm
 |Ocaml_target.DEBUGGABLE(hm)->command_for_debuggable dir mdata hm
 |Ocaml_target.TOPLEVEL(name,l)->command_for_toplevel dir mdata name l;;
   
  
  
  
 let command_for_ocaml_target_in_dir dir mdata tgt=
   Shell_command.take_care_of_root_directory dir
     (command_for_ocaml_target dir mdata tgt);; 
          

 


end;;






module Alaskan_make_ocaml_target=struct

(*

#use"Country/Alaska/alaskan_make_ocaml_target.ml";;

*)



let cmd_for_tgt=Alaskan_command_for_ocaml_target.command_for_ocaml_target_in_dir;;

let ingr_for_tgt =Alaskan_ingredients_for_ocaml_target.ingredients_for_ocaml_target;;
let ingr_for_top =Alaskan_ingredients_for_ocaml_target.marked_ingredients_for_unprepared_toplevel;;


let is_up_to_date dir tgts tgt=
  if Ocaml_target.is_a_debuggable tgt
  then false
  else 
  if Ocaml_target.test_path dir tgt
  then List.mem tgt tgts
  else false;;

let unit_make dir (bowl,(mdata,tgts)) tgt=
  if (not bowl)
  then (bowl,(mdata,tgts))
  else
  if is_up_to_date dir tgts tgt
  then (true,(mdata,tgts))
  else 
  let temp1=Image.image Shell_command.announce_and_do (cmd_for_tgt dir mdata tgt) in 
  if List.for_all (fun bowl->bowl) temp1
  then let opt_tgt=(if Ocaml_target.is_a_debuggable tgt then None else (Some tgt)) in
       let tgts2=Option.add_perhaps opt_tgt tgts in
        match Ocaml_target.ml_from_lex_or_yacc_data tgt with
       None->(true,(mdata,tgts2))
       |Some(mlx)->
                   let mdata2=Alaskan_force_modification_time.update dir mdata mlx in
                   (true,(mdata2,tgts2))        
  else (false,(mdata,tgts));;

let make_nontoplevel dir (mdata,tgts) tgt=
  let l=ingr_for_tgt mdata tgt in
  List.fold_left (unit_make dir)  (true,(mdata,tgts)) l;;
  
exception Ending_for_toplevel_pusher;;  



let rec pusher_for_toplevel dir (successful_ones,to_be_treated,ts)=
  match to_be_treated with
  []->raise(Ending_for_toplevel_pusher)
  |(tgt,is_an_ending_or_not)::others->
  let (bowl2,ts2)=unit_make dir (true,ts) tgt in
  if bowl2
  then let new_successful_ones=(
         if is_an_ending_or_not=Is_an_ending_or_not.Yes
         then let hm=Option.unpack(Ocaml_target.main_module tgt) in
              (*
                Note that the cmi and cmo give the same hm
              *)
              if List.mem hm successful_ones
              then successful_ones
              else hm::successful_ones
         else successful_ones
       ) in
       (new_successful_ones,others,ts2)
  else let hm=Option.unpack(Ocaml_target.main_module tgt) in
  	   let remains=List.filter
       (fun (tgt,_)->
         not(Alaskan_ingredients_for_ocaml_target.module_dependency_for_ocaml_target 
         (fst ts) [hm] tgt)
       ) to_be_treated in
       (successful_ones,remains,ts2);; 

let rec iterator_for_toplevel dir (successful_ones,to_be_treated,ts)=
  match to_be_treated with
  []->(List.rev successful_ones,ts)
  |_->iterator_for_toplevel dir (pusher_for_toplevel dir (successful_ones,to_be_treated,ts));;

  
let make_toplevel dir ts name l=
    let temp1=ingr_for_top (fst ts) name l in
    let (successful_ones,ts2)=iterator_for_toplevel dir ([],temp1,ts) in
    let new_toplevel=Ocaml_target.toplevel name successful_ones  in
    unit_make dir (true,ts2) new_toplevel;;
 
let make dir ts tgt=
  match Ocaml_target.toplevel_data tgt with
  None->make_nontoplevel dir ts tgt
  |Some(name,l)->make_toplevel dir ts name l;; 
 


end;;






module Alaskan_printer_equipped_types=struct


(* 

#use"Country/Germany/german_printer_equipped_types.ml";;


*)

let from_data mdata=
   Option.filter_and_unpack (
  	fun md->
   	let hm=Modulesystem_data.name md
   	and ap=Modulesystem_data.principal_path md in
   	let text=Io.read_whole_file ap in
   	if (Substring.is_a_substring_of
     ("let "^"print_out ") text)&&
    	(not(Half_dressed_module.is_optional hm))
   	then Some(hm)
  	else None
   ) mdata;;

let instructions printer_equipped_types=
  let temp2=List.rev_map (
    function x->
      "#install_printer "^(Half_dressed_module.module_name x)^".print_out;"^";"
  ) printer_equipped_types in
  let temp3="\n\n\n"::(List.rev ("\n\n\n"::temp2)) in
  let part2=String.concat "\n" temp3 in
  part2;;  
 
let declare_printer hm0 l=hm0::l;;
         
let undeclare_printer hm0 l=
  List.filter (fun hm->hm<>hm0) l;;    
 

end;;






module Alaskan_register_mlx_file=struct


(* 

#use"Country/Alaska/alaskan_register_mlx_file.ml";;


*)

exception Already_registered_file of Mlx_filename.t;;  
exception Overcrowding of Mlx_filename.t*(Ocaml_ending.t list);;
exception Bad_pair of Mlx_filename.t*Ocaml_ending.t;; 
exception Name_conflict of Half_dressed_module.t * Half_dressed_module.t;; 
   
 
let on_monitored_modules mdata mlx_file =
   let hm=Mlx_filename.half_dressed_core mlx_file
   and ending=Mlx_filename.ending mlx_file in 
   let nm=Half_dressed_module.undress hm in
   let (before,opt,after)=Three_parts.select_center_element  (fun dt->
      Half_dressed_module.undress(Modulesystem_data.name dt)=nm) 
      mdata in
   if opt=None
   then  let old_info=Read_info_on_file_in_system.complete_info mdata mlx_file in
         let info1=Modulesystem_data.make_presence ending old_info in
         (*
         if a mll or mly file is being registered, the ml will automatically be created,
         so let us anticipate by already adding a ml presence
         *)
         let info=(if List.mem ending [Ocaml_ending.mll;Ocaml_ending.mly]
         then Modulesystem_data.make_ml_present info1 else info1) in
         	  before@[info] 
   else
   let old_dt=Option.unpack(opt) in
   let old_name=Modulesystem_data.name old_dt in
   if (old_name<>hm)
   then raise(Name_conflict(old_name,hm))
   else 
   let edgs=Modulesystem_data.registered_endings old_dt in
   if List.length(edgs)>1
   then  raise(Overcrowding(mlx_file,edgs))
   else  
   if List.mem ending edgs
   then raise(Already_registered_file(mlx_file))
   else
   if (not(List.mem Ocaml_ending.ml (ending::edgs)))
   then raise(Bad_pair(mlx_file,List.hd edgs))
   else 
   let dt1=Read_info_on_file_in_system.complete_info mdata mlx_file in
   let new_dt=Modulesystem_data.make_presence ending dt1 in
   if ending<>Ocaml_ending.ml
   then before@(new_dt::after) 
   else 
   let temp3=List.rev(Modulesystem_data.direct_fathers new_dt) in
   if temp3=[]
   then before@(new_dt::after)
   else  
   let last_father=List.hd(List.rev(Modulesystem_data.direct_fathers new_dt)) in
   let (before1,opt1,after1)=Three_parts.select_center_element  (fun dt->
           (Modulesystem_data.name dt)=last_father) before in
   let lf1=Option.unpack opt1  in    
   let temp2=Image.image (Modulesystem_data.update_anclibdir new_dt mdata) (after1@after) in
   let final_list=before1@(lf1::new_dt::temp2) in
   final_list;;

  
let on_targets (old_mdata,old_dirs,old_tgts) mlx=
  let hm=Mlx_filename.half_dressed_core mlx in
  let new_dir=Half_dressed_module.subdirectory hm in
 let new_mdata=on_monitored_modules old_mdata mlx in
 let new_dirs=
 (if List.mem new_dir old_dirs then old_dirs else old_dirs@[new_dir] )
 and new_tgts=
 (if Half_dressed_module.is_optional hm
  then old_tgts
  else (*
       The only outdated targets are the main toplevel, 
       and targets corresponding to an identical module
       (for example when a mll or mly is added to
       an already registered ml) 
        *)
       List.filter (
        fun tgt->match Ocaml_target.toplevel_name tgt with
          None->(match Ocaml_target.main_module tgt with
                 None->true
                 |Some(hm2)->hm2<>hm
                )
          |Some(name)->false
       ) old_tgts
  ) in
  (new_mdata,new_dirs,new_tgts);; 
 



end;;






module Alaskan_try_to_register=struct


(* 

#use"Country/Alaska/alaskan_try_to_register.ml";;

*)



let mlx_file mdata mlx_file=
    try(Some(Alaskan_register_mlx_file.on_monitored_modules 
        mdata mlx_file)) with _->None;;  

module Private=struct

exception Pusher_exn;;

let pusher  (vdata,failures,yet_untreated)=
     match yet_untreated with
      []->raise(Pusher_exn)
      |mlx::others->
      (
        match mlx_file vdata mlx with
        None->(vdata,mlx::failures,others)
        |Some(nfs)->(nfs,failures,others)
      );; 

let rec iterator x=
   let (vdata,failures,yet_untreated)=x in
   match yet_untreated with
      []->(failures,vdata)
      |mlx::others->
      (
        match mlx_file vdata mlx with
        None->iterator(pusher x)
        |Some(nfs)->iterator(pusher x)
      );;   

end;;

let mlx_files mdata mlx_files=
   Private.iterator(mdata,[],mlx_files);;
 
  
   

end;;






module Alaskan_create_target_system=struct


(* 

#use"Country/Alaska/alaskan_create_target_system.ml";;


*)

let display_circular_dependencies printer l cycles= 
  if cycles=[]
  then ()
  else
  let temp1=Image.image(fun cycle->
    let ttemp1=Image.image (fun j->printer (List.nth l (j-1))) cycle in
     String.concat " -> " ttemp1 
  ) cycles in
  let temp2="\n\n The following cycles have been detected : "^(String.concat "\n\n" temp1) in
  (print_string temp2;flush stdout);;
 

let select_good_files s_main_dir=
   let ap1=Absolute_path.of_string s_main_dir in
   let _=Sys.command ("touch "^s_main_dir^"/"^(Debugger_name.debugger_name)^".ml") in
   let temp1=More_unix.complete_ls (Directory_name.of_string s_main_dir) in
   let s_ap1=Absolute_path.to_string ap1 in
   let n1=String.length(s_ap1) in
   let selector=(
   fun ap->
     let s=Absolute_path.to_string ap in
     let t=Cull_string.cobeginning n1 s in
     (List.exists (fun edg->Substring.ends_with s edg) [".ml";".mli";".mll";".mly"])
     &&
     (List.for_all (fun beg->not(Substring.begins_with t beg)) ["Remembered/";"Forgotten/"])
     &&
     (* When a mll or mly is present, the ml will automatically be registered also,
        see the alaskan_register_mlx_file module. *)
     (not(
           (Substring.ends_with s ".ml")
           &&
           (List.exists (fun edg->Sys.file_exists(s_ap1^s^edg)) ["l";"y"])
     ))
     &&
     (List.for_all (fun edg->not(Substring.ends_with s edg) ) 
     ["neptu";
     "my_loadings.ml";
     "my_printers.ml";
     (*"debugger.ml";*)"my_pervasives.ml";
     ".ocamlinit"])
   ) in
   List.filter selector temp1;;
   
 let rec detect_identical_names (identical_names,l)=
   match l with 
   []->identical_names
  |(a,b)::others->
     let (temp1,temp2)=List.partition (fun t->snd(t)=b) others in
     if temp1<>[]
     then detect_identical_names(((a,b)::temp1)::identical_names,temp2)
     else detect_identical_names(identical_names,temp2);;  
     
 exception Identical_names of (((string*string) list) list);;    
     
 let clean_list_of_files main_dir l=
  (*
     raises an exception if there are different modules with
     identical names.
     Removes the files outside main_dir.
  *)
  let s_dir=Directory_name.to_string main_dir in
  let temp1=List.filter (fun ap->
    Substring.begins_with (Absolute_path.to_string ap) s_dir
  ) l in
  let temp2=Image.image (fun ap->
    let s=Absolute_path.to_string ap in
    (ap,Father_and_son.son s '/')
  ) temp1 in
  let temp3=detect_identical_names ([],temp2) in
  if temp3<>[]
  then let n1=String.length s_dir in
       let tempf1=(fun (x,y)->
           (Cull_string.cobeginning n1 (Absolute_path.to_string x),y)
        ) in
       let tempf2=Image.image (Image.image tempf1) in
       let temp4=tempf2 temp3 in
       raise(Identical_names(temp4))
  else temp2;;
  
let compute_dependencies l=
  let temp1=Ennig.index_everything l 
  and n=List.length l in
  let rec tempf=(fun (j1,(ap1,s1))->
    let ttemp1=Look_for_module_names.names_in_file ap1 in
    let ttemp2=Image.image Naked_module.to_string ttemp1 in
    let ttempf=(fun s_nm->
      Option.filter_and_unpack (fun 
      (k,(_,s))->
      if (Father_and_son.father s '.')=s_nm
      then Some(k)
      else None ) temp1
    ) in
    let ttemp3=Image.image ttempf ttemp2 in
    List.flatten  ttemp3
  )  in
  let tempg=(fun x-> let (_,(_,s))=x in
     if Substring.ends_with s ".mli"
     then let t=Cull_string.coending 1 s in
          match Option.find_it (fun (_,(_,s1))->s1=t) temp1 with
           None->tempf x
          |Some(y)->tempf y 
     else tempf x
  ) in
  let table_for_coatoms=Image.image tempg temp1 in
  let coat=Memoized.make(fun j->List.nth table_for_coatoms (j-1)) in
  let (cycles,good_list)=
  	Reconstruct_linear_poset.reconstruct_linear_poset coat 
  	(Ennig.ennig 1 n) in
  let _=display_circular_dependencies
  (fun (j1,(ap1,s1))->s1) temp1 cycles in
  Image.image (fun (j,_)->snd(List.nth temp1 (j-1)) ) good_list;;
  
let from_prepared_list dir l=
   let temp1=Option.filter_and_unpack (fun (ap,s)->
      Mlx_filename.try_from_path_and_root ap dir
   ) l in
   Alaskan_try_to_register.mlx_files [] temp1;;


let usual_outsiders=ref
    [
      "my_loadings.ml";
      "my_pervasives.ml";
      "my_printers.ml";
    ];;

let from_main_directory dir opt_topl_name special_outsiders=
	let old_s=Directory_name.to_string(dir) in
	let s1=Cull_string.coending 1 old_s in (* mind the trailing slash *)
	let temp1=select_good_files s1 in
    let temp2=clean_list_of_files dir temp1 in
    let temp3=compute_dependencies temp2 in
    let (failures,mdata1)=from_prepared_list dir temp3 in
    let preqt=Alaskan_printer_equipped_types.from_data mdata1 in
    let topl_name=(if opt_topl_name=None then "ecaml" else Option.unpack opt_topl_name) in
    let topl=(Alaskan_data.default_toplevel topl_name mdata1) in
 	let (mdata2,new_tgts2)=snd(Alaskan_make_ocaml_target.make dir (mdata1,[]) topl) in
 	let old_outsiders=(!usual_outsiders)@special_outsiders in
 	let new_outsiders=Option.filter_and_unpack (fun t->
 	   let s_ap=Directory_name.join dir t in
 	   if Sys.file_exists s_ap
 	   then Some(Absolute_path.of_string s_ap)
 	   else None
 	) old_outsiders in
 	(mdata2,new_tgts2,new_outsiders,preqt);;



end;;

end;;


let help_me ()=Assistance.Alaskan_create_target_system.from_main_directory
(Assistance.Directory_name.of_string directory_to_be_cured) None [];;