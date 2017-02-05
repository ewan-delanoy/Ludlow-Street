(*



1) Change the value of current_sick_directory just below to suit
your needs
2) In a faraway terminal, do 

#use"/Users/ewandelanoy/Documents/OCaml/Ordinary/Remembered/assistance.ml";;

3) In the same terminal, do

let ap=Absolute_path.of_string current_sick_directory;;
let ap1=Absolute_path.of_string (current_sick_directory^"/current_root_directory.ml");;
let new_content=
"(*\n\n#use\"current_root_directory.ml\";;\n\n*)\n\n\n"^
"let current_root_directory=Directory_name.of_string\n\""^
current_sick_directory^"\";;";;
Io.erase_file_and_fill_it_with_string ap1 new_content;;

Current_target_system.refresh();;

*)

let current_sick_directory="/Users/ewandelanoy/Documents/OCaml/Ordinary";;

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

let hard_concat x y=List.rev_append(List.rev(x))(y);;

let easy_big_concat=function
[]->[]
|x::y->List.fold_left(List.rev_append)(x)(y);;

let hard_big_concat=function
[]->[]
|x::y->List.fold_left(hard_concat)(x)(y);;

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

let find_it_really f l=unpack(find_it f l);;

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


let rec find_and_stop_immediately_after f l=
 let rec find_and_stop_immediately_after0=(function
  da_ober->match da_ober with
   []->None
   |a::peurrest->match f(a) with
		None->find_and_stop_immediately_after0(peurrest)
		|Some(x)->Some(x)
 ) in
 find_and_stop_immediately_after0(l);;


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
  
let add_optionally opt l=match opt with
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

let rec find_and_stop_immediately_after f a b=
 let rec find_and_stop_immediately_after0=(function
  j->if (j>b)
     then None
	 else match f(j) with
		None->find_and_stop_immediately_after0(j+1)
		|Some(x)->Some(x)
 ) in
 find_and_stop_immediately_after0 a;;

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

let original_select f l=
  let rec tempf=(fun (graet,da_ober)->match da_ober with
  []->(graet,None,[])
  |x::peurrest->if f x 
                then (graet,Some(x),peurrest)
                else tempf(x::graet,peurrest)
  ) in
  tempf([],l);;

let select_center_element f l=let (temp1,opt,after)=original_select f l in (List.rev temp1,opt,after);;

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






module Substring=struct

(*

Operation on substring finding, with indexes starting from 1.

#use"find_substring.ml";;


*)

type substring=string;;

let begins_with x (y:substring)=
      let ly=String.length(y) in
      if String.length(x)<ly
      then false
      else (String.sub x 0 ly)=y;;
      
 let is_the_beginning_of y x=begins_with x y;;     
   
 let ends_with x (y:substring)=
      let ly=String.length(y) in
      if String.length(x)<ly
      then false
      else (String.sub x ((String.length x)-ly) ly)=y;;  
   
 let is_the_ending_of y x=ends_with x y;;  
 
 
 
 let is_a_substring_located_at (y:substring) x old_j =
    let j=old_j-1 in
    let ly=String.length(y) in
      if String.length(x)<j+ly
      then false
      else (String.sub x j ly)=y;;
 
  let is_a_substring_of (x:substring) y=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) in
      Ennig.exists tester 0 (String.length(y)-lx);; 
      
  let substring_leftmost_index (x:substring) y=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) in
      try (Option.unpack(Ennig.find_it tester 0 (String.length(y)-lx))+1) with
      _->(-1);;
  
  let substring_rightmost_index (x:substring) y=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) 
      and temp1=List.rev(Ennig.ennig(0)(String.length(y)-lx)) in
      try ((Option.find_it_really tester temp1)+1) with
      _->(-1);;
  
   let substring_leftmost_index_from (x:substring) y i=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) in
      match Ennig.find_it tester (i-1) (String.length(y)-lx) with
         None->(-1)
        |Some(k)->k+1;;
  
 let occurrences (x:substring) y=
   let lx=String.length x 
   and n=String.length y in
   let rec tempf=(fun (j,accu)->
      if j>n then List.rev(accu) else
      let k=substring_leftmost_index_from x y j in
      if k<0 then List.rev(accu) else
      tempf(k+lx,k::accu)
   )  in
   tempf (1,[]);;
   
   

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
   


end;;






module Ordered_string=struct

(* file created using the "write_contents_of_ordered_list_module" function *)
(* in Creators/ordered_list_creator.ml *)

type set=string Ordered.old_set;;
let lt s1 s2=
	let n1=String.length(s1) and n2=String.length(s2) in
	if n1=n2
	then match Ennig.find_it(function j->String.get(s1)(j)<>String.get(s2)(j) )(0)(n1-1)
		with
			 None->false
			|Some(j0)->String.get(s1)(j0)<String.get(s2)(j0) 
	else n1<n2;;
let cmp=((Total_ordering.from_lt lt):>(string Total_ordering.t));;


let unsafe_set=(Ordered.unsafe_set:>(string list-> set));;
let forget_order=(Ordered.forget_order:>(set->string list));;

let kreskus_strizh x=Ordered.kreskus_strizh cmp x;;
let kreskus x=Ordered.kreskus cmp x;;

let elfenn=((fun a ox->Ordered.elfenn cmp a ox):>(string->set->bool));;
let teuzin=((fun ox oy->Ordered.teuzin cmp ox oy):>( set->set->set));;
let diforchan=((fun x->Ordered.diforchan cmp x):>(string list->set));;
let lemel=((fun ox oy->Ordered.lemel cmp ox oy):>(set->set->set));;
let ental=((fun ox oy->Ordered.ental cmp ox oy):>(set->set->bool));;
let kengeij=((fun ox oy->Ordered.kengeij cmp ox oy):>set->set->set);;
let kengeij_goullo=((fun ox oy->Ordered.kengeij_goullo cmp ox oy):>set->set->bool);;
let min=((fun x->Ordered.min cmp x):>string list->string);;
let max=((fun x->Ordered.max cmp x):>string list->string);;

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
 ((fun x->Ordered.expand_boolean_algebra cmp x):>(set list->(set list)));; 
 
 


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
 
let quick_partition f l=
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
  let j=Substring.substring_leftmost_index(w)(x) in
  if j=(-1) then None else 
   Some(  beginning (j-1) x,
    cobeginning (j+String.length(w)-1) x);;


 let trim_spaces s=
   let n=String.length s in
   let opt1=Option.find_it(fun j->not(List.mem(String.get s (j-1)) [' ';'\t';'\n']))(Ennig.ennig 1 n) in
   if opt1=None then "" else
   let i1=Option.unpack opt1 in
   let k1=Option.find_it_really(fun j->not(List.mem(String.get s (n-j)) [' ';'\t';'\n']))(Ennig.ennig 1 n) in 
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
   let (temp2,temp3)=Prepared.quick_partition(fun k->k<j) temp1 in
   let a=(if List.length(temp2)<6 then 1 else List.nth(List.rev temp2)(5))
   and b=(if List.length(temp3)<6 then n else List.nth(temp3)(5)) in
   String.sub s (a-1) (b-a);;
   
   
   
     
   

  
  

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






module Max=struct




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
   let temp1=Option.filter_and_unpack (function 
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






module Min=struct




let list=function 
[]->failwith("min of empty set undefined")
|a::b->List.fold_left(min)(a)(b);;

let minimize_it f=function
[]->failwith("min on empty set undefined")
|x::y->
 let rec minimize_it0=(function
  (current_candidate,current_value,da_ober)->match da_ober with
  []->(current_candidate,current_value)
  |a::peurrest->let va=f(a) in
                if (va<current_value)
				then minimize_it0(a,va,peurrest)
				else minimize_it0(current_candidate,current_value,peurrest)
 ) 
in
 minimize_it0(x,f(x),y);;

let minimize_it_if_possible f l=
   let temp1=Option.filter_and_unpack (function 
     None->None
    |Some(x)->Some(x,f x) ) l in
   if temp1=[]
   then None
   else Some(fst(minimize_it(snd) temp1));;

let minimize_it_with_care f=function
[]->failwith("careful min on empty set undefined")
|x::y->
 let rec minimize_it_with_care0=(function
  (current_candidates,current_value,da_ober)->match da_ober with
  []->(current_value,List.rev(current_candidates))
  |a::peurrest->let va=f(a) in
                if (va<current_value)
				then minimize_it_with_care0([a],va,peurrest)
				else if (va=current_value)
				     then minimize_it_with_care0(a::current_candidates,current_value,peurrest)
					 else minimize_it_with_care0(current_candidates,current_value,peurrest)
 ) 
in
 minimize_it_with_care0([x],f(x),y);;


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

let check_filename t=
  if Sys.file_exists t
  then t
  else raise(Nonexistent_file(t));;

let join (D s) w=check_filename(s^"/"^w);;

let force_join (D s) w=
   let t=s^"/"^w in
   if Sys.file_exists t
   then t
   else let _=Sys.command("touch "^t) in
        t;;

let help_filename (D s) w=
  if w="" then failwith("empty filename") else
  if (String.get w 0)='/'
  then check_filename w
  else check_filename(s^"/"^w);;


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






module Swoon=struct


(* 

Useful to avoid empty strings between two successive separators in
an archive string. To see example of how it is used, look at the
archive/unarchive function in the new_modulesystem_data module for
example.

#use"swoon.ml";;

*)

let swoon s=if s="" then "#" else s;;
let unswoon s=if s="#" then "" else s;;
  


end;;






module Subdirectory=struct

(*

Subdirectories name, with the trailing slash removed.

#use"subdirectory.ml";;

*)

type t=SD of string;;

let unveil (SD s)=s;;


let of_string s=SD s;;


(*
exception Debbie of string;;

let of_string s=
  if s="" then SD"" else
  let n=String.length(s)-1 in
  if (String.get s n)='/'
  then raise(Debbie(s))
  else SD(s);;
*)

let to_string (SD s)=if s="" then "" else s^"/";;


let ocaml_name (SD s)="Subdirectory"^"."^"of_string(\""^s^"\")";;

end;;






module Stabilize=struct


let one_more_time f (an_holl,da_ober)=
 let l_da_ober=Tidel.forget_order(da_ober) in
 let temp1=Listennou.easy_big_concat(List.rev_map(f)(l_da_ober)) in
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
 let zz=Listennou.easy_big_concat [zz1;zz2;zz3] in
 let temp1=Listennou.easy_big_concat(List.rev_map (function (x,y)->[f x y]) zz ) in
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






module Simplify_without_orderings=struct

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






module More_unix=struct

(*

#use"more_unix.ml";;

*)



type end_file=Absolute_path.t;;
 
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
 
 let complete_ls x=Explicit.explore_tree adhoc_ls [x];;
 
 let complete_ls_with_nondirectories_only x=
  let y=List.filter(is_a_nondirectory_or_a_nib)(complete_ls x) in
  (y:Absolute_path.t list:>end_file list);;
  
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
  let x=Absolute_path.of_string s in
  let temp1=complete_ls x in
  Image.image Absolute_path.to_string temp1;;    

let quick_beheaded_complete_ls s=
  let x=Absolute_path.of_string s in
  let n=String.length(Absolute_path.to_string x) in
  let temp1=complete_ls x in
  Image.image (fun ap->Cull_string.cobeginning n (Absolute_path.to_string ap)) temp1;; 
  
    
let read_process command =
  let buffer_size = 2048 in
  let buffer = Buffer.create buffer_size in
  let string = Bytes.create buffer_size in
  let in_channel = Unix.open_process_in command in
  let chars_read = ref 1 in
  while !chars_read <> 0 do
    chars_read := input in_channel string 0 buffer_size;
    Buffer.add_substring buffer string 0 !chars_read
  done;
  ignore (Unix.close_process_in in_channel);
  Buffer.contents buffer;;    
    
let greadlink x=Cull_string.coending 1 (read_process ("/usr/local/bin/greadlink -f "^x));;    
    
 let symbol_check ap=
   let w=Absolute_path.to_string ap in
   try ((greadlink w)=w) with _->false;;   
 
let symbol_checked_ls x=List.filter symbol_check (ls x);;
    
let watcher_for_leaf_finding="Remembered/continue_looking_for_leaves";;  
   
type marshaled_filename=string;;   
   
let store_leaf_finding_data (mshl:marshaled_filename) x=
	let owen=open_out ("Remembered/"^mshl^".marshal") in
	let _=Marshal.to_channel owen x [Marshal.Compat_32] in
	close_out owen;;

let retrieve_leaf_finding_data (mshl:marshaled_filename)=
	let iris=open_in ("Remembered/"^mshl^".marshal") in
	let answer=((Marshal.from_channel iris ):(string list * int * int * string list)) in
	let _=close_in iris in
	answer;;
  
 let data_for_leaf_finding old_s (mshl:marshaled_filename)=
   let ap=Absolute_path.of_string old_s in
   let s=Absolute_path.to_string ap in
   let tempf=(fun w->Image.image Absolute_path.to_string 
    (symbol_checked_ls(Directory_name.of_string w)) ) 
   and tempg=store_leaf_finding_data mshl in
   let temph=(fun riff->Explicit.find_leaves tempf tempg (watcher_for_leaf_finding,riff)) in
   let _=Sys.command (" touch "^watcher_for_leaf_finding) in
   if Sys.file_exists("Remembered/"^mshl^".marshal")
   then  let data=retrieve_leaf_finding_data mshl in
         (temph,ref(data)) 
   else (temph,ref([],0,1,[s]));;
      
 type complete_filename=string;;    
      
 let make_kosher (stallion:complete_filename) 
   impure_file=
   let s_impure_file=Absolute_path.to_string impure_file in
   (*
   this function performs some hacks on files that the Yosemite system
   refuses to open because it doesn't know their source.
   *)     
   if (not(Sys.file_exists s_impure_file))||(not(Sys.file_exists stallion))
   then failwith("One of the two files is missing")
   else
   let text=Io.read_whole_file impure_file in
   let _=Sys.command("cp "^stallion^" "^s_impure_file^"_temporary_copy") in
   let _=Io.erase_file_and_fill_it_with_string 
   (Absolute_path.of_string(s_impure_file^"_temporary_copy")) text  in
   let _=Sys.command("rm -f "^s_impure_file) in
   let _=Sys.command("mv "^s_impure_file^"_temporary_copy "^s_impure_file) in
   ();;
   
      
 let readable_ocaml_files s=   
   let temp1=beheaded_ls_with_nondirectories_only (Absolute_path.of_string s) in
   let temp2=List.filter (fun z->List.for_all 
       (fun ending->not(Substring.ends_with z ending)) [".cma";".cmi";".cmo";".cmx";".mli"] ) temp1 in
   let temp3=List.filter (fun z->not(List.mem z 
   [".depend";".ocamlinit"]) ) temp2 in
   temp3;;
 
 let string_of_short_int j=
 if (j<10) then "0"^(string_of_int j) else (string_of_int j);;


 
 let print_out (dummy:Format.formatter) ap=
   let x=Absolute_path.to_string ap in
   Format.open_box 0;
   Format.print_string(x);
   Format.close_box();;


   


end;;






module Replace_inside=struct


type patient=string;;
type replacee=string;;
type replacer=string;;

let replace_inside_string (s1:patient) ((a:replacee),(b:replacer))=
  let rgxp=Str.regexp(Str.quote a) in
  ((Str.global_replace rgxp b s1):patient);;
 
let replace_several_inside_string=List.fold_left replace_inside_string;;  
 
let replace_inside_file fn ((a:replacee),(b:replacer))=
    let s1=Io.read_whole_file fn in
    let la=String.length(a) in
    if List.exists (fun j->(String.sub s1 j la)=a) (Ennig.ennig 0 ((String.length s1)-la))
    then let s2=replace_inside_string s1 (a,b) in
         Io.erase_file_and_fill_it_with_string fn s2
    else ();; 
    
let replace_several_inside_file fn l=
    let s1=Io.read_whole_file fn in
    let s2=replace_several_inside_string s1 l in
    Io.erase_file_and_fill_it_with_string fn s2;; 

 type beginning_marker = string;;
 type ending_marker = string;;
 
exception Absent_beginning_marker;;
exception Absent_ending_marker;; 
 
let overwrite_between_markers_inside_string ((bm,em):beginning_marker*ending_marker) 
   (s1:patient) (b:replacer)=
     if (bm,em)=("","") then b else
     let substring_leftmost_index_from=(fun x y i0->
      let lx=String.length(x) and ly=String.length(y) in
      let rec tempf=(fun j->
        if j>ly-lx then (-1) else 
        if (String.sub y j lx)=x then j else (tempf(j+1))
      ) in
      tempf i0) in
     let i1=substring_leftmost_index_from bm s1 0 in
     if i1=(-1) then raise(Absent_beginning_marker) else
     let j1=i1+(String.length bm)-1 in
     let i2=substring_leftmost_index_from em s1 (j1+1) in
     if i2=(-1) then raise(Absent_ending_marker) else
     (* let j2=i2+(String.length em)-1 in *)
     let before=String.sub s1 0 (j1+1)
     and after=String.sub s1 i2 (String.length(s1)-i2) 
     in
     before^b^after ;; 
     
let overwrite_between_markers_inside_file 
   ((bm,em):beginning_marker*ending_marker) 
   fn (b:replacer)=
    let s1=Io.read_whole_file fn in
    let s2=overwrite_between_markers_inside_string (bm,em) s1 b in
    Io.erase_file_and_fill_it_with_string fn s2;;      
 
(* 


 overwrite_between_markers_inside_string ("aaa","bb")
   "123aaa5678bb78910" "456";;    
     
*)






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

Computes the (canonical) maximal acyclic sub-poset, returns
it as a list L where each element of L is a pair (a,anc_a)
where anc_a is the list of all ancestors of a. Actually anc_a 
is a pair (z1,z2) where z1 is ordered as in L and z2 is ordered
by OCaml's default ordering.

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
    then let temp3=coatoms_of_a::(Image.image (fun z->snd(List.assoc z checked)) (coat_a)) in
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
        let (before,_,after)=Three_parts.original_select (fun x->x=p) between in
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






module Preserve_initial_ordering=struct

(*

#use"preserve_initial_ordering.ml";;

*)

let preserve_initial_ordering l=
    let rec tempf=(fun
    (treated_part,ordered_treated_part,yet_untreated)->
      match yet_untreated with
      []->Listennou.hard_big_concat(List.rev(treated_part))
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
      []->Listennou.hard_big_concat(List.rev(treated_part))
      |x::remains->
        let better_x=List.filter 
        (fun y->Tidel.nelfenn y ordered_treated_part) x in
        if better_x=[]
        then tempf(treated_part,ordered_treated_part,remains)
        else
        let temp1=Tidel.teuzin(Tidel.diforchan x) ordered_treated_part in
        let temp2=List.rev(better_x) in
        let temp3=(List.hd temp2,true)::(Image.image (fun t->(t,false)) (List.tl temp2)) in
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






module Charset=struct

(*

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
    
 let look_for_capitalized_identifiers s=
   let n=String.length s in
   let rec tempf=(fun (graet,j,j0)->
       if (j>=(n-1)) then List.rev(graet) else
       let c=String.get s (j+1) in
       if (j0>=0)
       then if List.mem c lowercase_identifier_elements
            then tempf(graet,j+1,j0)
            else let s1=String.lowercase(String.sub s j0 (j-j0+1)) in
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
let is_an_uppercase_letter c=List.mem c uppercase_letters;;   
  
let string_is_alphanumeric s=
   List.for_all (fun j->
     character_is_alphanumeric(String.get s j)
   ) (Ennig.ennig 0 (String.length(s)-1));;  
  

  
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






module Current_root_directory=struct

(*

#use"current_root_directory.ml";;

*)


let current_root_directory=Directory_name.of_string
current_sick_directory;;

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






module Ocaml_ending=struct

(*

#use"ocaml_ending.ml";;

*)

type t=Ml |Mli |Mll |Mly;;

let ml=Ml and mli=Mli and mll=Mll and mly=Mly;;



let pattern_matching f_ml f_mli f_mll f_mly=function
   Ml->f_ml |Mli->f_mli |Mll->f_mll |Mly->f_mly;;

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
  try (fst(Option.find_it_really (fun (x,y)->y=s) correspondances))
  with _->raise(Unknown_ending(s));;

let to_string edg=snd(Option.find_it_really (fun (x,y)->x=edg) correspondances);;  



let ocaml_name w=
 let c="Ocaml_ending"^"." in
 match w with
 Ml->c^"Ml"
|Mli->c^"Mli"
|Mll->c^"Mll"
|Mly->c^"Mly";;


end;;






module Ocaml_library=struct


(* 




#use"Makefile_makers/ocaml_library.ml";;


*)


type t=NumLib |StrLib |UnixLib;;

let correspondances=[NumLib,"num";StrLib,"str";UnixLib,"unix"];;

exception Unknown_lib of string;;

let of_string s=
  try (fst(Option.find_it_really (fun (x,y)->y=s) correspondances))
  with _->raise(Unknown_lib(s));;

let to_string lib=snd(Option.find_it_really (fun (x,y)->x=lib) correspondances);;  


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
NumLib->["num";"big_int";"arith_status."] 
|StrLib->["str"] 
|UnixLib->["unix"];;    
  
let all_libraries=[NumLib;StrLib;UnixLib];;  



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


let root_directory (HD(s,dir))= dir;;

let module_name (HD (s,dir))=
  let t=Father_and_son.son s '/'  in
  (String.capitalize t);;

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
  Ocaml_ending.pattern_matching
  (ML (s,dir)) (MLI (s,dir)) (MLL (s,dir)) (MLY (s,dir)) ending;;
  
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
    inactivity_count : int
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
let inactivity_count x=x.inactivity_count;;

let modification_time x edg=
  Ocaml_ending.pattern_matching
  (ml_modification_time x)
  (mli_modification_time x)
  (mll_modification_time x)
  (mly_modification_time x) edg;;

let make (nam,mlp,mlip,mllp,mlyp,mlmt,mlimt,mllmt,mlymt,libned,dirfath,allanc,dirned,inac)=
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
    inactivity_count=inac

};;

let compact_make (dir,nam,mlp,mlip,mllp,mlyp,mlmt,mlimt,mllmt,mlymt,libned,dirfath,allanc,dirned,inac)=
  make (Half_dressed_module.of_string_and_root nam dir,
  		mlp,mlip,mllp,mlyp,
  		mlmt,mlimt,mllmt,mlymt,
  		Image.image Ocaml_library.of_string libned,
  		Image.image (fun s->Half_dressed_module.of_string_and_root s dir) dirfath,
  		Image.image (fun s->Half_dressed_module.of_string_and_root s dir) allanc,
  		Image.image Subdirectory.of_string dirned,inac);;

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
    inactivity_count=x.inactivity_count

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
    inactivity_count=x.inactivity_count


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
    inactivity_count=x.inactivity_count


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
    inactivity_count=x.inactivity_count


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
    inactivity_count=x.inactivity_count


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
    inactivity_count=x.inactivity_count


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
    inactivity_count=x.inactivity_count


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
    inactivity_count=x.inactivity_count


};;  
  
let check_presence ending dt=
   Ocaml_ending.pattern_matching 
    dt.ml_present
    dt.mli_present
    dt.mll_present
    dt.mly_present
    ending;; 

let make_presence ending dt=
   Ocaml_ending.pattern_matching 
    (make_ml_present dt)
    (make_mli_present dt)
    (make_mll_present dt)
    (make_mly_present dt)
    ending;;  
  
let make_absence ending dt=
   Ocaml_ending.pattern_matching 
    (make_ml_absent dt)
    (make_mli_absent dt)
    (make_mll_absent dt)
    (make_mly_absent dt)
    ending;;    

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
    inactivity_count=x.inactivity_count


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
    		inactivity_count=x.inactivity_count

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
    inactivity_count=x.inactivity_count


   };;       
       
let is_optional x=Half_dressed_module.is_optional(x.name);;
let is_not_optional x=not(is_optional x);;

let is_executable x=Half_dressed_module.is_executable(x.name);;
let is_not_executable x=not(is_executable x);;




let outdated_acolytes dt=
  let hm=dt.name in
  let (n_ml,n_mli,n_mll,n_mly)=compute_modification_times hm in
  let temp1=[
    Ocaml_ending.mll,dt.mll_modification_time,n_mll;
    Ocaml_ending.mly,dt.mly_modification_time,n_mly;
    Ocaml_ending.ml , dt.ml_modification_time,n_ml ;
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
    inactivity_count=x.inactivity_count


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
    inactivity_count=x.inactivity_count


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
    inactivity_count=x.inactivity_count


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
    inactivity_count=x.inactivity_count


};;

let force_modification_time x edg new_val=
  Ocaml_ending.pattern_matching
   (force_ml_modification_time x new_val) 
   (force_mli_modification_time x new_val) 
   (force_mll_modification_time x new_val) 
   (force_mly_modification_time x new_val) 
   edg;;

let increment_inactivity_count x=
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
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;
    inactivity_count=(x.inactivity_count+1)
};;

let reset_inactivity_count x=
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
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;
    inactivity_count=0
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
    inactivity_count=x.inactivity_count
};;

type is_optimized=bool;;


let needed_dirs_and_libs (bowl:is_optimized) dt=
   let extension=(if bowl then ".cmxa" else ".cma") in
   let dirs=String.concat(" ")
    (Image.image(fun y->"-I "^(Subdirectory.to_string(y)) )
    dt.needed_directories)
	and libs=String.concat(" ")
    (Image.image(fun z->Ocaml_library.file_for_library(z)^extension)
    dt.needed_libraries) in
    String.concat " " ["";dirs;libs;""];;

let needed_dirs_and_libs_for_several (bowl:is_optimized) l_dt=
   let extension=(if bowl then ".cmxa" else ".cma") in
   let pre_dirs1=Image.image (fun dt->Tidel.diforchan(dt.needed_directories)) l_dt in
   let pre_dirs2=Ordered.forget_order (Tidel.big_teuzin pre_dirs1) in
   let dirs=String.concat(" ")
    (Image.image(fun y->"-I "^(Subdirectory.to_string(y)) )
    pre_dirs2) in
   let pre_libs1=Image.image (fun dt->Tidel.diforchan(dt.needed_libraries)) l_dt in
   let pre_libs2=Ordered.forget_order (Tidel.big_teuzin pre_libs1) in 
   let libs=String.concat(" ")
    (Image.image(fun z->Ocaml_library.file_for_library(z)^extension)
    pre_libs2) in
    String.concat " " ["";dirs;libs;""];;


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
(*  
let sw=Swoon.swoon;;
*)  
  
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
     Swoon.swoon(String.concat industrial_separator2 (Image.image Ocaml_library.to_string x.needed_libraries));
     Swoon.swoon(String.concat industrial_separator2 (Image.image Half_dressed_module.to_string x.direct_fathers));
     Swoon.swoon(String.concat industrial_separator2 (Image.image Half_dressed_module.to_string x.all_ancestors));
     Swoon.swoon(String.concat industrial_separator2 (Image.image Subdirectory.unveil x.needed_directories));
     string_of_int(x.inactivity_count)
   ];;

let unarchive s=
   let l1=Str.split (Str.regexp_string industrial_separator1) s in
   let v1=Str.split (Str.regexp_string industrial_separator2) (Swoon.unswoon(List.nth l1  9))
   and v2=Str.split (Str.regexp_string industrial_separator2) (Swoon.unswoon(List.nth l1 10))
   and v3=Str.split (Str.regexp_string industrial_separator2) (Swoon.unswoon(List.nth l1 11))
   and v4=Str.split (Str.regexp_string industrial_separator2) (Swoon.unswoon(List.nth l1 12)) in
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
    inactivity_count = int_of_string(List.nth l1 13);
};;
     


end;;






module Modulesystem=struct


(* 

#use"Makefile_makers/modulesystem.ml";;


The only allowed combinations are .mll .ml or .mly .ml or .mli .ml.


*)




type t={
  root : Directory_name.t;
  data : Modulesystem_data.t list;
  main_toplevel_name:string;
};;


let make (dir,l,mtn)={root=dir;data=l;main_toplevel_name=mtn;};;


let from_root_and_toplevel dir opt=
  let mtn=(match opt with
   None->"ocaml_toplevel"
   |Some(s)->s
  ) in
  make (dir,[],mtn);;

let filedata_selector ending x=List.filter 
   (Modulesystem_data.check_presence ending) x.data;;

let root x=x.root;; 
let all_filedata x=x.data;;
let main_toplevel_name x=x.main_toplevel_name;;
let ml_filedata = filedata_selector Ocaml_ending.ml;;
let mli_filedata = filedata_selector Ocaml_ending.mli;;
let mll_filedata = filedata_selector Ocaml_ending.mll;;
let mly_filedata = filedata_selector Ocaml_ending.mly;;     
   
   

let all_mlx_files x=
  Listennou.hard_big_concat
  (Image.image Modulesystem_data.acolytes x.data);; 
  
let all_mlx_paths x=Image.image Mlx_filename.to_absolute_path (all_mlx_files x);;  
  
let local_directories fs=
  let temp1=all_mlx_files fs in
  let temp5=Image.image Mlx_filename.to_string temp1 in
  let temp2=Image.image (fun s->Father_and_son.father s '/') temp5 in
  let temp3=Ordered_string.diforchan temp2 in
  let temp4=Ordered.forget_order temp3 in
  temp4;;

let find_module_registration fs hm=
  Option.find_it(fun a->Modulesystem_data.name a=hm) fs.data;;   
   
 let see_if_file_is_registered fs mlx=
    let hm=Mlx_filename.half_dressed_core mlx
    and edg=Mlx_filename.ending mlx in  
    match Option.find_it (fun a->Modulesystem_data.name a=hm) (fs.data) with
    None->false
    |Some(dt)->Modulesystem_data.check_presence edg dt;;
 
let check_presences fs hm=
    match Option.find_it (fun a->Modulesystem_data.name a=hm) (fs.data) with
    None->Ocaml_ending.exhaustive_uple (fun _->false)
    |Some(dt)->Ocaml_ending.exhaustive_uple 
     (fun edg->Modulesystem_data.check_presence edg dt);;
 
 let acolytes fs hm=
    match Option.find_it (fun a->Modulesystem_data.name a=hm) 
      (fs.data) with
     None->[]
    |Some(dt)->Modulesystem_data.acolytes dt;;    
   
 
 let outdated_files fs=
   let temp1=Image.image Modulesystem_data.outdated_acolytes fs.data in
   Listennou.hard_big_concat temp1;;
 

   
 let descendants fs names=
    let temp1=List.filter(
      fun dt->List.exists (fun t->List.mem t names) 
        (Modulesystem_data.all_ancestors dt)
    )(fs.data) in
    temp1;;
    
let optionality_partition l=
  let (before,core,after)=Three_parts.select_center_element 
    Modulesystem_data.is_optional l in
  (before,Option.add_optionally core after);;    

exception Deletability_issue of Mlx_filename.t;;

 let is_deletable fs mlxfile=
   let hm=Mlx_filename.half_dressed_core mlxfile in
   if (descendants fs [hm])<>[]
   then false
   else let edg=Mlx_filename.ending mlxfile in
        if List.mem edg [Ocaml_ending.ml;Ocaml_ending.mli]
        then true
        else 
        if List.mem edg [Ocaml_ending.mll;Ocaml_ending.mly]
        then let opt=Option.find_it (fun a->Modulesystem_data.name a=hm) fs.data in
             (
               match opt with
               None->true
               |Some(dt)->not(Modulesystem_data.ml_present dt)
             )
        else raise(Deletability_issue(mlxfile));; 
 
 let unregistered_mlx_files fs=
   let temp1=Mlx_filename.complete_ls (root fs) in
   List.filter (fun mlx->
     not(see_if_file_is_registered fs mlx)
   ) temp1;;
 
 let system_size x=List.length(x.data);;
 
exception  Non_registered_module of Half_dressed_module.t;;
 
let above fs hm=
   let files=all_filedata fs in
   match Option.find_it(fun dt->Modulesystem_data.name dt=hm) files with
    None->raise(Non_registered_module(hm))
   |Some(dt)->Modulesystem_data.all_ancestors dt;;
   
let below fs hm=
   let files=all_filedata fs in
   Option.filter_and_unpack(fun dt->
   if List.mem hm (Modulesystem_data.all_ancestors dt)
   then Some(Modulesystem_data.name dt)
   else None) files;;   
 
let directly_below fs hm=
   let files=all_filedata fs in
   Option.filter_and_unpack(fun dt->
   if List.mem hm (Modulesystem_data.direct_fathers dt)
   then Some(Modulesystem_data.name dt)
   else None) files;;    
 
let inactivity_report fs=
  let temp1=List.filter Modulesystem_data.is_not_optional fs.data in 
  let (_,m_min)=Min.minimize_it Modulesystem_data.inactivity_count temp1
  and (m_max,l_max)=Max.maximize_it_with_care Modulesystem_data.inactivity_count temp1 in
  let temp2=Image.image Modulesystem_data.name l_max in
  let temp3=List.filter (fun hm->directly_below fs hm=[]) temp2 in
  (m_max-m_min,(fun ()->temp3));;

let files_containing_string fs old_name=
   let temp1=all_mlx_paths fs in
   List.filter (fun ap->Substring.is_a_substring_of 
     old_name (Io.read_whole_file ap)) temp1;;
 
 let sliced_ocaml_name fs=
   let temp1=Image.image (fun dt->
     (Modulesystem_data.unprefixed_compact_ocaml_name dt)^";"
   ) fs.data in
   
   Sliced_string.of_string_list(
   [
     "Module"^"system.make(";
     "";
     "\t"^(Directory_name.ocaml_name(root  fs))^",";
     "";
     "\tImage.image Modulesystem_data.compact_make [";
     ""
    ]
    @temp1@
    [
     "";
     "],";
     "\""^(fs.main_toplevel_name)^"\")"
    ]
    );;
 
 let ocaml_name fs=Sliced_string.print(sliced_ocaml_name fs);;
 
 
let industrial_separator1=Industrial_separator.new_separator ();;  
let industrial_separator2=Industrial_separator.new_separator ();;    

  
let archive x=
   String.concat industrial_separator1
   [
     Directory_name.to_string(x.root);
     Swoon.swoon(String.concat industrial_separator2 (Image.image Modulesystem_data.archive x.data));
     x.main_toplevel_name
   ];;

let unarchive s=
   let l1=Str.split (Str.regexp_string industrial_separator1) s in
   let v1=Str.split (Str.regexp_string industrial_separator2) (Swoon.unswoon(List.nth l1  1)) in
{
    root = Directory_name.of_string(List.nth l1 0);
    data =Image.image Modulesystem_data.unarchive v1;
    main_toplevel_name =List.nth l1  2
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

let main_module=function
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


 
let toplevel_from_modulesystem fs=
  let temp1=Modulesystem.all_filedata fs in
  let temp2=List.filter Modulesystem_data.is_not_optional temp1 in
  let temp3=Image.image Modulesystem_data.name temp2
  in
  toplevel (Modulesystem.main_toplevel_name fs) temp3;; 
 
let pattern_matching 
  f_nodep f_mll f_mly f_cmi f_cmo f_dcmo f_cma f_cmx f_exec f_dbg f_top=function
  NO_DEPENDENCIES(mlx)->f_nodep mlx 
 |ML_FROM_MLL(hm)->f_mll hm
 |ML_FROM_MLY(hm)->f_mly hm
 |CMI(hm)->f_cmi hm
 |CMO(hm)->f_cmo hm
 |DCMO(hm)->f_dcmo hm
 |CMA(hm)->f_cma hm
 |CMX(hm)->f_cmx hm
 |EXECUTABLE(hm)->f_exec hm
 |DEBUGGABLE(hm)->f_dbg hm
 |TOPLEVEL(name,l)->f_top name l;;
 
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
  Swoon.swoon(String.concat industrial_separator1 (Image.image Half_dressed_module.archive l))];;
  
  
  
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
   then let v1=Str.split (Str.regexp_string industrial_separator1) (Swoon.unswoon(List.nth l1  2)) in 
        TOPLEVEL(ms,Image.image Half_dressed_module.unarchive v1) 
   else
   raise(Unrecognized_constructor(c));;

end;;






module Compute_modulesystem_directories=struct

(*

#use"Makefile_makers/isidore_compute_modulesystem_directories.ml";;

*)


let individual_directory hm=
       let s_hm=Half_dressed_module.to_string hm in
       let s_dir=Father_and_son.father s_hm '/' in
       Subdirectory.of_string s_dir;;

let compute_modulesystem_directories fs=
  let temp1=Modulesystem.all_filedata fs in
  let temp2=Image.image (
    fun dt->
       let hm=Modulesystem_data.name dt in
       individual_directory hm
  ) temp1 in
  let temp3=Tidel.diforchan(temp2) in
  Ordered.forget_order temp3;;
 


end;;






module Target_system=struct

(*

#use"Makefile_makers/isidore_target_system.ml";;

The up_to_date_targets record only stores non-mlx targets.
A non-mlx target is up to date when it has been produced
from the latest mlx targets.

*)

type t={
   modulesystem : Modulesystem.t;
   directories : Subdirectory.t list;
   up_to_date_targets : Ocaml_target.t list
};;

let make filesys l_dirs l_targs={
   modulesystem=filesys;
   directories=l_dirs;
   up_to_date_targets=l_targs;
};;

let modulesystem ts=ts.modulesystem;;
let directories ts=ts.directories;;
let up_to_date_targets ts=ts.up_to_date_targets;;

let root ts=Modulesystem.root (ts.modulesystem);;
let main_toplevel_name ts=Modulesystem.main_toplevel_name (ts.modulesystem);;
let find_module_registration ts mlx=Modulesystem.find_module_registration (ts.modulesystem) mlx;;
let all_mlx_files ts=Modulesystem.all_mlx_files (ts.modulesystem);;

let from_modulesystem fs=
  let l_dirs=Compute_modulesystem_directories.compute_modulesystem_directories fs in
  make fs l_dirs [];;
 

let from_root_and_toplevel dir opt=
  from_modulesystem(Modulesystem.from_root_and_toplevel dir opt);; 

 
let unregistered_mlx_files ts=Modulesystem.unregistered_mlx_files ts.modulesystem;; 
 
let sliced_ocaml_name ts=
   let l_dirs=
       [
    		 ")"; 
    		"( Image.image Subdirectory.of_string [";
  		]
  		@(Image.image (fun dir->"\""^(Subdirectory.to_string(dir))^"\";") ts.directories)@ 
  		[  
    		"])"; 
    		"([";
  		]
  and l_targets=Image.image (fun tgt->
     Sliced_string.concat_two (Ocaml_target.sliced_ocaml_name(tgt))
       (Sliced_string.of_string_list [";"])
  ) ts.up_to_date_targets in	
  let sls1=Sliced_string.of_string_list [ "Target_"^"system"^"."^"make";"(" ]
  and sls2=Modulesystem.sliced_ocaml_name ts.modulesystem  
  and sls3=Sliced_string.of_string_list l_dirs 
  and sls4=Sliced_string.concat l_targets 
  and sls5=Sliced_string.of_string_list [ "])" ] in
  Sliced_string.concat [sls1;sls2;sls3;sls4;sls5];;
 
let ocaml_name ts=Sliced_string.print(sliced_ocaml_name ts);;

 
let all_modules ts=
  let temp1=Modulesystem.all_filedata ts.modulesystem in
  let temp2=Image.image (fun dt->
     Half_dressed_module.to_string(Modulesystem_data.name dt)
  ) temp1 in
  temp2;;
 
let system_size x=Modulesystem.system_size(x.modulesystem);; 
 
let industrial_separator1=Industrial_separator.new_separator ();;  
let industrial_separator2=Industrial_separator.new_separator ();;    

  
  
let archive x=
   let temp1=Image.image (fun w->Swoon.swoon(Subdirectory.unveil w)) x.directories in
   String.concat industrial_separator1
   [
     Modulesystem.archive x.modulesystem;
     Swoon.swoon(String.concat industrial_separator2 temp1);
     Swoon.swoon(String.concat industrial_separator2 (Image.image Ocaml_target.archive x.up_to_date_targets))
   ];;

let unarchive s=
   let l1=Str.split (Str.regexp_string industrial_separator1) s in
   let v1=Str.split (Str.regexp_string industrial_separator2) (Swoon.unswoon(List.nth l1  1))
   and v2=Str.split (Str.regexp_string industrial_separator2) (Swoon.unswoon(List.nth l1 2)) in
{
    modulesystem = Modulesystem.unarchive(List.nth l1 0);
    directories = Image.image (fun v->Subdirectory.of_string(Swoon.unswoon v)) v1;
    up_to_date_targets =Image.image Ocaml_target.unarchive v2;
    
};;
    
   

 
 

end;;






module Targets_from_modulesystem=struct

(*

#use"Makefile_makers/isidore_targets_from_modulesystem.ml";;

*)

let from_modulesystem_data dt=
  let hm=Modulesystem_data.name dt 
  and (mlp,mlip,mllp,mlyp)=Modulesystem_data.presences dt in
  let temp1=[
                mllp,Ocaml_target.ml_from_mll hm;
                mlyp,Ocaml_target.ml_from_mly hm;
           mlp||mlip,Ocaml_target.cmi hm;
     	   mlp||mlip,Ocaml_target.cmo hm;
           mlp||mlip,Ocaml_target.cma hm;
           mlp||mlip,Ocaml_target.cmx hm;
                 mlp,Ocaml_target.executable hm;
  ] in
  Option.filter_and_unpack (fun x->if fst x then Some(snd x) else None) temp1;;
  
let from_modulesystem fs=
  let l_dt=Modulesystem.all_filedata fs in
  let temp1=Image.image from_modulesystem_data l_dt 
  and temp2=Image.image Modulesystem_data.name l_dt in
  let temp3=Listennou.hard_big_concat temp1 
  and mtn=Modulesystem.main_toplevel_name fs in
  temp3@[Ocaml_target.toplevel mtn temp2]
  ;;


 
 


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


let unveil=function
 (C (s,ann,fai))->(s,ann,fai)
 |ChangeDir(dir_string,ann,fai)->("cd "^dir_string,ann,fai);;

exception Content_of_cd of string;;

let command_content=function 
 (C (s,ann,fai))->s
 |ChangeDir(dir_string,ann,fai)->raise(Content_of_cd(dir_string));;

let usual s=C(s,"","Failed during "^s);;
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
    
let take_care_of_root_directory fs l=
  let root=Modulesystem.root fs in
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






module Ingredients_for_ocaml_target=struct

(*

It is assumed that no "manual tampering" is made,
e.g. manual rewriting of a ml coming from a mll, etc.


#use"Makefile_makers/ingredients_for_ocaml_target.ml";;

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

let targets_from_ancestors fs dt=
     let ancestors=Modulesystem_data.all_ancestors dt in
     let temp1=Image.image (fun hm2->
            let opt2=Modulesystem.find_module_registration fs hm2 in
            let dt2=Option.unpack opt2 in
            targets_from_ancestor_data dt2
          ) ancestors in
     Preserve_initial_ordering.preserve_initial_ordering temp1;;

let debuggable_targets_from_ancestors fs dt=
     let ancestors=Modulesystem_data.all_ancestors dt in
     let temp1=Image.image (fun hm2->
            let opt2=Modulesystem.find_module_registration fs hm2 in
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

let optimized_targets_from_ancestors fs dt=
     let ancestors=Modulesystem_data.all_ancestors dt in
     let temp1=Image.image (fun hm2->
            let opt2=Modulesystem.find_module_registration fs hm2 in
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

let ingredients_for_ml_from_mll fs hm=
  let opt=Modulesystem.find_module_registration fs hm in
  if opt=None then raise(Unregistered_ml_from_mll(hm)) else 
  let dt=Option.unpack opt in
  (targets_from_ancestors fs dt)@(immediate_ingredients_for_ml_from_mll hm);;

let ingredients_for_ml_from_mly fs hm=
  let opt=Modulesystem.find_module_registration fs hm in
  if opt=None then raise(Unregistered_ml_from_mly(hm)) else 
  let dt=Option.unpack opt in
  (targets_from_ancestors fs dt)@(immediate_ingredients_for_ml_from_mly hm);;

let ingredients_for_cmi fs hm=
          let opt=Modulesystem.find_module_registration fs hm in
          if opt=None then raise(Unregistered_cmi(hm)) else 
          let dt=Option.unpack opt in
          (targets_from_ancestors fs dt)@(immediate_ingredients_for_cmi dt hm);;

let ingredients_for_cmo fs hm=
          let opt=Modulesystem.find_module_registration fs hm in
          if opt=None then raise(Unregistered_cmo(hm)) else 
          let dt=Option.unpack opt in
          (targets_from_ancestors fs dt)@(immediate_ingredients_for_cmo dt hm);;

let ingredients_for_dcmo fs hm=
          let opt=Modulesystem.find_module_registration fs hm in
          if opt=None then raise(Unregistered_dcmo(hm)) else 
          let dt=Option.unpack opt in
          (debuggable_targets_from_ancestors fs dt)@(immediate_ingredients_for_dcmo dt hm);;

let ingredients_for_cma fs hm=
          let opt=Modulesystem.find_module_registration fs hm in
          if opt=None then raise(Unregistered_cma(hm)) else 
          let dt=Option.unpack opt in
          (targets_from_ancestors fs dt)@(immediate_ingredients_for_cma dt hm);;

let ingredients_for_cmx fs hm=
          let opt=Modulesystem.find_module_registration fs hm in
          if opt=None then raise(Unregistered_cmx(hm)) else 
          let dt=Option.unpack opt in
          (optimized_targets_from_ancestors fs dt)@(immediate_ingredients_for_cmx dt hm);;
 
let ingredients_for_executable fs hm=
  let opt=Modulesystem.find_module_registration fs hm in
  if opt=None then raise(Unregistered_executable(hm)) else 
  let dt=Option.unpack opt in
  (optimized_targets_from_ancestors fs dt)
  @(immediate_ingredients_for_executable hm);; 
  
let ingredients_for_debuggable fs hm=
  let opt=Modulesystem.find_module_registration fs hm in
  if opt=None then raise(Unregistered_debuggable(hm)) else 
  let dt=Option.unpack opt in
  (debuggable_targets_from_ancestors fs dt)
  @(immediate_ingredients_for_debuggable hm);;   
  
let ingredients_for_toplevel_element fs name hm=
   let opt=Modulesystem.find_module_registration fs hm in
  if opt=None then raise(Unregistered_module_in_toplevel(name,hm)) else 
  let dt=Option.unpack opt in
  if (Modulesystem_data.mli_present dt)&&(not(Modulesystem_data.ml_present dt))
  then (ingredients_for_cmi fs hm)@[Ocaml_target.cmi hm]
  else (ingredients_for_cmo fs hm)@[Ocaml_target.cmo hm];;  
  
let ingredients_for_toplevel fs name l=
  let temp1=Image.image (ingredients_for_toplevel_element fs name) l in
  Preserve_initial_ordering.preserve_initial_ordering temp1;;
      
 
let ingredients_for_ocaml_target fs tgt=
  Ocaml_target.pattern_matching
    (ingredients_for_nodep)
    (ingredients_for_ml_from_mll  fs)
    (ingredients_for_ml_from_mly  fs)
  	(ingredients_for_cmi fs)
    (ingredients_for_cmo fs)
    (ingredients_for_dcmo fs)
    (ingredients_for_cma fs)
    (ingredients_for_cmx fs)
    (ingredients_for_executable  fs)
    (ingredients_for_debuggable  fs)
    (ingredients_for_toplevel fs)
    tgt;;



let marked_ingredients_for_unprepared_toplevel fs name l=
  let temp1=Image.image (ingredients_for_toplevel_element fs name) l in
  Preserve_initial_ordering.and_mark_endings temp1;;

let module_dependency_for_nodep mlx=false;;
let module_dependency_for_ml_from_mll fs l_hm hm1=
       if List.mem hm1 l_hm
       then true
       else  
       let dt1=Option.unpack(Modulesystem.find_module_registration fs hm1) in
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
let module_dependency_for_toplevel fs l_hm name l_hm2=
  List.exists(fun hm2->
  (module_dependency_for_cmo fs l_hm hm2)||(List.mem hm2 l_hm)
  ) l_hm2;;

let module_dependency_for_ocaml_target fs l_hm tgt=
  Ocaml_target.pattern_matching
    (module_dependency_for_nodep)
    (module_dependency_for_ml_from_mll  fs l_hm)
    (module_dependency_for_ml_from_mly  fs l_hm)
  	(module_dependency_for_cmi  fs l_hm)
    (module_dependency_for_cmo  fs l_hm)
    (module_dependency_for_dcmo fs l_hm)
    (module_dependency_for_cma  fs l_hm)
    (module_dependency_for_cmx  fs l_hm)
    (module_dependency_for_executable  fs l_hm)
    (module_dependency_for_debuggable  fs l_hm)
    (module_dependency_for_toplevel fs l_hm)
    tgt;;



let mlx_dependency_for_ocaml_target fs mlx tgt=
  let hm=Mlx_filename.half_dressed_core mlx in
  module_dependency_for_ocaml_target fs [hm] tgt;;

let mlx_list_dependency_for_ocaml_target fs l_mlx tgt=
 List.exists (fun mlx->mlx_dependency_for_ocaml_target fs mlx tgt) l_mlx;;

let still_up_to_date_targets fs l_mlx l_tgt=
 List.filter (fun tgt->not(mlx_list_dependency_for_ocaml_target fs l_mlx tgt)) 
 l_tgt;;




end;;






module Command_for_ocaml_target=struct

(*

#use"Makefile_makers/command_for_ocaml_target.ml";;

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

let ingr=Ingredients_for_ocaml_target.ingredients_for_ocaml_target;;

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

let command_for_ml_from_mll fs hm=
          let s_hm=Half_dressed_module.to_string hm in
          let s=
          "ocamllex "^
          " -o "^s_hm^".ml"^
          	 " "^s_hm^".mll" in
          [Shell_command.usual s];; 
 
let command_for_ml_from_mly fs hm=
          let s_hm=Half_dressed_module.to_string hm in
          let s=
          "ocamlyacc "^
          " -o "^s_hm^".ml"^
          	 " "^s_hm^".mly" in
          [Shell_command.usual s];;  

let command_for_cmi fs hm=
          let opt=Modulesystem.find_module_registration fs hm in
          if opt=None then raise(Unregistered_cmi(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let ending=(
          if Modulesystem_data.mli_present dt
          then ".mli"
          else ".ml"
          ) in
          let s1=
          "ocamlc "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -c "^s_hm^ending in
          let s_dir=Directory_name.to_string(Modulesystem.root fs) in
          let full_mli=s_dir^s_hm^".mli" in
          if (not(Modulesystem_data.mli_present dt))
             &&(Sys.file_exists(full_mli))
          then  let dummy_mli=s_dir^"uvueaoqhkt.mli" in
                let s2="mv "^full_mli^" "^dummy_mli
                and s3="mv "^dummy_mli^" "^full_mli in
                [Shell_command.usual s2;
                 Shell_command.usual s1;
                 Shell_command.usual s3]
          else   [Shell_command.usual s1];;

let command_for_cmo fs hm=
          let opt=Modulesystem.find_module_registration fs hm in
          if opt=None then raise(Unregistered_cmo(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s=
          "ocamlc "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_hm^".cmo"^
          " -c "^s_hm^".ml" in
          [Shell_command.usual s];;

let command_for_dcmo fs hm=
          let opt=Modulesystem.find_module_registration fs hm in
          if opt=None then raise(Unregistered_dcmo(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s=
          "ocamlc -g "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_hm^".d.cmo"^
          " -c "^s_hm^".ml" in
          [Shell_command.usual s];;

          
let command_for_cma fs hm=
          let opt=Modulesystem.find_module_registration fs hm in
          if opt=None then raise(Unregistered_cma(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s=
          "ocamlopt -a "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_hm^".cma"^
          " -c "^s_hm^".ml" in
          [Shell_command.usual s];;
 
let command_for_cmx fs hm=
          let opt=Modulesystem.find_module_registration fs hm in
          if opt=None then raise(Unregistered_cmx(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s=
          "ocamlopt "^(Modulesystem_data.needed_dirs_and_libs true dt)^
          " -o "^s_hm^".cma"^
          " -c "^s_hm^".ml" in
          [Shell_command.usual s];; 
 

let command_for_executable fs hm=
          let opt=Modulesystem.find_module_registration fs hm in
          if opt=None then raise(Unregistered_executable(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let temp1=ingr fs (Ocaml_target.EXECUTABLE(hm)) in
          let temp2=Option.filter_and_unpack cmx_manager temp1 in
          let s=
          "ocamlopt "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_hm^".caml_executable"^
          (String.concat " " temp2) in
          [Shell_command.usual s];; 
  
let command_for_debuggable fs hm=
          let opt=Modulesystem.find_module_registration fs hm in
          if opt=None then raise(Unregistered_debuggable(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let short_s_hm=Father_and_son.son s_hm '/' in
          let temp1=ingr fs (Ocaml_target.DEBUGGABLE(hm)) in
          let temp2=Option.filter_and_unpack dcmo_manager temp1 in
          let s=
          "ocamlc -g "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^short_s_hm^".caml_debuggable "^
          (String.concat " " temp2) in
          [Shell_command.usual s];; 
  
let command_for_toplevel fs name l=
          let temp1=Image.image (fun hm->(hm,Modulesystem.find_module_registration fs hm)) l  in
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
          let s=
          "ocamlmktop "^(Modulesystem_data.needed_dirs_and_libs_for_several false l_dt)^
          " -o "^name^" "^
          "  "^s_lhm^" " in
          [Shell_command.usual s];;   
 
let command_for_ocaml_target fs tgt=
  Ocaml_target.pattern_matching
  	command_for_nodep
  	(command_for_ml_from_mll  fs)
    (command_for_ml_from_mly  fs)
  	(command_for_cmi fs)
    (command_for_cmo fs)
    (command_for_dcmo fs)
    (command_for_cma fs)
    (command_for_cmx fs)
    (command_for_executable  fs)
    (command_for_debuggable  fs)
    (command_for_toplevel fs)
    tgt;;
  
  
 let command_for_ocaml_target_in_dir fs tgt=
   Shell_command.take_care_of_root_directory fs (command_for_ocaml_target fs tgt);; 
          

 


end;;






module Write_makefile=struct

(*

It is assumed that no "manual tampering" is made,
e.g. manual rewriting of a ml coming from a mll, etc.


#use"Makefile_makers/write_makefile.ml";;

*)




let write_makefile_element fs tgt=
  let temp1=Ingredients_for_ocaml_target.ingredients_for_ocaml_target fs tgt
  and temp2=Command_for_ocaml_target.command_for_ocaml_target fs tgt in
  let temp3=Image.image Ocaml_target.to_string temp1 in
  let temp4=Sliced_string.make_aggregates " " temp3 in
  let temp5=Sliced_string.to_string_list temp4 in
  let temp6=Image.image Shell_command.command_content temp2 in
  let pre_temp7=String.concat "\n" temp6 in
  let temp7=Str.split(Str.regexp" ") pre_temp7 in
  let temp8=Sliced_string.make_aggregates " " temp7 in
  let temp9=Sliced_string.to_string_list temp8 in 
  let s1=(Ocaml_target.to_string tgt)^" : " 
  and s2=String.concat " \\\n" temp5
  and s3="\n\t"
  and s4=String.concat " \\\n" temp9 in
  String.concat "" [s1;s2;s3;s4];;
  
  
let write_makefile fs=
  let temp1=Targets_from_modulesystem.from_modulesystem fs in
  let temp2=Image.image (write_makefile_element fs) temp1 in
  let temp3=Image.image Ocaml_target.to_string temp1 in
  let temp4=Sliced_string.make_aggregates " " temp3 in
  let temp5=Sliced_string.to_string_list temp4 in
  let temp6=String.concat " \\\n" temp5 in
  let temp7="clean :\n\trm -r -f "^temp6^"\n\n" in
  String.concat "\n\n" (temp2@[temp7]);;

  

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
   
let rec find_and_stop_immediately_after f l=
 let rec find_and_stop_immediately_after0=(function
  da_ober->match da_ober with
   []->None
   |a::peurrest->match f(a) with
		None->find_and_stop_immediately_after0(peurrest)
		|Some(x)->Some(x)
 ) in
 find_and_stop_immediately_after0(l);;
   
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
  find_and_stop_immediately_after(fun (i,rgxp)->
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

let indices_in_string s=
  My_str.find_all_occurrences My_str_example.moodle_cases s 1;;

let names_in_string z=
  let temp1=indices_in_string z in
  let temp2=Image.image (fun (_,(a,b))->String.sub z (a-1) (b-a+1) ) temp1 in
  let temp3=Three_parts.generic temp2 in
  let temp4=List.filter (fun (x,y,z)->not(List.mem y x)) temp3 in
  let temp5=Image.image (fun (x,y,z)->Naked_module.of_string (String.uncapitalize y)) temp4 in
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

end;;






module Read_info_on_file_in_system=struct


(* 


Recompute the characteristics of a module
stored in memory.

#use"Makefile_makers/new_modulesystem_read_info_on_file.ml";;


*)


let find_needed_data_for_file fs fn=
   let temp1=Look_for_module_names.names_in_file fn in
   let selecter=(fun info->
     let hm=Modulesystem_data.name info in
     let name=Half_dressed_module.undress hm in
     if List.mem name temp1
     then Some(info)
     else None
   ) in
   Option.filter_and_unpack selecter (Modulesystem.all_filedata fs);;

let find_needed_data fs mlx=
   let fn=Mlx_filename.to_path mlx in
   find_needed_data_for_file fs fn;;

let find_needed_names fs mlx=
   let temp1=find_needed_data fs mlx in
   Image.image Modulesystem_data.name temp1;;

 let find_needed_libraries fs mlx genealogy=
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

 let find_needed_directories fs mlx genealogy=
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
 
 
 let complete_info fs mlx opt_inac=
   let (hm,edg)=Mlx_filename.decompose mlx in
   let genealogy=find_needed_data fs mlx in
   let (mlp,mlip,mllp,mlyp)=Modulesystem.check_presences fs hm
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
   let allanc=Option.filter_and_unpack tempf (Modulesystem.all_filedata fs) in
   let libned=find_needed_libraries fs mlx genealogy
   and dirned=find_needed_directories fs mlx genealogy in
   let inac=(function Some(i)->i |None->0)(opt_inac) in
   Modulesystem_data.make
   (hm,mlp,mlip,mllp,mlyp,mlmt,mlimt,mllmt,mlymt,libned,dirfath,allanc,dirned,inac);;
   
let recompute_complete_info_for_module fs hm=
  let opt=Modulesystem.find_module_registration fs hm in
  let dt=Option.unpack opt in
  let opt_inac=Some(Modulesystem_data.inactivity_count dt) in
  let edg=List.hd(Modulesystem_data.registered_endings dt) in
  let mlx=Mlx_filename.join hm edg in
  complete_info fs mlx opt_inac;;
    


end;;






module Self_update_modulesystem=struct


(* 


#use"Makefile_makers/self_update_modulesystem.ml";;


*)

let message_about_circular_dependencies printer l cycles= 
  if cycles=[]
  then ""
  else
  let temp1=Image.image(fun cycle->
    let ttemp1=Image.image (fun j->printer (List.nth l (j-1))) cycle in
     String.concat " -> " ttemp1 
  ) cycles in
  let temp2=String.concat "\n\n" temp1 in
  temp2;;

exception Circular_dependencies of string;;

let treat_circular_dependencies tolerate_cycles printer l cycles=
  if cycles=[]
  then ()
  else let msg=message_about_circular_dependencies printer l cycles in  
       if tolerate_cycles
       then (print_string msg;flush stdout)     
       else raise(Circular_dependencies(msg));; 
 
let self_update_modulesystem tolerate_cycles fs =
  let output=ref"" and very_dirty_ones_ref=ref[] in
  let write=(fun x->output:=(!output)^x^"\n") in
  let temp1=Modulesystem.all_filedata fs in
  let indexed_temp1=Ennig.index_everything temp1 in
  let force_recompute_data=Memoized.make(fun j->
     let dt=List.nth temp1 (j-1) in
     let hm=Modulesystem_data.name dt in
     let recomputed_dt=
     Read_info_on_file_in_system.recompute_complete_info_for_module fs hm  in
     recomputed_dt
  ) in
  let recompute_data_if_necessary=Memoized.make(fun j->
    let dt=List.nth temp1 (j-1) in
    let ac=Modulesystem_data.outdated_acolytes dt in
    if ac<>[]
    then let hm=Modulesystem_data.name dt in
         let _=(very_dirty_ones_ref:=(!very_dirty_ones_ref)@ac) in
         let s_hm=Half_dressed_module.to_string(hm) in
         let _=write ("Checking "^s_hm^" (because it has been overwritten)") in
         force_recompute_data j
    else dt
  ) in
  let coatoms=Memoized.make(fun j->
    let ttemp1=recompute_data_if_necessary j in
    let ttemp2=Modulesystem_data.direct_fathers ttemp1 in
    Option.filter_and_unpack(fun hm->
       match Option.find_it(fun (j,dt)->Modulesystem_data.name dt=hm)(indexed_temp1)
       with
       None->None
       |Some(j0,_)->Some(j0)
    ) ttemp2
 ) in
 let n=List.length temp1 in
 let (cycles,reconstruction)=Reconstruct_linear_poset.reconstruct_linear_poset
   coatoms (Ennig.ennig 1 n) in
 let _=treat_circular_dependencies tolerate_cycles
       (fun fd->Half_dressed_module.to_string(Modulesystem_data.name fd)) temp1 cycles in  
 if (!(very_dirty_ones_ref)=[])
 then (fs,[])
 else       
 let good_list=Image.image fst reconstruction in
 let good_list2=Image.image (fun j->(j,List.nth temp1 (j-1) )) good_list in
 let dirty_ones_ref=ref(!very_dirty_ones_ref) in
 let recompute_data_if_necessary2=(fun (j,dt)->
    let hm=Modulesystem_data.name dt 
    and fathers=Modulesystem_data.direct_fathers dt in
    if List.exists (fun mlx->Mlx_filename.half_dressed_core mlx=hm) (!dirty_ones_ref) 
    then recompute_data_if_necessary j
    else
    match Option.find_it(fun t->List.mem (Mlx_filename.half_dressed_core t) fathers) (!dirty_ones_ref) with
    None->dt
    |Some(mlx)->
          let hm2=Mlx_filename.half_dressed_core mlx in
          let ac=Modulesystem_data.acolytes dt in
          let _=(dirty_ones_ref:=(!dirty_ones_ref)@ac) in
         let recomputed_dt=Read_info_on_file_in_system.recompute_complete_info_for_module fs hm
         and s_hm=Half_dressed_module.to_string(hm) 
         and s_hm2=Half_dressed_module.to_string(hm2)  in
         let _=write ("Checking "^s_hm^" (because of "^s_hm2^")") in
         recomputed_dt
  ) in
 let new_list=Image.image recompute_data_if_necessary2 good_list2 in
 let dirty_modules=Tidel.diforchan(Image.image Mlx_filename.half_dressed_core (!dirty_ones_ref)) in
 let ancestors_for_dirty_ones=Tidel.image (fun hm->
    let fd=Option.find_it_really (fun x->Modulesystem_data.name x=hm) new_list in
    Tidel.diforchan(Modulesystem_data.all_ancestors(fd)) 
 ) dirty_modules in
 let all_ancestors_for_dirty_ones=Tidel.big_teuzin (
   dirty_modules::ancestors_for_dirty_ones
 ) in
 let update_for_activity_count=(fun fd->
   let hm=Modulesystem_data.name fd in
   if Tidel.elfenn hm all_ancestors_for_dirty_ones
   then Modulesystem_data.increment_inactivity_count fd
   else fd
 ) in
 let better_list=Image.image update_for_activity_count new_list in
 let _=(if (!output)<>"" then print_string("\n"^(!output)^"\n");flush stdout) in
 (Modulesystem.make(Modulesystem.root fs,better_list,
  Modulesystem.main_toplevel_name fs),(!dirty_ones_ref));;  
 

  
    


end;;






module Abstract_renamer=struct


(* 


#use"Makefile_makers/isidore_abstract_renamer.ml";;

Abstract type to handle renaming of an individual
file in a file system.

The object of type Modulesystem.t is converted
to the abstract version just before renaming,
and unconverted just after.

*)

type data={
  name : Half_dressed_module.t option;
  ml_present : bool;
  mli_present : bool;
  mll_present : bool;
  mly_present : bool;
  ml_modification_time : float;
  mli_modification_time : float;
  mll_modification_time : float;
  mly_modification_time : float;
  needed_libraries : Ocaml_library.t list;
  direct_fathers : (Half_dressed_module.t option) list;
  all_ancestors : (Half_dressed_module.t option) list;
  needed_directories : Subdirectory.t list;
  inactivity_count : int
   
};;

let abstractify hm x=
  let abstracter=(fun hm2->
  if hm=hm2 then None else Some(hm2)
  ) in
  
  {
  name = abstracter(Modulesystem_data.name x);
  ml_present = Modulesystem_data.ml_present x;
  mli_present = Modulesystem_data.mli_present x;
  mll_present = Modulesystem_data.mll_present x;
  mly_present = Modulesystem_data.mly_present x;
  ml_modification_time = Modulesystem_data.ml_modification_time x;
  mli_modification_time = Modulesystem_data.mli_modification_time x;
  mll_modification_time = Modulesystem_data.mll_modification_time x;
  mly_modification_time = Modulesystem_data.mly_modification_time x;
  needed_libraries = Modulesystem_data.needed_libraries x;
  direct_fathers =Image.image abstracter (Modulesystem_data.direct_fathers x);
  all_ancestors =Image.image abstracter (Modulesystem_data.all_ancestors x);
  needed_directories = Modulesystem_data.needed_directories x;
  inactivity_count = Modulesystem_data.inactivity_count x 
};;

let unabstractify hm x=
  let unabstracter=(fun opt->match opt with
  None->hm
  |Some(hm2)->hm2
  ) in
  
  Modulesystem_data.make(
  unabstracter(x.name),
  x.ml_present,
  x.mli_present, 
  x.mll_present, 
  x.mly_present, 
  x.ml_modification_time, 
  x.mli_modification_time, 
  x.mll_modification_time, 
  x.mly_modification_time, 
  x.needed_libraries, 
  Image.image unabstracter (x.direct_fathers),
  Image.image unabstracter (x.all_ancestors),
  x.needed_directories,
  x.inactivity_count
 );;






 

end;;






module Rename_file_in_system=struct


(* 


#use"Makefile_makers/isidore_rename_file_in_system.ml";;

We separate renaming from relocating. The latter changes
directories while the former changes only the name, staying in the
same directory. 



*)



exception Nonregistered_module of Half_dressed_module.t;;  


let rename fs old_name s_new_name=
   let interm_list=Image.image
   (Abstract_renamer.abstractify old_name) (Modulesystem.all_filedata fs) in
   let opt=Modulesystem.find_module_registration fs old_name in
   if opt=None
   then raise(Nonregistered_module(old_name))
   else 
   let old_dt=Option.unpack opt in
   let old_acolytes=Modulesystem_data.acolytes old_dt in
   let temp7=Image.image (fun mlx->Mlx_filename.do_file_renaming mlx s_new_name) 
   old_acolytes in
   let new_name=Mlx_filename.half_dressed_core(List.hd temp7) in
   let old_mname=Half_dressed_module.module_name old_name
   and new_mname=Half_dressed_module.module_name new_name
   in
   let changer=Look_for_module_names.change_module_name_in_file
   old_mname new_mname in
   let desc=Modulesystem.descendants fs [old_name] in
   let temp1=Image.image Modulesystem_data.acolytes desc in
   let temp2=Listennou.hard_big_concat temp1 in
   let temp3=Image.image Mlx_filename.to_path temp2 in
   let _=Image.image changer temp3 in
   let cmd2=Shell_command.usual ("rm -f "^(Half_dressed_module.to_string old_name)^".cm* ") in
   let cmd=Shell_command.take_care_of_root_directory fs [cmd2] in
   let _=Image.image Shell_command.announce_and_do cmd in
   let new_list=Image.image
   (Abstract_renamer.unabstractify new_name) interm_list in
   Modulesystem.make(
   		Modulesystem.root fs,
        new_list, 
   		Modulesystem.main_toplevel_name fs
   	);;
 
 
 


 

end;;






module Relocate_file_in_system=struct


(* 


#use"Makefile_makers/isidore_relocate_file_in_system.ml";;

We separate renaming from relocating. The latter changes
directories while the former changes only the name, staying in the
same directory. 

Speech follows action : file displacement takes place first, 
then the Ocaml data values are updated.

*)


exception NonoptDependingOnOpt of Half_dressed_module.t*(Half_dressed_module.t list);;  
exception Nonregistered_module of Half_dressed_module.t;;  
 
let relocate_module fs old_name new_subdir=
   let (before,opt,after)=Three_parts.select_center_element  (fun dt->
      Modulesystem_data.name dt=old_name) (Modulesystem.all_filedata fs) in
   if opt=None
   then raise(Nonregistered_module(old_name))
   else 
   let old_dt=Option.unpack opt in
   let old_acolytes=Modulesystem_data.acolytes old_dt in
   let temp5=Image.image (fun mlx->Mlx_filename.do_file_displacing mlx new_subdir) old_acolytes in
   let new_name=Mlx_filename.half_dressed_core(List.hd temp5) in
   let desc=Modulesystem.descendants fs [old_name] in
   let data_renamer=Modulesystem_data.rename (old_name,new_name) in
   let bowls=(	Half_dressed_module.is_optional old_name,
				Half_dressed_module.is_optional new_name) in
   let cmd2=Shell_command.usual ("rm -f "^(Half_dressed_module.to_string old_name)^".cm* ") in
   let cmd=Shell_command.take_care_of_root_directory fs [cmd2] in
   let _=Image.image Shell_command.announce_and_do cmd in
   if bowls=(false,true)
   then 
        let mandatory_desc=List.filter Modulesystem_data.is_not_optional desc in
        if mandatory_desc<>[]
        then let temp1=Image.image Modulesystem_data.name mandatory_desc in
             raise(NonoptDependingOnOpt(new_name,temp1))
        else 
        let (before2,after2)=Modulesystem.optionality_partition after in
        let part1=before@before2
        and part2=Image.image data_renamer (old_dt::after2) in
        Modulesystem.make(Modulesystem.root fs,part1@part2,Modulesystem.main_toplevel_name fs)
   else
   if bowls=(true,false)
   then let (before1,after1)=Modulesystem.optionality_partition before in
        let part2=Image.image data_renamer (old_dt::after1@after) in
        Modulesystem.make(Modulesystem.root fs,before1@part2,Modulesystem.main_toplevel_name fs)
   else let part2=Image.image data_renamer (old_dt::after) in
        Modulesystem.make(Modulesystem.root fs,before@part2,Modulesystem.main_toplevel_name fs);;

exception Already_optional of Half_dressed_module.t;;

let make_module_optional fs old_name=
    let s_old_name=Half_dressed_module.to_string(old_name) in
    if Substring.begins_with s_old_name "Optional/"
    then raise(Already_optional(old_name))
    else 
    relocate_module fs old_name (Subdirectory.of_string "Optional");;    


end;;






module Arrange_positions_in_modulesystem=struct


(* 

There are two main rules :

1) The ancestors of a file x always come before x.
2) Non-optional files always come before optional files.

We assume that the first file is not optional, if the file system is not empty.

#use"Makefile_makers/arrange_positions_in_modulesystem.ml";;


*)


let reserved_item_for_beginning_of_optional_files=None;;

let index_necessarily_before_optional_file fs=
   let l1=Ennig.index_everything (Modulesystem.all_filedata fs) in
   match Option.find_it(fun (j,info)->Modulesystem_data.is_optional info
   ) l1 with
   None->Some(List.length(l1),reserved_item_for_beginning_of_optional_files)
   |Some(j1,_)->Some(j1-1,reserved_item_for_beginning_of_optional_files);;

let index_necessarily_after_nonoptional_file fs=
   let l1=Ennig.index_everything (Modulesystem.all_filedata fs) in
   match Option.find_it(fun (j,info)->Modulesystem_data.is_optional info
   ) l1 with
   None->None
   |Some(j1,_)->Some(j1,reserved_item_for_beginning_of_optional_files);;

let formalize_other_bounds fs l=
  let l1=Ennig.index_everything (Modulesystem.all_filedata fs) in
  let localize=(fun anv->
    match Option.find_it (fun (j,info)->(Modulesystem_data.name info)=anv) l1 with
    None->None
    |Some(j1,_)->Some(j1,Some(anv))
  ) in
  Image.image localize l;;

let lower_bound fs x l_other_bounds_before=
  let name_of_x=Modulesystem_data.name x in
  let l1=Ennig.index_everything (Modulesystem.all_filedata fs) in
  let temp1=(fun l->
     if l=[] then None else
     let last_parent_name=List.hd(List.rev l) in
     let j=fst(Option.find_it_really (fun (j,info)->
     (Modulesystem_data.name info)=last_parent_name) l1) in
     Some(j,Some(last_parent_name))
  )(Modulesystem_data.direct_fathers x) in
  let temp2=(fun bowl->
     if bowl
     then index_necessarily_before_optional_file fs
     else None
  )(Half_dressed_module.is_optional name_of_x ) in
 let temp3=temp1::temp2::(formalize_other_bounds fs l_other_bounds_before) in
 let temp4=Max.maximize_it_if_possible(fst)(temp3) in
 Option.propagate fst temp4;;
  
  
let upper_bound fs x l_other_bounds_after=
  let name_of_x=Modulesystem_data.name x in
  let l1=Ennig.index_everything (Modulesystem.all_filedata fs) in
  let temp0=Option.find_it (fun (j,info)->List.mem name_of_x (Modulesystem_data.all_ancestors info)) l1 in
  let temp1=(function 
     None->None
     |Some(j1,data1)->Some(j1,Some(Modulesystem_data.name data1))
  )(temp0) in
  let temp2=(fun bowl->
     if bowl
     then None
     else index_necessarily_after_nonoptional_file fs
  )(Half_dressed_module.is_optional name_of_x ) in
   let temp3=temp1::temp2::(formalize_other_bounds fs l_other_bounds_after) in
   let temp4=Min.minimize_it_if_possible(fst)(temp3) in
   Option.propagate fst temp4;;
  
 exception NewNonoptDependingOnOldOpt of Half_dressed_module.t*Half_dressed_module.t;;
 exception OldNonoptDependingOnNewOpt of Half_dressed_module.t*(Half_dressed_module.t option);;
 exception Conflict of Half_dressed_module.t*Half_dressed_module.t;;
  
  
 let treat_insertion_error dt (_,data_down) (_,data_up)=
    if data_down=reserved_item_for_beginning_of_optional_files
    then raise(OldNonoptDependingOnNewOpt(Modulesystem_data.name dt,data_up))
    else
    if data_up=reserved_item_for_beginning_of_optional_files
    then raise(NewNonoptDependingOnOldOpt(Option.unpack data_down,Modulesystem_data.name dt))
    else raise(Conflict(Option.unpack data_down,Option.unpack data_up));;
  
 let insertion_index fs dt lower_bound upper_bound=
    if upper_bound=None
    then List.length(Modulesystem.all_filedata fs)
    else let (j_up,data_up)=Option.unpack(upper_bound) in
         if lower_bound=None
         then (* here we use the fact that the first file is not optional, 
              if the file system is not empty *)
              j_up-1
         else let (j_down,data_down)=Option.unpack(lower_bound) in
              if (j_down>j_up)
              then treat_insertion_error dt (j_down,data_down) (j_up,data_up)
              else j_up-1;;

 let insert_data fs x (l_other_bounds_before,l_other_bounds_after)=
   let lower_bound=lower_bound fs x l_other_bounds_before
   and upper_bound=upper_bound fs x l_other_bounds_after
   in
   let i=insertion_index fs x lower_bound upper_bound
   and l1=Ennig.index_everything (Modulesystem.all_filedata fs) in
   let (temp1,temp2)=List.partition (fun (j,t)->j<=i) l1 in
   let temp3=Image.image snd temp1
   and temp4=Image.image snd temp2 in
    Modulesystem.make (
    Modulesystem.root fs,
    temp3@(x::temp4),
    Modulesystem.main_toplevel_name fs);;
   



end;;






module Modify_modulesystem=struct


(* 


#use"Makefile_makers/modify_modulesystem.ml";;


*)


 
 exception Non_registered_file of Mlx_filename.t;;  
 exception Non_registered_module of Half_dressed_module.t;;  
 exception Abandoned_children of Mlx_filename.t*(Half_dressed_module.t list);;
 exception Derelict_children of Half_dressed_module.t*(Half_dressed_module.t list);;  
   
 let reset_inactivity_counts fs=
    let temp1=Image.image Modulesystem_data.reset_inactivity_count (Modulesystem.all_filedata fs) in 
    Modulesystem.make(
       Modulesystem.root fs,
       temp1,
       Modulesystem.main_toplevel_name fs
    );;  
   
 let unregister_mlx_file_and_keep_old_data fs mlxfile=
    let hm=Mlx_filename.half_dressed_core mlxfile in
    let desc=Modulesystem.descendants fs [hm] in
    if desc<>[]
    then let temp1=Image.image Modulesystem_data.name desc in
         raise(Abandoned_children(mlxfile,temp1))
    else
    let (before,opt,after)=Three_parts.select_center_element  (fun dt->
      Modulesystem_data.name dt=hm) (Modulesystem.all_filedata fs) in
    match opt with
     None->raise(Non_registered_file(mlxfile))
    |Some(dt)->
      let edg=Mlx_filename.ending mlxfile in
      if (not(Modulesystem_data.check_presence edg dt))
      then raise(Non_registered_file(mlxfile))
      else 
      let new_dt=Modulesystem_data.make_absence edg dt in
      if (Modulesystem_data.registered_endings new_dt)=[]
      then (dt,Modulesystem.make(Modulesystem.root fs,before@after,Modulesystem.main_toplevel_name fs ))
      else (dt,Modulesystem.make(Modulesystem.root fs,before@(new_dt::after),Modulesystem.main_toplevel_name fs ));;
    
   
 let unregister_mlx_file fs mlxfile=snd(unregister_mlx_file_and_keep_old_data fs mlxfile);;
   
 
 let unregister_module fs hm=
    let desc=Modulesystem.descendants fs [hm] in
    if desc<>[]
    then let temp1=Image.image Modulesystem_data.name desc in
         raise(Derelict_children(hm,temp1))
    else
    let (before,opt,after)=Three_parts.select_center_element  (fun dt->
      Modulesystem_data.name dt=hm) (Modulesystem.all_filedata fs) in
    if opt=None 
    then raise(Non_registered_module(hm))  
    else
    Modulesystem.make(Modulesystem.root fs,
    	before@after,Modulesystem.main_toplevel_name fs );;
    
   
  
  exception Already_registered_file of Mlx_filename.t;;  
  exception Overcrowding of Mlx_filename.t*(Ocaml_ending.t list);;
  exception Bad_pair of Mlx_filename.t*Ocaml_ending.t;; 
  exception Name_conflict of Half_dressed_module.t * Half_dressed_module.t;; 
   
 
 let register_mlx_file_without_treating_inactivity fs mlx_file =
   let hm=Mlx_filename.half_dressed_core mlx_file
   and ending=Mlx_filename.ending mlx_file in 
   let nm=Half_dressed_module.undress hm in
   let all_data=Modulesystem.all_filedata fs in
   let (before,opt,after)=Three_parts.select_center_element  (fun dt->
      Half_dressed_module.undress(Modulesystem_data.name dt)=nm) 
      all_data in
   if opt=None
   then  let old_info=Read_info_on_file_in_system.complete_info fs mlx_file None in
         let info1=Modulesystem_data.make_presence ending old_info in
         (*
         if a mll or mly file is being registered, the ml will automatically be created,
         so let us anticipate by already adding a ml presence
         *)
         let info=(if List.mem ending [Ocaml_ending.mll;Ocaml_ending.mly]
         then Modulesystem_data.make_ml_present info1 else info1) in
         (Modulesystem.make(
         	Modulesystem.root fs,
         	before@[info],
         	Modulesystem.main_toplevel_name fs),info)  
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
   let dt1=Read_info_on_file_in_system.complete_info fs mlx_file None in
   let new_dt=Modulesystem_data.make_presence ending dt1 in
   if ending<>Ocaml_ending.ml
   then let final_list=before@(new_dt::after) in
         (Modulesystem.make(
         Modulesystem.root fs,final_list,Modulesystem.main_toplevel_name fs),
          new_dt)
   else 
   let temp3=List.rev(Modulesystem_data.direct_fathers new_dt) in
   if temp3=[]
   then let final_list=before@(new_dt::after) in
         (Modulesystem.make(
         Modulesystem.root fs,final_list,
         Modulesystem.main_toplevel_name fs),new_dt)
   else  
   let last_father=List.hd(List.rev(Modulesystem_data.direct_fathers new_dt)) in
   let (before1,opt1,after1)=Three_parts.select_center_element  (fun dt->
           (Modulesystem_data.name dt)=last_father) before in
   let lf1=Option.unpack opt1  in    
   let temp2=Image.image (Modulesystem_data.update_anclibdir new_dt all_data) (after1@after) in
   let final_list=before1@(lf1::new_dt::temp2) in
   (Modulesystem.make(
         Modulesystem.root fs,final_list,Modulesystem.main_toplevel_name fs),
   new_dt);;
   
  let register_mlx_file fs mlx_file =
      let (fs2,new_dt)=register_mlx_file_without_treating_inactivity fs mlx_file in
      let dir=Modulesystem.root fs2
      and l=Modulesystem.all_filedata fs2
      and mtn=Modulesystem.main_toplevel_name fs2 
      and hm=Modulesystem_data.name new_dt in
      let all_ancestors=hm::(Modulesystem_data.all_ancestors new_dt) in
      let new_l=Image.image(
        fun dt->
          if List.mem (Modulesystem_data.name dt) all_ancestors
          then dt
          else Modulesystem_data.increment_inactivity_count dt
      ) l in
      Modulesystem.make(dir,new_l,mtn)
      ;;
  
  
  let try_to_register_mlx_file fs mlx_file=
    try(Some(register_mlx_file fs mlx_file)) with _->None;;  

   let try_to_register_mlx_files fs mlx_files=
   let rec tempf=(fun
    (vfs,failures,yet_untreated)->
      match yet_untreated with
      []->(failures,vfs)
      |mlx::others->
      (
        match try_to_register_mlx_file vfs mlx with
        None->tempf(vfs,mlx::failures,others)
        |Some(nfs)->tempf(nfs,failures,others)
      )
   ) in
   tempf(fs,[],mlx_files);;
  
   
  exception NonoptDependingOnOpt of Half_dressed_module.t*(Half_dressed_module.t list);;  
  exception Nonregistered_module of Half_dressed_module.t;;  
  
   
  let reposition fs hm (l_before,l_after)=
   let (before,opt,after)=Three_parts.select_center_element
   (fun dt->
      Modulesystem_data.name dt=hm) (Modulesystem.all_filedata fs) in
   if opt=None
   then raise(Nonregistered_module(hm))
   else 
   let info=Option.unpack opt 
   and amputated_fs=Modulesystem.make 
   (Modulesystem.root fs,before@after,Modulesystem.main_toplevel_name fs) in
   Arrange_positions_in_modulesystem.insert_data amputated_fs info (l_before,l_after);;  

exception Non_existent_mtime of Mlx_filename.t;;

let force_modification_time_update fs mlx=
   let hm=Mlx_filename.half_dressed_core mlx
   and edg=Mlx_filename.ending mlx in
   let (before,opt,after)=Three_parts.select_center_element  (fun dt->
      Modulesystem_data.name dt=hm) 
      (Modulesystem.all_filedata fs) in
   if opt=None
   then raise(Non_existent_mtime(mlx))
   else 
   let dt=Option.unpack opt in
   let dir=Modulesystem.root fs in
   let file=(Directory_name.to_string dir)^(Mlx_filename.to_string mlx) in
   let old_val=Modulesystem_data.modification_time dt edg 
   and new_val=(Unix.stat file).Unix.st_mtime  in
   if old_val=new_val
   then fs
   else let new_dt=Modulesystem_data.force_modification_time dt edg new_val in
        Modulesystem.make(Modulesystem.root fs,before@(new_dt::after),
        Modulesystem.main_toplevel_name fs);;

    


end;;






module Modify_target_system=struct


(* 


#use"Makefile_makers/modify_target_system.ml";;

*)


let unregister_mlx_file ts mlx=
  let old_fs=Target_system.modulesystem ts
  and old_tgts=Target_system.up_to_date_targets ts 
  and hm=Mlx_filename.half_dressed_core mlx in
 let new_filesys=Modify_modulesystem.unregister_mlx_file old_fs mlx in
 let new_dirs=Compute_modulesystem_directories.compute_modulesystem_directories new_filesys 
 and new_tgts=List.filter (fun tgt->
   match Ocaml_target.main_module tgt with
   None->false |Some(hm2)->hm2<>hm
 ) old_tgts in
  Target_system.make new_filesys new_dirs new_tgts;;
  
let unregister_module ts hm=
  let old_fs=Target_system.modulesystem ts
  and old_tgts=Target_system.up_to_date_targets ts in
 let new_filesys=Modify_modulesystem.unregister_module old_fs hm in
 let new_dirs=Compute_modulesystem_directories.compute_modulesystem_directories new_filesys 
 and new_tgts=List.filter (fun tgt->
   match Ocaml_target.main_module tgt with
   None->false |Some(hm2)->hm2<>hm
 ) old_tgts in
  Target_system.make new_filesys new_dirs new_tgts;;  
  
  
 let register_mlx_file ts mlx=
  let old_fs=Target_system.modulesystem ts
  and old_dirs=Target_system.directories ts 
  and old_tgts=Target_system.up_to_date_targets ts 
  and hm=Mlx_filename.half_dressed_core mlx in
  let new_dir=Compute_modulesystem_directories.individual_directory hm in
 let new_filesys=Modify_modulesystem.register_mlx_file old_fs mlx in
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
       let mn=Target_system.main_toplevel_name ts in
       List.filter (
        fun tgt->match Ocaml_target.toplevel_name tgt with
          None->(match Ocaml_target.main_module tgt with
                 None->true
                 |Some(hm2)->hm2<>hm
                )
          |Some(name)->name<>mn
       ) old_tgts
  ) in
  Target_system.make new_filesys new_dirs new_tgts;; 
 
 let reposition ts mlx (l_before,l_after)=
    let old_fs=Target_system.modulesystem ts
    and old_dirs=Target_system.directories ts 
    and old_tgts=Target_system.up_to_date_targets ts in
    let new_fs=Modify_modulesystem.reposition old_fs mlx (l_before,l_after) in
    Target_system.make new_fs old_dirs old_tgts;; 
    
 
let self_update tolerate_cycles ts=
    let old_fs=Target_system.modulesystem ts
    and old_tgts=Target_system.up_to_date_targets ts in
    let (new_fs,dirty_ones)=Self_update_modulesystem.self_update_modulesystem tolerate_cycles old_fs in
	let new_dirs=Compute_modulesystem_directories.compute_modulesystem_directories new_fs 
 	and new_tgts1=Ingredients_for_ocaml_target.still_up_to_date_targets old_fs dirty_ones old_tgts in
 	let dir=Target_system.root ts in
 	let checker=(fun tgt->
 	  let s=(Directory_name.to_string dir)^(Ocaml_target.to_string tgt) in 
 	  Sys.file_exists s ) in
 	let new_tgts=List.filter checker new_tgts1 in
    Target_system.make new_fs new_dirs new_tgts;;   
     
 
let rename_module ts old_hm new_hm=
  let old_fs=Target_system.modulesystem ts
  and old_dirs=Target_system.directories ts 
  and old_tgts=Target_system.up_to_date_targets ts in
  let untouched_targets=List.filter
   (fun tgt->not(Ingredients_for_ocaml_target.module_dependency_for_ocaml_target
   old_fs [old_hm] tgt)) old_tgts in
  let new_fs=Rename_file_in_system.rename old_fs old_hm new_hm in
  let ts1=Target_system.make new_fs old_dirs untouched_targets in
  self_update false ts1;;   
  
let make_module_optional ts old_hm =
  let old_fs=Target_system.modulesystem ts
  and old_tgts=Target_system.up_to_date_targets ts in
 let new_fs=Relocate_file_in_system.make_module_optional old_fs old_hm in
 let new_dirs=Compute_modulesystem_directories.compute_modulesystem_directories new_fs in
 let ts1=Target_system.make new_fs new_dirs old_tgts in
  self_update false ts1;;   
  
let relocate_module ts old_hm new_dir=
    let old_fs=Target_system.modulesystem ts
    and old_tgts=Target_system.up_to_date_targets ts in
    let new_fs=Relocate_file_in_system.relocate_module old_fs old_hm new_dir in
    let new_dirs=Compute_modulesystem_directories.compute_modulesystem_directories new_fs in
    let ts1=Target_system.make new_fs new_dirs old_tgts in
    self_update false ts1;;   
  
 let force_modification_time_update ts mlx= 
    let fs=Target_system.modulesystem ts in
    Target_system.make 
    (Modify_modulesystem.force_modification_time_update fs mlx) 
    (Target_system.directories ts) 
    (Target_system.up_to_date_targets ts);;
  
let reset_inactivity_counts ts=
  let old_fs=Target_system.modulesystem ts
  and old_dirs=Target_system.directories ts 
  and old_tgts=Target_system.up_to_date_targets ts in
  let new_fs=Modify_modulesystem.reset_inactivity_counts(old_fs) in
  Target_system.make new_fs old_dirs old_tgts;;   

  
  

end;;






module Make_ocaml_target=struct

(*

#use"Makefile_makers/make_ocaml_target.ml";;

*)



let cmd_for_tgt=Command_for_ocaml_target.command_for_ocaml_target_in_dir;;
let ingr_for_tgt=Ingredients_for_ocaml_target.ingredients_for_ocaml_target;;
let ingr_for_top=Ingredients_for_ocaml_target.marked_ingredients_for_unprepared_toplevel;;

let is_up_to_date ts tgt=
  let fs=Target_system.modulesystem ts in
  if Ocaml_target.test_path (Modulesystem.root fs) tgt
  then List.mem tgt (Target_system.up_to_date_targets ts)
  else false;;

let unit_make (bowl,ts) tgt=
  if (not bowl)
  then (bowl,ts)
  else
  if is_up_to_date ts tgt
  then (true,ts)
  else 
  let fs=Target_system.modulesystem ts  in
  let temp1=Image.image Shell_command.announce_and_do (cmd_for_tgt fs tgt) in 
  if List.for_all (fun bowl->bowl) temp1
  then  let ts2=
             Target_system.make
              fs 
              (Target_system.directories ts)
              (tgt::(Target_system.up_to_date_targets ts))
              in
        match Ocaml_target.ml_from_lex_or_yacc_data tgt with
       None->(true,ts2)
       |Some(mlx)->
                   let ts3=Modify_target_system.force_modification_time_update ts2 mlx in
                   (true,ts3)        
  else (false,ts);;

let make_nontoplevel ts tgt=
  let l=ingr_for_tgt (Target_system.modulesystem ts) tgt in
  List.fold_left unit_make  (true,ts) l;;
  
let rec iterator_for_toplevel (successful_ones,to_be_treated,ts)=
  match to_be_treated with
  []->(List.rev successful_ones,ts)
  |(tgt,bowl)::others->
  let (bowl2,ts2)=unit_make (true,ts) tgt in
  if bowl2
  then let new_successful_ones=(
         if bowl
         then let hm=Option.unpack(Ocaml_target.main_module tgt) in
               hm::successful_ones
         else successful_ones
       ) in
       iterator_for_toplevel(new_successful_ones,others,ts2)
  else let hm=Option.unpack(Ocaml_target.main_module tgt) in
  	   let remains=List.filter
       (fun (tgt,_)->
         not(Ingredients_for_ocaml_target.module_dependency_for_ocaml_target 
         (Target_system.modulesystem ts) [hm] tgt)
       ) to_be_treated in
       iterator_for_toplevel(successful_ones,remains,ts2);; 
  
 let make_toplevel ts name l=
    let fs=Target_system.modulesystem ts in
    let temp1=ingr_for_top fs name l in
    let (successful_ones,ts2)=iterator_for_toplevel([],temp1,ts) in
    let new_toplevel=Ocaml_target.toplevel name successful_ones  in
    unit_make (true,ts2) new_toplevel;;
 
let make ts tgt=
  match Ocaml_target.toplevel_data tgt with
  None->make_nontoplevel ts tgt
  |Some(name,l)->make_toplevel ts name l;; 
 


end;;






module Compare_two_modulesystem_data=struct


(* 


#use"Makefile_makers/isidore_compare_two_modulesystem_data.ml";;

Here we compare two modulesystem elements corresponding to the same
module name.

*)

 type t = {
      name : Half_dressed_module.t;
      ml_presences : bool*bool;
      mli_presences : bool*bool;
      mll_presences : bool*bool;
      mly_presences : bool*bool;
      ml_modification_time_has_changed : bool;
      mli_modification_time_has_changed : bool;
      mll_modification_time_has_changed : bool;
      mly_modification_time_has_changed : bool;
      changed_needed_libraries : (Ocaml_library.t list)*(Ocaml_library.t list);
      changed_direct_fathers : (Half_dressed_module.t list)*(Half_dressed_module.t list);
      changed_all_ancestors : (Half_dressed_module.t list)*(Half_dressed_module.t list);
      changed_needed_directories : (Subdirectory.t list)*(Subdirectory.t list);
    };;

module FD=Modulesystem_data;;

exception Different_Names of FD.t*FD.t;;

let sd=Simplify_without_orderings.symmetric_decomposition;;

let compare dt1 dt2=
    if (FD.name dt1)<>(FD.name dt2)
    then raise(Different_Names(dt1,dt2))
    else
    {
	  name=FD.name dt1;
      ml_presences=(FD.ml_present dt1,FD.ml_present dt2);
      mli_presences=(FD.mli_present dt1,FD.mli_present dt2);
      mll_presences=(FD.mll_present dt1,FD.mll_present dt2);
      mly_presences=(FD.mly_present dt1,FD.mly_present dt2);
      ml_modification_time_has_changed=((FD.ml_modification_time dt1)<>(FD.ml_modification_time dt2));
      mli_modification_time_has_changed=((FD.mli_modification_time dt1)<>(FD.mli_modification_time dt2));
      mll_modification_time_has_changed=((FD.mll_modification_time dt1)<>(FD.mll_modification_time dt2));
      mly_modification_time_has_changed=((FD.mly_modification_time dt1)<>(FD.mly_modification_time dt2));
      changed_needed_libraries=sd (FD.needed_libraries dt1) (FD.needed_libraries dt2);
      changed_direct_fathers=sd (FD.direct_fathers dt1) (FD.direct_fathers dt2) ;
      changed_all_ancestors=sd (FD.all_ancestors dt1) (FD.all_ancestors dt2);
      changed_needed_directories=sd (FD.needed_directories dt1) (FD.needed_directories dt2);

};;

let uncurried_compare (dt1,dt2)=compare dt1 dt2;;

let tab=String.make 0 ' ';;

let mlx_contribution s=function
   (true,false)->tab^" "^s^" disappeared "
   |(false,true)->tab^" "^s^" appeared "
   |_->"";;
   
let mlx_mt_contribution s (b1,b2) b=if(b1&&b2&&b) then tab^s^" changed" else "";; 


let display_list printer l=
  if l=[] then "" else (String.concat "," (Image.image printer l));;
  
let display_prefixed_list pref printer l=
  if l=[] then "" else pref^(String.concat "," (Image.image printer l));;  
  
let display_list_pair printer (l1,l2)=
  let old_s1=display_list printer l1
  and old_s2=display_list printer l2 in
  let s1=(if old_s1="" then "" else old_s1^" out")
  and s2=(if old_s2="" then "" else old_s2^" in")
  in
  if (s1<>"")&&(s2<>"") then s1^" | "^s2 else s1^s2;;

let display x=
  let temp1=[
   
      mlx_contribution "Ml" x.ml_presences;
      mlx_contribution "Mli" x.mli_presences;
      mlx_contribution "Mll" x.mll_presences;
      mlx_contribution "Mly" x.mly_presences;
      mlx_mt_contribution "Ml"   x.ml_presences  x.ml_modification_time_has_changed;
      mlx_mt_contribution "Mli" x.mli_presences x.mli_modification_time_has_changed;
      mlx_mt_contribution "Mll" x.mll_presences x.mll_modification_time_has_changed;
      mlx_mt_contribution "Mly" x.mly_presences x.mly_modification_time_has_changed;
      display_list_pair  	   Ocaml_library.to_string x.changed_needed_libraries;
      display_prefixed_list " has abandoned direct use of : " Half_dressed_module.to_string (fst(x.changed_direct_fathers));
      display_prefixed_list " now uses directly : " Half_dressed_module.to_string (snd(x.changed_direct_fathers));
      display_prefixed_list " has stopped using : " Half_dressed_module.to_string (fst(x.changed_all_ancestors));
      display_prefixed_list " now uses : " Half_dressed_module.to_string (snd(x.changed_all_ancestors));
      (*
      display_list_pair  Half_dressed_module.to_string x.changed_direct_fathers;
      display_list_pair  Half_dressed_module.to_string x.changed_all_ancestors;
      *)
      display_list_pair  Subdirectory.to_string x.changed_needed_directories; 
  
  ] in
  let temp2=List.filter (fun s->s<>"") temp1 in
  if temp2=[] then "" else
  let s_name=Half_dressed_module.to_string x.name in
  let temp3=Image.image (fun s->if String.contains s ':' then s_name^s else s_name^" : "^s) temp2 in
  String.concat "\n" temp3;;
  
let display_list l=
  let temp1=Image.image display l in
  let temp2=List.filter (fun s->s<>"") temp1 in
  if temp2=[] then "" else
  String.concat "\n" temp2;;   
    
   
   


end;;






module Compare_two_modulesystems=struct


(* 

#use"Makefile_makers/isidore_compare_two_modulesystems.ml";;

*)

 type t = {
      two_roots : Directory_name.t*Directory_name.t;
      two_toplevel_names : string*string;
      personal_modules : (Half_dressed_module.t list)*(Half_dressed_module.t list);
      intramodular_differences : Compare_two_modulesystem_data.t list;
};;

let consider_roots (dir1,dir2)=
  if dir1<>dir2
  then "Root changed from "^(Directory_name.to_string dir1)^" to "^(Directory_name.to_string dir2)
  else "";;

let consider_toplevel_names (s1,s2)=
  if s1<>s2
  then "Toplevel name changed from "^s1^" to "^s2
  else "";;

let consider_modules adjective l=
 if l=[] then "" else 
 adjective^" modules : "^(String.concat "," (Image.image Half_dressed_module.to_string l));;

let compare fs1 fs2=
  let data1=Modulesystem.all_filedata fs1
  and data2=Modulesystem.all_filedata fs2 in
  let (common_core,old_modules,new_modules)=
  	 (Simplify_without_orderings.generic_symmetric_decomposition 
  	   Modulesystem_data.name data1 data2) in
  let temp1=Image.image Compare_two_modulesystem_data.uncurried_compare common_core in
  let temp2=List.filter (fun dt->(Compare_two_modulesystem_data.display dt)<>"") temp1 in	   
  {
      two_roots =(Modulesystem.root fs1,Modulesystem.root fs2);
      two_toplevel_names =(Modulesystem.main_toplevel_name fs1,Modulesystem.main_toplevel_name fs2);
      personal_modules=(old_modules,new_modules);
      intramodular_differences=temp2;
  };;
 
let display x=
  let temp1=[
   
      consider_roots x.two_roots;
      consider_toplevel_names x.two_toplevel_names;
      consider_modules "Old" (fst(x.personal_modules));
      consider_modules "New" (snd(x.personal_modules));
      Compare_two_modulesystem_data.display_list x.intramodular_differences
  
  ] in
  let temp2=List.filter (fun s->s<>"") temp1 in
  if temp2=[] then "" else
  String.concat "\n\n" (temp2@["\n"]);;
  
   
 

end;;






module Compare_two_target_systems=struct


(* 

#use"Makefile_makers/isidore_compare_two_target_systems.ml";;

*)

 type t = {
      file_differences : Compare_two_modulesystems.t;
      personal_targets : (Ocaml_target.t list)*(Ocaml_target.t list);
      personal_directories : (Subdirectory.t list)*(Subdirectory.t list);
};;


let consider_targets adjective l=
 if l=[] then "" else 
 adjective^" targets : "^(String.concat "," (Image.image Ocaml_target.to_string l));;


let consider_directories adjective l=
 if l=[] then "" else 
 adjective^" directories : "^(String.concat "," (Image.image Subdirectory.to_string l));;

let deal_with_targets l1 l2=
  let ttg1=Tidel.safe_set(l1)
  and ttg2=Tidel.safe_set(l2) in
  let temp1=Tidel.lemel ttg1 ttg2
  and temp2=Tidel.lemel ttg2 ttg1 in
  let temp3=List.filter (fun tgt->Tidel.elfenn tgt temp1) l1
  and temp4=List.filter (fun tgt->Tidel.elfenn tgt temp2) l2 in
  (temp3,temp4);;
 

  

let compare ts1 ts2=
  let fs1=Target_system.modulesystem ts1
  and fs2=Target_system.modulesystem ts2 in
  let l1=Target_system.up_to_date_targets ts1
  and l2=Target_system.up_to_date_targets ts2 in
  {
     file_differences=Compare_two_modulesystems.compare fs1 fs2;
      personal_targets=deal_with_targets l1 l2;
      personal_directories=
      	Simplify_without_orderings.symmetric_decomposition
      	(Target_system.directories ts1) (Target_system.directories ts2);
  };;
 
let obsolete_targets_will_be_displayed=ref false;; 

let display x=
  let temp1=[

      (
       if (!obsolete_targets_will_be_displayed)
       then consider_targets "Obsolete" (fst(x.personal_targets))
       else ""
      );
      consider_targets "Newly created" (snd(x.personal_targets));
      consider_directories "Obsolete" (fst(x.personal_directories));
      consider_directories "Newly created" (snd(x.personal_directories));
      Compare_two_modulesystems.display x.file_differences
  
  ] in
  let temp2=List.filter (fun s->s<>"") temp1 in
  if temp2=[] then "" else
  String.concat "\n\n" (temp2@["\n"]);;
  
   

end;;






module Current_date=struct

(*

#use"current_date.ml";;

*)

let current_date ()=
  let module Unix_again=(struct
   include Unix;;
   let day x=x.tm_mday;;
   let month x=x.tm_mon;;
   let year x=x.tm_year;;
  end) in
  let temp=Unix.gmtime(Unix.time()) in
  let year=string_of_int((Unix_again.year temp)+1900)
  and month1=string_of_int((Unix_again.month temp)+1)
  and day1=string_of_int(Unix_again.day temp) in
  let month=Cull_string.resize_from_right month1 2 '0'
  and day=Cull_string.resize_from_right day1 2 '0' in
  year^"_"^month^"_"^day;;



end;;






module Mutable_target_system=struct

(*

#use"Makefile_makers/mutable_target_system.ml";;

*)



type t={
   content : Target_system.t ref;
   location_for_makefile : string;
   location_for_targetfile : string;
   location_for_loadingsfile : string;
   location_for_pervasivesfile : string;
   
};;

let makefile_counter=ref 0;;
let targetfile_counter=ref 0;;
let loadingsfile_counter=ref 0;;
let pervasivesfile_counter=ref 0;;

let make ts opt_makefile opt_targetfile opt_loadingsfile opt_pervasivesfile=
   let makefile_location=(
     match opt_makefile with
     None->let m=(!makefile_counter)+1 in
           (
            makefile_counter:=m;
            "makefile"^(string_of_int(m))
            )
     |Some(s)->s       
   ) and targetfile_location=(
     match opt_targetfile with
     None->let m=(!targetfile_counter)+1 in
           (
            targetfile_counter:=m;
            "targetfile"^(string_of_int(m))
            )
     |Some(s)->s       
   ) and loadingsfile_location=(
     match opt_loadingsfile with
     None->let m=(!loadingsfile_counter)+1 in
           (
            loadingsfile_counter:=m;
            "loadingsfile"^(string_of_int(m))
            )
     |Some(s)->s       
   ) and pervasivesfile_location=(
     match opt_pervasivesfile with
     None->let m=(!pervasivesfile_counter)+1 in
           (
            pervasivesfile_counter:=m;
            "my_pervasives"^(string_of_int(m))^".ml"
            )
     |Some(s)->s       
   ) 
   in

{
   content=ref(ts);
   location_for_makefile=makefile_location;
   location_for_targetfile=targetfile_location;
   location_for_loadingsfile=loadingsfile_location;
   location_for_pervasivesfile=pervasivesfile_location
};;


let target_system mts=(!(mts.content));;
let modulesystem mts=Target_system.modulesystem (!(mts.content));;
let directories mts=Target_system.directories (!(mts.content));;
let up_to_date_targets mts=Target_system.up_to_date_targets (!(mts.content));;
let root mts=Target_system.root (!(mts.content));;
let find_module_registration mts mlx=Target_system.find_module_registration (!(mts.content)) mlx;;
let all_modules mts=Target_system.all_modules (!(mts.content));;
let all_filedata mts=Modulesystem.all_filedata (modulesystem mts);;
let all_mlx_files mts=Target_system.all_mlx_files (!(mts.content));;
let location_for_makefile mts=mts.location_for_makefile;;
let location_for_targetfile mts=mts.location_for_targetfile;;
let location_for_loadingsfile mts=mts.location_for_loadingsfile;;
let location_for_pervasivesfile mts=mts.location_for_pervasivesfile;;

let absolute_location_for_makefile mts=
  Directory_name.force_join (root mts) 
  (mts.location_for_makefile);;

let absolute_location_for_targetfile mts=
  Directory_name.force_join (root mts) 
  (mts.location_for_targetfile);;

let absolute_location_for_loadingsfile mts=
  Directory_name.force_join (root mts) 
  (mts.location_for_loadingsfile);;

let absolute_location_for_pervasivesfile mts=
  Directory_name.force_join (root mts) 
  (mts.location_for_pervasivesfile);;

let from_modulesystem fs opt1 opt2 opt3=
   make (Target_system.from_modulesystem fs) opt1 opt2 opt3;;


let unregistered_mlx_files mts=Target_system.unregistered_mlx_files (!(mts.content));; 


 
let change_content mts ts=(mts.content:=ts);;

let loadings mts=
  let temp1=up_to_date_targets mts in
  let temp2=Option.filter_and_unpack (
    function (Ocaml_target.CMO(x))->
      Some("#load\""^(Half_dressed_module.to_string x)^".cmo\";;") 
    |_->None
  ) temp1 in
  let temp3="\n\n\n"::(List.rev ("\n\n\n"::temp2)) in
  let temp4=String.concat "\n" temp3 in
  temp4;; 
 
let save_makefile mts=
  let cs=modulesystem mts in
  let s1="# This makefile was automatocally written by\n"^
  "# the write_makefile function in the ml_manager module. \n\n"^
  (Write_makefile.write_makefile cs) in
  let lm=absolute_location_for_makefile mts in
  Io.erase_file_and_fill_it_with_string (Absolute_path.of_string lm) s1;;

let save_loadingsfile mts=
   let s=loadings mts
   and beg_m="(* loadings begin here *)"
   and end_m="(* loadings end here *)" in
   Replace_inside.overwrite_between_markers_inside_file (beg_m,end_m)
    (Absolute_path.of_string(absolute_location_for_loadingsfile mts)) s;;

let save_targetfile mts=
  let ts=target_system mts in
  let s1=Target_system.archive ts in
  let lt=absolute_location_for_targetfile mts in
  Io.erase_file_and_fill_it_with_string (Absolute_path.of_string lt) s1;;

let save_all mts=(save_makefile mts;save_loadingsfile mts;save_targetfile mts);;

let discreet_self_update tolerate_cycles mts=
   let ts=target_system mts in
   let ts2=Modify_target_system.self_update tolerate_cycles ts in
   if ts2<>ts
   then let _=(mts.content:=ts2;save_all mts) 
        and d12=Compare_two_target_systems.compare ts ts2 in
        Some(d12)
   else None;;


let unit_discreet_self_update tolerate_cycles mts= 
  let _=  discreet_self_update tolerate_cycles mts in ();;     

let semi_discreet_self_update tolerate_cycles mts=
   match discreet_self_update tolerate_cycles mts with
   (Some d12)-> print_string(Compare_two_target_systems.display d12)
   |None->();;

let self_update tolerate_cycles mts=
   match discreet_self_update tolerate_cycles mts with
   (Some d12)-> print_string(Compare_two_target_systems.display d12)
   |None->(print_string("Nothing new in the source files. \n");flush stdout);;

let machen mts tgt=
   let ts=target_system mts in
   let ts2=snd(Make_ocaml_target.make ts tgt) in
   (mts.content:=ts2;save_all mts);;

let usual_toplevel mts=
  Ocaml_target.toplevel_from_modulesystem (modulesystem mts);;


let force_recompile mts=
  (
   machen mts (usual_toplevel mts);
   save_all mts
   );;

let recompile mts=
  let opt=discreet_self_update false mts in
  if opt=None
  then ()
  else force_recompile mts;;

let compile_acyclic_part mts=
  let opt=discreet_self_update true mts in
  if opt=None
  then ()
  else force_recompile mts;;
  
let reset_inactivity_counts mts=
   let ts=(!(mts.content)) in
   (
   mts.content:=(Modify_target_system.reset_inactivity_counts ts);
   save_all mts
   );;
  
let register mts mlx=
   let ts=target_system mts in
   let ts2=Modify_target_system.register_mlx_file ts mlx in
   (mts.content:=ts2;force_recompile mts);;


let unregister mts mlx=
   let ts=target_system mts in
   let ts2=Modify_target_system.unregister_mlx_file ts mlx in
   (mts.content:=ts2;force_recompile mts);;

let unregister_module mts hm=
   let ts=target_system mts in
   let ts2=Modify_target_system.unregister_module ts hm in
   (mts.content:=ts2;force_recompile mts);;


let unregistered_mlx_files mts=
  let ts=target_system mts in
  Target_system.unregistered_mlx_files ts;;  

let reposition mts mlx (l_before,l_after)=
   let ts=target_system mts in
   let ts2=Modify_target_system.reposition ts mlx (l_before,l_after) in
   (mts.content:=ts2;recompile mts);;

let rename_module mts name_before name_after=
   let _=unit_discreet_self_update false mts in
   let ts=target_system mts in
   let ts2=Modify_target_system.rename_module ts name_before name_after in
   (mts.content:=ts2;force_recompile mts);;

let relocate_module mts hm new_dir=
   let _=unit_discreet_self_update false mts in
   let ts=target_system mts in
   let ts2=Modify_target_system.relocate_module ts hm new_dir in
   (mts.content:=ts2;force_recompile mts);;

let make_module_optional mts hm=
   let _=unit_discreet_self_update false mts in
   let ts=target_system mts in
   let ts2=Modify_target_system.make_module_optional ts hm in
   (mts.content:=ts2;force_recompile mts);;

exception Non_registered_module of Half_dressed_module.t;;

let above mts hm=Modulesystem.above (modulesystem mts) hm;;
let below mts hm=Modulesystem.below (modulesystem mts) hm;;
let directly_below mts hm=Modulesystem.directly_below (modulesystem mts) hm;;   

    
let forget_unregistered_file mts ap=
   let s_dir=Directory_name.to_string(root mts) in
   let n_dir=String.length s_dir in
   let s_ap=Absolute_path.to_string ap in
   let subpath=Cull_string.cobeginning n_dir s_ap in
   let new_subpath=(Current_date.current_date())^"_"^
         (Replace_inside.replace_inside_string subpath ("/","_dir_") ) in
   let _=Sys.command ("mkdir -p "^s_dir^"Forgotten") in
   let _=Sys.command ("touch "^s_dir^"Forgotten/"^new_subpath) in
   let cmd="mv "^s_ap^" "^s_dir^"Forgotten/"^new_subpath in
   let _=Sys.command cmd in 
   ();;


exception FileWithDependencies of 
	Mlx_filename.t*(Half_dressed_module.t list);;


let forget mts ap=
  let hm=Half_dressed_module.of_path_and_root ap (root mts) 
  and mlx=Mlx_filename.of_path_and_root ap (root mts)  in
  match Modulesystem.find_module_registration (modulesystem mts) hm with
   None->forget_unregistered_file mts ap
  |Some(_)->
   let bel=below mts (Mlx_filename.half_dressed_core mlx) in
    if bel=[]
    then (unregister mts mlx;forget_unregistered_file mts ap)
    else raise(FileWithDependencies(mlx,bel));;

let unregistered_ml_files mts=
   let temp1=Mlx_filename.complete_ls (root mts) in
   let temp2=List.filter (fun mlx->
   if (Mlx_filename.ending mlx)=Ocaml_ending.ml
   then not(Modulesystem.see_if_file_is_registered (modulesystem mts) mlx)
   else false) temp1 in
   temp2;;
   
let system_size x=Target_system.system_size(!(x.content));; 

let inactivity_report mts=Modulesystem.inactivity_report (modulesystem mts);;

end;;






module Create_target_system=struct


(* 

#use"Makefile_makers/isidore_create_modulesystem.ml";;


*)

let display_circular_dependencies printer l cycles= 
  if cycles=[]
  then ()
  else
  let temp1=Image.image(fun cycle->
    let ttemp1=Image.image (fun j->printer (List.nth l (j-1))) cycle in
     String.concat " -> " ttemp1 
  ) cycles in
  let temp2=String.concat "\n\n" temp1 in
  (print_string temp2;flush stdout);;
 

let select_good_files s_main_dir=
   let ap1=Absolute_path.of_string s_main_dir in
   let temp1=More_unix.complete_ls ap1 
   and n1=String.length(Absolute_path.to_string ap1) in
   let selector=(
   fun ap->
     let s=Absolute_path.to_string ap in
     let t=Cull_string.cobeginning n1 s in
     (List.exists (fun edg->Substring.ends_with s edg) [".ml";".mli";".mll";".mly"])
     &&
     (List.for_all (fun beg->not(Substring.begins_with t beg)) ["Remembered/";"Forgotten/"])
     &&
     (List.for_all (fun edg->not(Substring.ends_with s edg) ) 
     ["neptu";"my_loadings.ml";"my_pervasives.ml";"ocamlinit"])
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
     raise an exception if there are different modules with
     identical names.
     Remove the files outside main_dir.
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
    Listennou.hard_big_concat  ttemp3
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
  
let from_prepared_list dir opt l=
   let temp1=Option.filter_and_unpack (fun (ap,s)->
      Mlx_filename.try_from_path_and_root ap dir
   ) l in
   let fs=Modulesystem.from_root_and_toplevel dir opt in
   Modify_modulesystem.try_to_register_mlx_files fs temp1;;

let from_main_directory dir=
	let old_s=Directory_name.to_string(dir) in
	let s1=Cull_string.coending 1 old_s in (* mind the trailing slash *)
	let temp1=select_good_files s1 in
    let temp2=clean_list_of_files dir temp1 in
    let temp3=compute_dependencies temp2 in
   let (failures,fs1)=from_prepared_list dir (Some"ecaml") temp3 in
   let ts1=Target_system.from_modulesystem fs1 in
   let tl1=Ocaml_target.toplevel_from_modulesystem fs1 in
   Make_ocaml_target.make ts1 tl1 ;;



end;;






module Implement_modulesystem=struct

(*

#use"Makefile_makers/implement_modulesystem.ml";;

*)

type directory_to_be=string;;

let print_message s=(print_string (s^"\n");flush stdout);;

let implement fs (root_to_be:directory_to_be)=
  let s_old_root=Directory_name.to_string(Modulesystem.root fs) in
  let _=print_message "Creating new root directory ..." in
  let cmd1="mkdir -p "^root_to_be in
  let _=Sys.command cmd1 in
  let _=print_message "New root directory exists now." in
  let new_root_dir=Directory_name.of_string root_to_be in
  let s_new_root=Directory_name.to_string new_root_dir in
  let simple_copy=(fun fn->
    let cmd="cp "^s_old_root^"/"^fn^" "^s_new_root^"/"^fn  in
    Sys.command cmd
  )  in
  let careful_copy=(fun fn->
     let _=simple_copy fn in
     let new_path=Absolute_path.of_string(s_new_root^"/"^fn) in
     Replace_inside.replace_inside_file
       new_path (s_old_root,s_new_root)
  ) in
  let temp1=Modulesystem.all_mlx_files fs in
  let temp2=Image.image Mlx_filename.to_string temp1 in
  let temp3="Remembered"::"Forgotten"::(Modulesystem.local_directories fs) in
  let _=print_message "Copying files ..." in
  let _=Image.image (fun sdir->
    let cmd="mkdir -p "^s_new_root^"/"^sdir in
    Sys.command cmd
  ) temp3 in
  let _=Explicit.image simple_copy temp2 in
  let _=List.iter careful_copy 
   [".ocamlinit";"Remembered/aztec.ml";
    "my_loadings.ml";"my_pervasives.ml";
    "current_root_directory.ml"] in
  let _=print_message "Files copied." in
  ();;








   
   
  

end;;






module Values_in_modules=struct


(* 

#use"Makefile_makers/values_in_modules.ml";;

*)


let rename_value fs old_name new_name=
  let temp1=Modulesystem.files_containing_string fs old_name in
  let m=String.length(Directory_name.to_string(Modulesystem.root fs)) in
  let temp2=Image.image (fun ap->
    Cull_string.cobeginning m (Absolute_path.to_string ap)) temp1 in
  let message="\n\n The following files will be rewritten : \n\n"^
  (String.concat "\n" temp2) in
  let _=(print_string message;flush stdout) in
  List.iter (fun ap->
  Replace_inside.replace_inside_file ap (old_name,new_name)) temp1;;

let list_values_from_module_in_file module_name file=
   let s=Io.read_whole_file file in
   let temp1=Look_for_module_names.indices_in_file file in
   let temp2=List.filter (fun (t,(i,j))->
     (t=My_str_example.index_for_pointed_case)&&
     (Cull_string.interval s i j=(String.capitalize module_name))
   ) temp1 in
   let temp3=Image.image(fun (t,(i,j))->
    Charset.starry_from
     Charset.strictly_alphanumeric_characters
     s (j+2)
   ) temp2 in
   Ordered_string.diforchan temp3;;

let list_values_from_module_in_modulesystem module_name fs=
   let temp1=Modulesystem.all_mlx_paths fs in
   let temp2=Image.image (fun ap->
    let ttemp1=list_values_from_module_in_file module_name ap in
    Ordered_string.image (fun x->(x,ap) ) ttemp1
    ) temp1 in
   let temp3=Listennou.hard_big_concat temp2 in
   let temp4=Image.image fst temp3 in 
   let temp5=Ordered_string.diforchan temp4 in
   let temp6=Ordered.forget_order temp5 in
   let temp7=Image.image (
      fun x->(x,Option.filter_and_unpack(
        fun (y,ap)->if y=x then Some(ap) else None
      ) temp3)
   ) temp6 in
   temp7;;
 
 let list_value_occurrences_in_file t file=
   let s=Io.read_whole_file file in
   let temp1=Substring.occurrences t s in
   Image.image (fun j->Cull_string.closeup_around_index 
      s j
   ) temp1;;

let show_value_occurrences_in_modulesystem t fs=
   let m=String.length(Directory_name.to_string(Modulesystem.root(fs))) in
   let temp1=Modulesystem.all_mlx_paths fs in
   let temp2=Image.image (fun ap->
    let ttemp1=list_value_occurrences_in_file t ap in
    let mname=Cull_string.cobeginning(m)(Absolute_path.to_string ap) in
    Image.image (fun x->mname^":\n"^x ) ttemp1
    ) temp1 in
   let temp3=Listennou.hard_big_concat temp2 in
   let temp4=String.concat "\n\n\n" (""::temp3@[""]) in 
   print_string temp4;;





end;;






module Find_suitable_ending=struct

(*

#use"find_suitable_ending.ml";;

*)

(*

Note that the order in Ocaml_ending.correspondances is important


*)

exception No_suitable_location of Directory_name.t*(Subdirectory.t list)*string;;

let find_file_location dir l_subdir x=
  let s_dir=Directory_name.to_string(dir) in
  let temp1=Cartesian.product(l_subdir)(""::Ocaml_ending.all_string_endings) in
  let tempf=(fun (sd,edg)->
  let s1=s_dir^(Subdirectory.to_string sd)^x^edg in
  if Sys.file_exists s1
  then Some(Absolute_path.of_string s1)
  else None
  ) in
  let opt=Option.find_and_stop_immediately_after tempf temp1 in
  if opt=None
  then raise(No_suitable_location(dir,l_subdir,x))
  else  Option.unpack(opt);;

end;;






module Current_target_system=struct

(*

#use"Makefile_makers/current_target_system.ml";;

*)




let location_for_current_makefile="makefile";;
let location_for_current_targetfile="targetfile.ocaml_made";;
let location_for_current_loadingsfile="my_loadings.ml";;
let location_for_current_pervasivesfile="my_pervasives.ml";;


let current_root=Current_root_directory.current_root_directory;;
let root_string=Cull_string.coending 1 (Directory_name.to_string current_root);;


let read_archive ()=
  let lm=Directory_name.join current_root location_for_current_targetfile in
   let archive=Io.read_whole_file (Absolute_path.of_string lm) in
   Target_system.unarchive archive ;;

let current=
 let ts=(
   try read_archive() with 
   _-> Target_system.from_root_and_toplevel current_root (Some"ecaml")
 ) in
 {
   Mutable_target_system.content=ref(ts);
   Mutable_target_system.location_for_makefile=location_for_current_makefile;
   Mutable_target_system.location_for_targetfile=location_for_current_targetfile;  
   Mutable_target_system.location_for_loadingsfile=location_for_current_loadingsfile;  
   Mutable_target_system.location_for_pervasivesfile=location_for_current_pervasivesfile
 };;

let current_target_system ()=(!(current.Mutable_target_system.content));; 
 
let current_modulesystem ()=
  Target_system.modulesystem(current_target_system ());; 
 
let refresh ()=
  let temp1=Create_target_system.from_main_directory current_root in 
  (current.Mutable_target_system.content:=(snd temp1); 
  Mutable_target_system.save_all current;);;

let rename_value old_name new_name=   
Values_in_modules.rename_value (current_modulesystem()) old_name new_name;;  
   
let show_value_occurrences s=   
Values_in_modules.show_value_occurrences_in_modulesystem s (current_modulesystem());;   

let values_from_module s=
Values_in_modules.list_values_from_module_in_modulesystem 
   s (current_modulesystem());;

let start_debugging()=
	let ts=(!(current.Mutable_target_system.content)) in
	let fs=Target_system.modulesystem ts in
	let dir=Target_system.root ts in
	let rdir=Compute_modulesystem_directories.compute_modulesystem_directories fs in
	let ap=Find_suitable_ending.find_file_location dir rdir "Optional/ladybird.ml" in
	let hm=Half_dressed_module.of_path_and_root ap dir in
	let tgt=Ocaml_target.debuggable hm in
	let _=Mutable_target_system.machen current tgt in
	(
	  print_string
	  "\n\n Now, start \n\nocamldebug ladybird.ocaml_debuggable\n\nin another terminal\n\n";
	  flush stdout
	);;   
   

let backup ()=
   let uberdir=Father_and_son.father root_string '/' in
   let tempdir_name="Ordinary_"^(Current_date.current_date()) in
   let tempdir=uberdir^"/"^tempdir_name in
   Implement_modulesystem.implement (current_modulesystem ()) tempdir;;
    
    
     


end;;

