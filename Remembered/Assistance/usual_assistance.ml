(*

#use"/Users/ewandelanoy/Documents/OCaml/Ordinary/Remembered/Assistance/usual_assistance.ml";;

And then, call German_pervasives.rsh.
For more exotic problems, consult the global_assistance.ml file.



*)

#load "nums.cma";;
#load "str.cma";;




module Option=struct



exception Unpackable of string;;

let unpack_with_error_message s=function
None->raise(Unpackable(s))
|Some(x)->x;;

let unpack x =unpack_with_error_message "void is not unpackable" x;;


let propagate f=function
None->None
|Some(x)->Some(f(x));;

let rec seek f =function
[]->None
|a::b->if f(a) then Some(a) else seek(f)(b);;

let find f l=unpack(seek f l);;

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



exception Ht_exn;;

let ht x=match x with
    []->raise(Ht_exn)
    |a::b->(a,b);;

let rec uncurrified_rev_append (x,y)=match x with
[]->y
|a::peurrest->uncurrified_rev_append (peurrest,a::y);;

let rec uncurrified_append (x,y)=uncurrified_rev_append (List.rev x,y);;

let factor (x,y)=
    let rec factor0=(fun
       (graet,da_ober1,da_ober2)->
       if (da_ober1=[])||(da_ober2=[])
       then (List.rev graet,da_ober1,da_ober2)
       else let (a1,peurrest1)=ht da_ober1
            and (a2,peurrest2)=ht da_ober2 in
            if a1=a2
            then factor0(a1::graet,peurrest1,peurrest2)
            else (List.rev graet,da_ober1,da_ober2)
    ) in
    factor0([],x,y);;

let comparable_for_prefix_order  a b=
    let (_,a1,b1)=factor(a,b) in (a1=[])||(b1=[]);;


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

exception Not_encountered;;

let rec seek_and_remember f (graet,da_ober)=
   match da_ober with
   []->raise(Not_encountered)
   |a::peurrest->if f a 
                 then (graet,da_ober)
                 else seek_and_remember f (a::graet,peurrest);;

let rec nice_cut small_one large_one=
    match small_one with
     []->Some(large_one)
    |a1::peurrest1->
      (
         match large_one with
         []->None
         |a2::peurrest2->
             if a2=a1
             then nice_cut peurrest1 peurrest2
             else None
      );;


let hi=List.length;;
let rev=List.rev;;

(*
glued_slices 0 [1;2;3;0;4;5;0;6;7;8;0;0;0;9;10;0;11;0;12;13;0;0];;
glued_slices 0 [0;1;2;3;0;4;5;0;6;7;8;0;0;0;9;10;0;11;0;12;13;0;0];;
glued_slices 0 [0;0;1;2;3;0;4;5;0;6;7;8;0;0;0;9;10;0;11;0;12;13;0;0];;



*)

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


let lex_for_strings=
    ((fun s1 s2->
      let m1=String.length s1
      and m2=String.length s2
      in
      let m=Pervasives.min(m1)(m2) in
      match Option.seek (fun j->(String.get s1 j)<>(String.get s2 j)) (Ennig.ennig 0 (m-1)) with
      None->standard m1 m2
      |Some(j)->standard (String.get s1 j) (String.get s2 j) 
    ) : string t);;

let silex_for_strings=
      ((fun s1 s2->
        let m1=String.length s1
        and m2=String.length s2
        in
        let first_try=standard(m1)(m2) in
        if first_try<>Equal
        then first_try
        else lex_for_strings s1 s2
      ) : string t);;    

let lex_for_string_lists=
  ((fun l1 l2->
      let (_,left_part,right_part)=Listennou.factor (l1,l2) in
      if left_part=[] 
      then (if right_part=[] 
           then Equal 
           else Lower)
      else if right_part=[] 
           then Greater 
           else lex_for_strings (List.hd left_part) (List.hd right_part)  
  ) : (string list) t);;

let for_longest_match_pairs=  
((fun (s1,v1) (s2,v2)->
  let first_try=silex_for_strings(s2)(s1) in
  if first_try<>Equal 
  then first_try
  else standard v1 v2
 ): (string*'b) t);;
 
 
 
 


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
   (kengeij kenver x y,lemel kenver x y,lemel kenver y x);;
  
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
  
let diforchan_plaen kenver x=
  forget_order(diforchan kenver  x);;
let elfenn_plaen kenver e x=
    elfenn kenver e (unsafe_set  x);;  
let kengeij_plaen kenver x y=
    forget_order(kengeij kenver  (unsafe_set x) (unsafe_set y) );;
let lemel_plaen kenver x y=
      forget_order(lemel kenver  (unsafe_set x) (unsafe_set y) );;
let teuzin_kalz_plaen kenver l=
        forget_order(big_teuzin kenver  (Image.image unsafe_set l) );;
let insert_plaen kenver x l=
        forget_order(insert kenver x (unsafe_set l));;     

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

let backwards_finder f s w0=
    let rec tempf=(fun j->
      if j<0 then 0 else
      if f(String.get s  j) then j+1 else
      tempf(j-1)
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
          match Option.seek(fun t->
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
     
let reverse s=
   implode(List.rev(explode s));; 
   
let insert_prefixes_at_indices l s=
    if l=[] then s else
    let n=String.length s in
    let temp1=Image.image (fun (pref,idx)->(idx,pref)) l in
    let temp2=Image.image fst temp1 in
    let temp3=Ordered.forget_order(Tidel.diforchan((n+1)::temp2)) in
    let temp4=Listennou.universal_delta_list temp3 in
    let temp5=Image.image(fun (i,j)->
       (List.assoc i temp1)^(String.sub s (i-1) (j-i)) ) temp4 in
    let i1=List.hd temp3 in
    let temp6=(
       if i1=1 then temp5 else (String.sub s 0 (i1-1))::temp5
    )  in 
    String.concat "" temp6;;

(*

insert_prefixes_at_indices ["hap",4;"na",12] "123py678901tion6";;

*)

exception Largest_common_prefix_exn;;

let largest_common_prefix l=
   if l=[] then raise(Largest_common_prefix_exn) else
   let lengths=Image.image String.length l in
   let m=Min.list lengths in
   let tester=(fun k->
     let temp1=Image.image (fun s->String.get s k) l in
     let v=List.hd temp1 in
     List.for_all (fun x->x=v) temp1
   ) in
   let rec tempf=(fun j->
     if j=m then j else 
     if tester(j) then tempf(j+1) else j
   ) in
   let j0=tempf 0 in
   String.sub (List.hd l) 0 j0;;

(*

largest_common_prefix ["abby";"abnormal"];;
largest_common_prefix ["";"call"];;
largest_common_prefix ["sad";"again"];;


*)





   
  


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
      if (String.length(x)<j+ly)||(j<0)
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
      try ((Option.find tester temp1)+1) with
      _->(-1);;
  
   let leftmost_index_of_in_from x y i=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) in
      match Ennig.find_it tester (i-1) (String.length(y)-lx) with
         None->(-1)
        |Some(k)->k+1;;
  
let leftmost_linedex_of_in x y=
    let j=leftmost_index_of_in x y in
    if j<0 then (-1) else
    Strung.number_of_lines_before y j;;

let leftmost_linedex_of_in_from x y i=
        let j=leftmost_index_of_in_from x y i in
        if j<0 then (-1) else
        Strung.number_of_lines_before y j;;    

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

 
   

end;;






module Charset=struct

(*

#use"charset.ml";;

*)

let lowercase_letters=    
  ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';
   'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';
   'u';'v';'w';'x';'y';'z'];;

    
let uppercase_letters= 
   ['A';'B';'C';'D';'E';'F';'G';'H';'I';'J';
    'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';
    'U';'V';'W';'X';'Y';'Z'];;
    
let lowercase_identifier_elements=    
    ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';
     'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';
     'u';'v';'w';'x';'y';'z';'_';'+';'-';'*';
     '0';'1';'2';'3';'4';'5';'6';'7';'8';'9']@uppercase_letters;;
     
let php_label_first_letters =
  [
    'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';
    'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';
    'u';'v';'w';'x';'y';'z';
    'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';
    'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';
    'U';'V';'W';'X';'Y';'Z';
    '_';
    ];;  

 let strictly_alphanumeric_characters =
  php_label_first_letters
  @
  [
   '0';'1';'2';'3';'4';'5';'6';'7';'8';'9'
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
    
 
  

end;;






module Characters_in_namespace_name=struct

(*

#use"Php_analizer/Great_Replacement/characters_in_namespace_name.ml";;

*)


let chars=
  (Ennig.doyle char_of_int 65 90)@
  (Ennig.doyle char_of_int 97 122)@
  (Ennig.doyle char_of_int 48 57)@
  ['\\';'_'];;


end;;






module After=struct

(*

#use"after.ml";;

*)

let after_star l s =
  let n=String.length s in
  let rec tempf=(
    fun j->
      if j>n then None else
      if  List.mem (String.get s (j-1)) l
      then tempf(j+1)
      else Some(j)
  ) in
  tempf;;

let list_of_whites=[' ';'\n';'\r';'\t'];;

let after_whites s =after_star list_of_whites s;;

  let after_whites_and_comments s=
    let n=String.length s in
    let rec tempf=(
      fun j->
        if j>n then None else
        if List.mem (String.get s (j-1)) list_of_whites
        then tempf(j+1)
        else 
        if Substring.is_a_substring_located_at "/*" s j
        then let k=Substring.leftmost_index_of_in_from "*/" s (j+2) in
             if k<0
             then None
             else tempf(k+2)
        else Some(j)
    ) in
    tempf;;
  
  (*    
  after_whites_and_comments "\n/* 567 */\t\r\n\n/* 89 ** // 78*/123";;    
  *)
  
exception Unfinished_simple_quoted_string of int;;  

let after_simple_quoted_string s k0=
    let n=String.length s in
    if (Strung.get s k0)<>'\''
    then k0
    else 
    let rec tempf=(fun k->
       if k>n
       then raise(Unfinished_simple_quoted_string(k0))
       else 
       let c=String.get s (k-1) in
       if c='\\'
       then tempf(k+2)
       else 
       if c='\''
       then k+1
       else tempf(k+1)
    ) in
    tempf (k0+1);;

exception Unfinished_double_quoted_string of int;;  
    
let after_double_quoted_string s k0=
        let n=String.length s in
        if (Strung.get s k0)<>'"'
        then k0
        else 
        let rec tempf=(fun k->
           if k>n
           then raise(Unfinished_double_quoted_string(k0))
           else 
           let c=String.get s (k-1) in
           if c='\\'
           then tempf(k+2)
           else 
           if c='"'
           then k+1
           else tempf(k+1)
        ) in
        tempf (k0+1);;     



exception Unbalanced_expression of char*char;;

let after_closing_character (lchar,rchar) s=
  let n=String.length s in
  let rec tempf=(
    fun (k,count)->
      if k>n
      then raise(Unbalanced_expression(lchar,rchar))
      else 
      if Substring.is_a_substring_located_at "/*" s k
      then let j=Substring.leftmost_index_of_in_from "*/" s (k+2) in
           tempf(j+2,count)
      else 
      if Substring.is_a_substring_located_at "//" s k
      then let j=Substring.leftmost_index_of_in_from "\n" s (k+2) in
           tempf(j+1,count)
      else 
      if (Substring.is_a_substring_located_at "<<<EOF\n" s k)
         ||
         (Substring.is_a_substring_located_at "<<<'EOF'\n" s k) 
      then let j=Substring.leftmost_index_of_in_from "\nEOF;\n" s (k+7) in
           tempf(j+6,count)
      else 
      let c=String.get s (k-1) in
      if c=lchar
      then tempf(k+1,count+1)
      else 
      if c='\''
      then let j=after_simple_quoted_string s k in
           tempf(j,count)
      else
      if c='"'
      then let j=after_double_quoted_string s k in
           tempf(j,count)
      else     
      if c<>rchar
      then tempf(k+1,count)
      else 
        if count=1
        then k+1
        else tempf(k+1,count-1)
  ) in
  tempf;;

(*

after_closing_character ('{','}') "{ 345 }89" (1,0);;
after_closing_character ('{','}') "{2{4}6{8{0}2}4}67" (1,0);;
after_closing_character ('{','}') "{\"3}5\"}89" (1,0);;
after_closing_character ('{','}') "{'3}5'}89" (1,0);;
after_closing_character ('{','}') "{/*4}6*/}01" (1,0);;
after_closing_character ('{','}') "{<<<EOF\n}\nEOF;\n}78" (1,0);;
after_closing_character ('{','}') "{<<<'EOF'\n}\nEOF;\n}90" (1,0);;

*)

let next_in_list l s=
  let n=String.length s in
  let rec tempf=(
    fun k->
      if k>n
      then None
      else 
      if Substring.is_a_substring_located_at "/*" s k
      then let j=Substring.leftmost_index_of_in_from "*/" s (k+2) in
           tempf(j+2)
      else 
      if Substring.is_a_substring_located_at "//" s k
      then let j=Substring.leftmost_index_of_in_from "\n" s (k+2) in
           tempf(j+1)
      else 
      if (Substring.is_a_substring_located_at "<<<EOF\n" s k)
         ||
         (Substring.is_a_substring_located_at "<<<'EOF'\n" s k) 
      then let j=Substring.leftmost_index_of_in_from "\nEOF;\n" s (k+7) in
           tempf(j+6)
      else 
      let c=String.get s (k-1) in
      if c='\''
      then let j=after_simple_quoted_string s k in
           tempf(j)
      else
      if c='"'
      then let j=after_double_quoted_string s k in
           tempf(j)
      else     
      if List.mem c l
      then Some(k)
      else tempf(k+1)
  ) in
  tempf;;


let classlike_declaration_chars=list_of_whites@Characters_in_namespace_name.chars;;

let after_classlike_declaration s i=
    Option.seek(
     fun j->not(List.mem 
         (String.get s (j-1)) classlike_declaration_chars
     )
    )(Ennig.ennig i (String.length s));;


let after_abstract_class s i0=
  if not(Substring.is_a_substring_located_at "abstract" s i0)
  then None
  else
  let opt1=after_whites s (i0+8) in
  if opt1=None then None else
  let i1=Option.unpack opt1 in
  if not(Substring.is_a_substring_located_at "class" s i1)
  then None
  else 
  let opt2=after_classlike_declaration s (i1+5) in
  if opt2=None then None else
  let i2=Option.unpack opt2 in
  if (Strung.get s i2)<>'{' then None else 
  Some(after_closing_character ('{','}') s (i2+1,1));;

(*

after_abstract_class "abstract  class {u\nv}234" 1;;

*)

let after_final_class s i0=
  if not(Substring.is_a_substring_located_at "final" s i0)
  then None
  else
  let opt1=after_whites s (i0+5) in
  if opt1=None then None else
  let i1=Option.unpack opt1 in
  if not(Substring.is_a_substring_located_at "class" s i1)
  then None
  else 
  let opt2=after_classlike_declaration s (i1+5) in
  if opt2=None then None else
  let i2=Option.unpack opt2 in
  if (Strung.get s i2)<>'{' then None else 
  Some(after_closing_character ('{','}') s (i2+1,1));;     

(*

after_final_class "final  class {u\nv}901" 1;;

*)

let after_usual_class s i0=
  if not(Substring.is_a_substring_located_at "class" s i0)
  then None
  else 
  let opt2=after_classlike_declaration s (i0+5) in
  if opt2=None then None else
  let i2=Option.unpack opt2 in
  if (Strung.get s i2)<>'{' then None else 
  Some(after_closing_character ('{','}') s (i2+1,1));;     

(*

after_usual_class "class {u\nv}234" 1;;
after_usual_class "class_loader { }" 1;;

*)

let after_interface s i0=
  if not(Substring.is_a_substring_located_at "interface" s i0)
  then None
  else 
  let opt2=after_classlike_declaration s (i0+5) in
  if opt2=None then None else
  let i2=Option.unpack opt2 in
  if (Strung.get s i2)<>'{' then None else 
  Some(after_closing_character ('{','}') s (i2+1,1));;  

(*

after_interface "interface {u\nv}678" 1;;

*)

let after_classlike_block s i=
   Option.find_and_stop(
     fun f->f s i
   )[
       after_abstract_class;
       after_final_class;
       after_usual_class;
       after_interface;
    ];;


(*

after_classlike_block "abstract  class {u\nv}234" 1;;
after_classlike_block "final  class {u\nv}901" 1;;
after_classlike_block "class {u\nv}234" 1;;
after_classlike_block "interface {u\nv}678" 1;;

*)    

let after_classlike_block_with_linebreak s i=
  let n=String.length s in
  let opt1=after_classlike_block s i in
  if opt1=None then None else
  let i1=Option.unpack opt1 in
  let opt2=Option.seek(fun j->
     not(List.mem (Strung.get s j) [' ';'\r';'\t']) )
  (Ennig.ennig i1 n) in
  if opt2=None then None else
  let i2=Option.unpack opt2 in
  if Strung.get s i2='\n'
  then Some(i2+1)
  else None;;
    
(*

after_classlike_block_with_linebreak "abstract  class {u\nv}  \t \n7" 1;;
after_classlike_block_with_linebreak "final  class {u\nv} \t\t\n3" 1;;
after_classlike_block_with_linebreak "class {u\nv}\n3" 1;;
after_classlike_block_with_linebreak "interface {u\nv} \t\n9" 1;;

*)    

exception End_of_div_not_found;;

let rec main_helper_for_div (s,n,div_count,idx)=
    if idx>n
    then raise(End_of_div_not_found)
    else
    if Substring.is_a_substring_located_at "</div>" s idx
    then if div_count=1
         then idx+6
         else main_helper_for_div(s,n,div_count-1,idx+6)
    else 
    if not(Substring.is_a_substring_located_at "<div " s idx)
    then main_helper_for_div(s,n,div_count,idx+1)
    else  
    let jdx=Substring.leftmost_index_of_in_from ">" s (idx+5) in
    main_helper_for_div(s,n,div_count+1,jdx);;

let after_div s idx=main_helper_for_div(s,String.length s,0,idx);;

(*

after_div "<div val=\"abc\"> xyz </div>789" 1;;

*)

let after_one pattern s idx=
  if Substring.is_a_substring_located_at pattern s idx
  then Some(idx+String.length pattern)
  else None;;

let after_one_among_several l_patterns s idx=
   Option.find_and_stop (
     fun pattern->after_one pattern s idx
   ) l_patterns;;

let  after_php_label s idx=
   if not(List.mem (Strung.get s idx) Charset.php_label_first_letters)
   then None
   else
   after_star 
     Charset.strictly_alphanumeric_characters s (idx+1);;
     





end;;






module Overwriter=struct

(*

#use"overwriter.ml";;

*)


type t=Ovw of string;;

let of_string s=Ovw(s);;
let to_string (Ovw s)=s;;


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






module Unix_command=struct

(*

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

let hardcore_verbose_uc s=
   let _=(print_string ("Executing "^s^"\n\n");flush stdout) in
   hardcore_uc s;;

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






module Capitalize_directory_names=struct

(*

#use"capitalize_directory_names.ml";;


*)

let cdn s=
  let n=String.length(s) in
  let temp1=List.filter(
     fun j->(String.get s j)='/'
  )(Ennig.ennig 0 (n-1)) in
  if temp1=[] then s else
  let temp4=List.rev(List.tl(List.rev temp1)) in
  let temp2=0::(Image.image (fun j->j+1) temp4) in
  let tempf=(fun j->
    let t=String.make 1 (String.get s j) in
    if List.mem j temp2
    then String.capitalize_ascii t
    else t
  ) in
  let temp3=Ennig.doyle tempf 0 (n-1) in
  String.concat "" temp3;;

(*

cdn "do/You/feel/like/sixty/feet";;
cdn "/do/You/feel/like/sixty/feet";;
cdn "do/You/feel/like/sixty/feet/";;
cdn "/do/You/feel/like/sixty/feet/";;

cdn "peggy";;
cdn "peggy/lee";;

*)

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
   Capitalize_directory_names.cdn(absolute_path_of_string1 s);;

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
    let _=Unix_command.uc ("rm -f "^g1) in
    let _=cr g1 in
    let _=Unix_command.uc ("mv "^g1^" "^w) in
    let _=Unix_command.uc ("rm -f "^g1) in
    of_string w;;
    
let print_out (fmt:Format.formatter) ap=
   Format.fprintf fmt "@[%s@]" (to_string ap);;



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
  
let overwrite_with ap s=
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
  overwrite_with ap new_content;; 

     
   
   
  


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

let without_trailing_slash (D s)=s;;

let connectable_to_subpath (D s)=s^"/";;

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
   else let _=Unix_command.uc("touch "^t) in
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






module Unjoin_path=struct


(* 


#use"unjoin_path.ml";;



*)

let unjoin_path ap=
  let (t1,t2)=Father_and_son.father_and_son (Absolute_path.to_string ap) '/' in
  (Directory_name.of_string(t1),
   No_slashes.of_string(t2));; 
  



   
   
   

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

let neighborhood_with_center_and_size s i d=
   let a=max(1)(i-d)
   and b=min(String.length s)(i+d) in
   interval s a b;;

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

let trim_spaces_on_the_left s=
      let n=String.length s in
      match Option.seek(fun j->
          not(List.mem (String.get s (j-1)) [' ';'\t';'\r';'\n'])
      )(Ennig.ennig 1 n) with
      None->""
      |Some(d)->cobeginning (d-1) s;;

let trim_spaces_on_the_right s=
      let n=String.length s in
      match Option.seek(fun j->
          not(List.mem (String.get s (n-j)) [' ';'\t';'\r';'\n'])
      )(Ennig.ennig 1 n) with
      None->""
      |Some(d)->coending (d-1) s;;
              

 let trim_spaces s=
   let n=String.length s in
   let opt1=Option.seek(fun j->not(List.mem(String.get s (j-1)) [' ';'\r';'\t';'\n']))(Ennig.ennig 1 n) in
   if opt1=None then "" else
   let i1=Option.unpack opt1 in
   let k1=Option.find(fun j->not(List.mem(String.get s (n-j)) [' ';'\r';'\t';'\n']))(Ennig.ennig 1 n) in 
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
  
let rewrite_duration x=
   if x=0. 
   then "Computation was quick.\n"
   else "Computation lasted "^(rewrite_float x)^"\n";;

 let timer=ref(0.000);;  
 
 let duration_of_computation f x=
   let t0=Unix.time() in
   let _=f(x) in
   let _=(timer:=Unix.time()-.t0) in
   (print_string(rewrite_duration (!timer));flush stdout);;
 
 let duration_of_last_computation ()=
  (print_string(rewrite_duration (!timer));flush stdout);;
   
   
 let  it f x=
  let t0=Unix.time() in
   let y=f(x) in
   let _=(timer:=Unix.time()-.t0) in
   let _=(print_string(rewrite_duration (!timer));flush stdout) in
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
         let temp2=Option.seek checker temp1 in
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
         let temp2=Option.seek checker temp1 in
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






module Subdirectory=struct

(*

Subdirectories name, with the trailing slash removed.

#use"subdirectory.ml";;

*)

type t=SD of string;;

let without_trailing_slash (SD s)=s;;


let of_string s=SD s;;

let depth (SD s)=
 if s="" then 0 else
 (List.length(Substring.occurrences_of_in "/" s))+1;;

let connectable_to_subpath (SD s)=if s="" then "" else s^"/";;

let rename_endsubdirectory (SD(old_subdir),new_esdname) (SD s)=
   if Substring.begins_with s old_subdir
   then let sub_s=Cull_string.cobeginning (String.length old_subdir) s in
        let t=Father_and_son.father old_subdir '/' in
        let new_t=(if t="" then "" else t^"/") in
        SD(new_t^new_esdname^sub_s)
   else SD(s);;
   
(*

rename_endsubdirectory (SD("Haag/Huug"),"Java") (SD "Haag/Huug/King/Jordan");;
rename_endsubdirectory (SD("Haag"),"Java") (SD "Haag/Huug/King/Jordan");;

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
   let s=Directory_name.connectable_to_subpath dir in
   let i=String.rindex s '/' in
   if i=0 then "" else
   (Cull_string.cobeginning (i+1) s);; 
  
 let is_a_nondirectory_or_a_nib x=
  if is_a_directory(x)
  then extension(x)="nib"
  else not(Substring.is_a_substring_of(".nib/")(Absolute_path.to_string x));;
  
 let naive_ls dir=
   let s=Directory_name.connectable_to_subpath dir in
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
   let s_dir=Directory_name.connectable_to_subpath dir in
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
  let n=String.length(Directory_name.connectable_to_subpath x) in
  let temp1=complete_ls x in
  Image.image (fun ap->Cull_string.cobeginning n (Absolute_path.to_string ap)) temp1;; 
  
end;;    

 
let complete_ls=Private.complete_ls;;
let is_a_directory=Private.is_a_directory;;
let quick_beheaded_complete_ls=Private.quick_beheaded_complete_ls;;
let complete_ls_with_nondirectories_only=Private.complete_ls_with_nondirectories_only;;

let all_files_with_endings dir l_endings=
   let temp1=complete_ls dir in
   let temp2=List.filter(
   fun ap->
     let s_ap=Absolute_path.to_string ap in
     List.exists( fun ending->
       Substring.ends_with s_ap ending)
     l_endings  
   ) temp1 in
   temp2;;  



   


end;;






module Industrial_separator=struct

(*

#use"jindustrial_separator.ml";;

Separators used in encoding of data types by strings.
The main key should not appear in any string inside the data types,
change it if necessary.

*)

let key="mpdykruvueaoqhkt";;

let slow_copy_task1=key^"001";;
let slow_copy_task2=key^"002";;
let half_dressed_module=key^"003";;
let mlx_ended_absolute_path=key^"004";;
let ocaml_target1=key^"005";;
let ocaml_target2=key^"006";;
let modulesystem_data1=key^"007";;
let modulesystem_data2=key^"008";;
let alaskan_or_german_data=key^"009";;
let alaskan_save_all1=key^"010";;
let alaskan_save_all2=key^"011";;


  

end;;






module Separator=struct

(*

#use"separator.ml";;

*)

type t=S of string;;

let of_string s=S s;;
let to_string (S s)=s;;



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
  
 let make_aggregates_if_possible sep=function
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
   let string_sep=Separator.to_string(sep) in
   Sl(Image.image (String.concat string_sep) temp1);;


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
  try (fst(Option.find (fun (x,y)->y=s) correspondances))
  with _->raise(Unknown_ending(s));;

let to_string edg=snd(Option.find (fun (x,y)->x=edg) correspondances);;  



let ocaml_name w=
 let c="Ocaml_ending"^"." in
 match w with
 Ml->c^"Ml"
|Mli->c^"Mli"
|Mll->c^"Mll"
|Mly->c^"Mly";;


end;;






module Naked_module=struct

(*

#use"naked_module.ml";;

A module name, or a candidate for one. Uncapitalized. Should contain no slashes.

*)

type t=N of string;;

let of_string s=N (String.uncapitalize_ascii s);; 
let to_string (N s)=s;;



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
  let new_path=(Directory_name.connectable_to_subpath dir)^(No_slashes.to_string new_name) in
  let i=Unix_command.uc("mv "^old_path^" "^new_path) in
  if i<>0
  then raise(Failed(ap,new_name))
  else Absolute_path.of_string new_path;;



   
   
   

end;;






module German_constant=struct


(* 

#use"Country/Germany/german_constant.ml";;


*)


let root=Directory_name.of_string "/Users/ewandelanoy/Documents/OCaml/Ordinary";;
let dir_for_backup=Directory_name.of_string 
"/Users/ewandelanoy/Documents/OCaml/Githubbed_ocaml";;

let main_toplevel_name="ecaml";;
let special_dirs=Image.image (fun sd->Subdirectory.SD "") ["Remembered";"Forgotten"];;


let name_for_makefile="makefile";;
let name_for_targetfile="targetfile.ocaml_made";;
let name_for_loadingsfile="my_loadings.ml";;
let name_for_pervasivesfile="my_pervasives.ml";;
let name_for_printersfile="my_printers.ml";;


  
 
 
 


end;;






module Rename_endsubdirectory=struct

(*

#use"rename_endsubdirectory.ml";;


*)

exception Already_present_directory of string;;

let in_unix_world (old_subdir,new_esdname)=
   let s_root=Directory_name.connectable_to_subpath(German_constant.root) in
   let s_old_subdir=Subdirectory.without_trailing_slash old_subdir in
   let new_name=s_root^(Father_and_son.father s_old_subdir '/')^"/"^new_esdname in
   if Sys.file_exists(new_name)
   then raise(Already_present_directory(new_name))
   else 
   let container=Father_and_son.father  new_name '/' in
   let _=
   Unix_command.uc 
     ("mkdir -p "^container) in
   Unix_command.uc 
     ("mv "^s_root^s_old_subdir^" "^new_name) ;;

let re (old_subdir,new_esdname) s=
   let s_old_subdir=Subdirectory.without_trailing_slash old_subdir in
   if Substring.begins_with s s_old_subdir
   then let sub_s=Cull_string.cobeginning (String.length s_old_subdir) s in
        (Father_and_son.father s_old_subdir '/')^"/"^new_esdname^sub_s
   else s;;
   
let on_absolute_path (old_subdir,new_subdirname) ap=
  let s_old_subdir=Subdirectory.connectable_to_subpath old_subdir in
  let s_ap=Absolute_path.to_string ap in
  let old_fulldir=(Directory_name.connectable_to_subpath(German_constant.root))^s_old_subdir in
  if Substring.begins_with s_ap old_fulldir
  then let sub_s=Cull_string.cobeginning (String.length old_fulldir) s_ap in
       Absolute_path.of_string(new_subdirname^sub_s)
  else ap;;   
   
   
   
(*

re (Subdirectory.of_string("Haag/Huug"),"Java") ("Haag/Huug/King/Jordan/and_co.ml");;

*)   

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
  let new_path=(Directory_name.connectable_to_subpath new_dir)^(No_slashes.to_string fn) in
  let i=Unix_command.uc("mv "^old_path^" "^new_path) in
  if i<>0
  then raise(Failed(ap,new_dir))
  else Absolute_path.of_string new_path;;



   
   
   

end;;






module Path_is_in_directory=struct

(*
#use"path_is_in_directory.ml";;
*)

let path_is_in_directory ap dir=
  Substring.begins_with
   (Absolute_path.to_string ap)
   (Directory_name.connectable_to_subpath dir)
;;

end;;






module Half_dressed_module=struct

(*

#use"half_dressed_module.ml";;

A module name, or a candidate for one. Can contain  slashes.
Should not contain dots.
Starts with an uncapitalized letter.
Designates a relative path.

*)


          
type t={
   bundle_main_dir : string;
   subdirectory    : string;
   naked_module     : string;
};;

let bundle_main_dir x=Directory_name.of_string(x.bundle_main_dir);;
let subdirectory x=Subdirectory.of_string(x.subdirectory);;
let naked_module  x=Naked_module.of_string(x.naked_module);;


exception Inexistent_module of string;;
 
let of_string_and_root old_s dir=
        let s=Father_and_son.invasive_father old_s '.' in
        let s_dir=Directory_name.without_trailing_slash dir in
	    if List.for_all (fun edg->not(Sys.file_exists(s_dir^"/"^s^edg)) ) Ocaml_ending.all_string_endings
	    then raise(Inexistent_module(s_dir^s))
	    else
	    {
	      bundle_main_dir = s_dir;
   		  subdirectory    =Father_and_son.father s '/';
          naked_module     =Father_and_son.son s '/';
	    };;  
   
let to_string x=
   let sub=x.subdirectory in
   if sub=""
   then x.naked_module
   else sub^"/"^(x.naked_module);;

let to_shortened_string x=x.naked_module;;   

let unveil x=(to_string x,bundle_main_dir x);;

exception FileOutsideDirectory of Absolute_path.t*Directory_name.t;;


let of_path_and_root ap dir=
    if (not(Path_is_in_directory.path_is_in_directory ap dir))
    then raise(FileOutsideDirectory(ap,dir))
    else 
    let s_dir=Directory_name.without_trailing_slash dir in
    let n_dir=(String.length s_dir)+1 in
    let subpath=Cull_string.cobeginning n_dir (Absolute_path.to_string ap) in
    let s=Father_and_son.invasive_father subpath '.' in
    {
	      bundle_main_dir = s_dir;
   		  subdirectory    =Father_and_son.father s '/';
          naked_module     =Father_and_son.son s '/';
    }  ;;    

let is_optional x=
  let s=to_string x in
  if String.length(s)<9 then false else
  String.sub s 0 9="Optional/";;

let is_forgotten x=
  let s=to_string x in
  if String.length(s)<10 then false else
  String.sub s 0 10="Forgotten/";;


let is_remembered x=
  let s=to_string x in
  if String.length(s)<11 then false else
  String.sub s 0 11="Remembered/";;

let is_archived hm=(is_optional hm)||(is_forgotten hm)||(is_remembered hm);;

let is_executable x=
  let s=to_string x in 
  let n=String.length s in
  if String.length(s)<10 then false else
  String.sub s (n-10) 10="executable";;

let capitalized_module_name x=
  (String.capitalize_ascii x.naked_module);;
  
let rename_endsubdirectory (old_subdir,new_subdirname) x=
   {
	      bundle_main_dir = x.bundle_main_dir;
   		  subdirectory    = Subdirectory.without_trailing_slash(
   		                    Subdirectory.rename_endsubdirectory
   		                       (old_subdir,new_subdirname) 
   		                       (Subdirectory.of_string(x.subdirectory)));
          naked_module    = x.naked_module;
    }  ;;    
   

let industrial_separator=Industrial_separator.half_dressed_module;;  

let archive x=
   String.concat industrial_separator 
    [x.bundle_main_dir;x.subdirectory;x.naked_module];;
  
let unarchive s=
   let l1=Str.split (Str.regexp_string industrial_separator) s in
   {
	      bundle_main_dir = List.nth l1 0;
   		  subdirectory    = List.nth l1 1;
          naked_module    = List.nth l1 2;
    };;  
   
            
          
   


end;;






module Mlx_ended_absolute_path=struct

(*

#use"mlx_ended_absolute_path.ml";;

*)

type t=MLX of Ocaml_ending.t*string*Directory_name.t;;

exception Unknown_ending of string;;
exception Unpointed_filename of string;;

exception Inexistent_filename of string;;

let short_path (MLX(edg,s,_))=match edg with
   Ocaml_ending.Ml->  s^".ml"
  |Ocaml_ending.Mli-> s^".mli"
  |Ocaml_ending.Mll-> s^".mll"
  |Ocaml_ending.Mly-> s^".mly";;

let to_string=short_path;;

let of_string_and_root s dir= 
  if not(String.contains s '.') then raise(Unpointed_filename(s)) else
  let (core,ending)=Father_and_son.father_and_son s '.' in
  let s_dir=Directory_name.connectable_to_subpath dir in
  if (not(Sys.file_exists(s_dir^s)))
  then raise(Inexistent_filename(s_dir^s))
  else
  if ending="ml"  then MLX (Ocaml_ending.ml,core,dir) else
  if ending="mli" then MLX (Ocaml_ending.mli,core,dir) else
  if ending="mll" then MLX (Ocaml_ending.mll,core,dir) else
  if ending="mly" then MLX (Ocaml_ending.mly,core,dir) else
  raise(Unknown_ending(s));;

let try_from_string_and_root s dir=
  try (Some(of_string_and_root s dir)) with _->None;;


let root (MLX(_,_,dir))=dir;;

exception FileOutsideDirectory of Absolute_path.t*Directory_name.t;;


let of_path_and_root ap dir=
    if (not(Path_is_in_directory.path_is_in_directory ap dir))
    then raise(FileOutsideDirectory(ap,dir))
    else 
    let s_dir=Directory_name.connectable_to_subpath dir in
    let n_dir=String.length s_dir in
    let subpath=Cull_string.cobeginning n_dir (Absolute_path.to_string ap) in
    of_string_and_root subpath dir;;    

let try_from_path_and_root ap dir=
    try (Some(of_path_and_root ap dir)) with _->None;;

let decompose (MLX(edg,s,dir))=
  (Half_dressed_module.of_string_and_root s dir,edg);;

let half_dressed_core mlx=fst(decompose mlx);;
let ending mlx=snd(decompose mlx);;

let to_path mlx=
  let (hm,edg)=decompose mlx in
  let dir=root mlx in
  let s_hm=Half_dressed_module.to_string hm 
  and s_dir=Directory_name.connectable_to_subpath dir in
  Absolute_path.of_string( s_dir^s_hm^(Ocaml_ending.to_string edg) );;

let join hs ending=
  let (s,dir)=Half_dressed_module.unveil hs in
  MLX(ending,s,dir);;

  
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
  let s_new_subdir=Subdirectory.connectable_to_subpath new_subdir
  and dir=root mlx in
  let s_dir=Directory_name.connectable_to_subpath dir in
  let new_dir=Directory_name.of_string(s_dir^s_new_subdir) in
  let ap=to_path mlx in
  let new_ap=Relocate_file.relocate ap new_dir in
  of_path_and_root new_ap (root mlx);;  
  
let rename_endsubdirectory (subdir,newdirname) (MLX(edg,s,dir))=
  MLX(edg,Rename_endsubdirectory.re (subdir,newdirname) s,dir);;
  
let is_optional x=Half_dressed_module.is_optional(half_dressed_core x);;  
let is_archived x=Half_dressed_module.is_archived(half_dressed_core x);;  

let complete_ls dir=
  let temp1=Directory_name.connectable_to_subpath dir in
  let temp2=More_unix.quick_beheaded_complete_ls temp1 in
  let temp3=Option.filter_and_unpack(
     fun s->try_from_string_and_root s dir
  ) temp2 in
  List.filter (fun mlx->not(is_archived mlx)) temp3;;

let to_absolute_path mlx=
  let s=short_path mlx
  and dir=root mlx in
 let s_dir=Directory_name.connectable_to_subpath dir in
 Absolute_path.of_string(s_dir^s);;   


let ocaml_name w=
  let s=short_path w
  and dir=root w in
  "Mlx_file"^"name"^".of_string_and_index("^
  (Strung.enclose s)^
  ")("^(Directory_name.connectable_to_subpath dir)^")";;    

let industrial_separator=Industrial_separator.mlx_ended_absolute_path;;  
 


let prepare_archive (MLX(edg,s,dir))=
  let s_edg=Ocaml_ending.to_string(edg) in
  let shortened_s_edg=String.sub s_edg 1 (String.length(s_edg)-1) in
  [shortened_s_edg;s;Directory_name.connectable_to_subpath dir];;

  
let archive x=String.concat industrial_separator (prepare_archive x);;
 

let unarchive s=
   let l1=Str.split (Str.regexp_string industrial_separator) s in
   let edg=List.hd l1 
   and s=List.nth l1 1 
   and dir=Directory_name.of_string(List.nth l1 2) in
   MLX(Ocaml_ending.of_string("."^edg),s,dir);;


end;;






module Outside_comments_and_strings=struct

(*

#use"outside_comments_and_strings.ml";;

Detect in a text the parts which can possibly contain module
names, i.e. those parts which are outside comments and outside
strings.

Comments are a little more complicated than strings because they
can be nested. Also, note that we can have strings inside comments :
for example (* a " ( * " b *) is a valid OCaml code snippet.

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
    lastchar_is_a_left_paren          : bool;
    lastchar_is_a_star                : bool;
    lastchar_is_a_backslash           : bool;
    rightmost_backslash_count_is_even : bool;
    penultchar_is_a_left_paren        : bool;
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
   let string_opened_now=(c='"')&&(not(x.string_mode))&&
       (not(x.lastchar_is_a_backslash))
   and string_closed_now=(c='"')&&(x.string_mode)&&(
      if x.lastchar_is_a_backslash
      then x.rightmost_backslash_count_is_even
      else true
   ) in
   let new_start=
      ((x.depth=0)&&string_closed_now)
      ||
      ((x.depth=1)&&comment_closed_now)
      ||
      ((x.depth=0)&&comment_opened_now) in
    let optional_last_index_for_interval=(
       if x.depth>0
       then None
       else
       if string_opened_now
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
       match optional_last_index_for_interval with
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
    string_mode    =(if string_opened_now then true else
                     if string_closed_now then false else
                     x.string_mode);
    lastchar_is_a_left_paren   =(c='(');
    lastchar_is_a_star         =(c='*');
    lastchar_is_a_backslash    =(c='\\');
    rightmost_backslash_count_is_even=(if c<>'\\' 
                                       then true 
                                       else not(x.rightmost_backslash_count_is_even) );
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
    lastchar_is_a_backslash    =false;
    rightmost_backslash_count_is_even=true;
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

[
((good_substrings "abcdef")=    [1, 6, "abcdef"]);
((good_substrings "(*abc*)def")=[8, 10, "def"]);
((good_substrings "ab(*cde*)f")=[1, 2, "ab"; 10, 10, "f"]);
((good_substrings "let a=\"\\\"\" in a+1;;")=[1, 6, "let a="; 11, 19, " in a+1;;"] );
((good_substrings "let a='\\\"' in a+2;;")=[1, 19, "let a='\\\"' in a+2;;"]  );
((good_substrings "let a=\"\\\\\" in a+3;;")=[1, 6, "let a="; 11, 19, " in a+3;;"]  );
((good_substrings "let a=\"\\\\\\\" in a+3;;")=[1, 6, "let a="]  );
];;

good_substrings "ab\"cde\"f";;
good_substrings "\"abc\"def";;
good_substrings "ghi(*a(*b*)c*)def";;
good_substrings "ghi(**a(*b*)c**)def";;
good_substrings "ghi(**a\"b\"c**)def";;
good_substrings "123\"(*\"890\"*)\"567";;
good_substrings "123(*67\"90\"23*)67";;


let nachste (s,n,j,st)=(s,n,j+1,one_more_step s n j (String.get s (j-1)) st);;
let s0="123456\"\\\\\"123456789";;
let n0=String.length s0;;
let v0=(s0,n0,1,initial_state);;
let ff=Memoized.small nachste v0;;
let gg n=match ff n with (_,_,_,st)->st;;


*)      



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
  let temp5=Image.image (fun (x,y,z)->Naked_module.of_string 
      (String.uncapitalize_ascii  y)) temp4 in
  temp5;;

let indices_in_file file=indices_in_string(Io.read_whole_file file);;  
let names_in_file file=names_in_string(Io.read_whole_file file);;



let change_module_name_in_string
   old_naked_name
   new_naked_name s=
   let old_name=String.capitalize_ascii(Naked_module.to_string(old_naked_name))
   and new_name=String.capitalize_ascii(Naked_module.to_string(new_naked_name)) in
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
   Io.overwrite_with file new_s;;  

let change_several_module_names_in_string l_changes s=
    List.fold_left(fun t (u,v)->change_module_name_in_string u v t) s l_changes;;

let change_several_module_names_in_file l_changes file=
   let s=Io.read_whole_file file in
   let new_s=change_several_module_names_in_string l_changes s in
   Io.overwrite_with file new_s;;  


(*   
   
indices_in_string "123 Haag.012 open Garfield;8";;

indices_in_string "(* Haag. *)234 Dog.\"open Garfield;\"67 Corn.4";;

   
*)   

end;;






module Recently_deleted=struct

(*

#use"recently_deleted.ml";;

*)

type t=RD of string list;;

let of_string_list l=RD l;;
let to_string_list (RD l)=l;;


end;;






module Recently_created=struct

(*

#use"recently_created.ml";;

*)

type t=RC of string list;;

let of_string_list l=RC l;;
let to_string_list (RC l)=l;;


end;;






module Recently_changed=struct

(*

#use"recently_changed.ml";;

*)

type t=RC of string list;;

let of_string_list l=RC l;;
let to_string_list (RC l)=l;;


end;;






module Ocaml_gsyntax_category=struct

(*

#use"Ocaml_analysis/ocaml_gsyntax_category.ml";;

*)

type t=
     Value
    |Type
    |Exception 
    |Module_opener
    |Module_closer 
    |Module_inclusion;;
    
    

end;;






module Ocaml_gsyntax_item=struct

(*

#use"Ocaml_analysis/ocaml_gsyntax_item.ml";;

*)

type t={
  category : Ocaml_gsyntax_category.t;
  name : string;
  interval_for_name : int*int;
  whole : string;
  content : string;
  interval_for_content : int*int;  
  is_an_included_item : bool;
};;

let name x=x.name;;
let content x=x.content;;
let whole x=x.whole;;

let make cat nm nm_itv intr ctnt ctnt_itv incldd_or_not=
    {
  		category =cat;
        name =nm;
        interval_for_name =nm_itv;
        whole =intr;
        content =ctnt;
        interval_for_content =ctnt_itv;  
        is_an_included_item =incldd_or_not;
    };;

let prepend_prefix prefix x=
    {
  		category =x.category;
        name =prefix^"."^x.name;
        interval_for_name =x.interval_for_name;
        whole =x.whole;
        content =x.content;
        interval_for_content =x.interval_for_content;  
        is_an_included_item =x.is_an_included_item;
    };;
    
let include_in_new_namespace new_nmspc x=
    {
  		category =x.category;
        name =new_nmspc^(Father_and_son.invasive_father x.name '.');
        interval_for_name =x.interval_for_name;
        whole =x.whole;
        content =x.content;
        interval_for_content =x.interval_for_content;  
        is_an_included_item =true;
    };;    
    
    
    
    
    
    
    
    

end;;






module Gparser_result=struct

(*

#use"GParser/hparser_result.ml";;

*)


type t={
   whole_range : int*int ;
   important_ranges : (int*int) list;
   final_cursor_position : int; 
   disjunction_index : int option;
};;

let whole_range x=x.whole_range;;
let important_ranges x=x.important_ranges;;
let final_cursor_position x=x.final_cursor_position;;
let disjunction_index x=x.disjunction_index;;

let veil b c d e={
   whole_range =b;
   important_ranges =c;
   final_cursor_position =d; 
   disjunction_index=e;
};;




end;;






module List_with_indices=struct

(*

#use"list_with_indices.ml";;

*)

exception Bad_set_of_indices;;

let list_with_indices l=
  let n=List.length l in
  let temp1=Ennig.doyle (fun i->Option.seek(fun p->fst(p)=i) l) 1 n in
  if List.mem None temp1
  then raise(Bad_set_of_indices)
  else
  Ennig.doyle (fun
     i->snd(Option.find(fun p->fst(p)=i) l)
  ) 1 n;;

(*

list_with_indices [3,"a";1,"b";2,"c"];;

*)  


end;;






module Gparser=struct

(*

#use"GParser/gparser.ml";;

*)

type t=
     Constant of string
    |Enclosure of string*string
    |Footless_constant of string
    |Sample_char of string
    |Sample_neg of string
    |Sample_star of string
    |Sample_negstar of string
    |Sample_plus of string
    |Race of string*string
    |Comment of string*string*string*string
    |House_with_doors of string*string*((string*string) list)
    |Chain of t list
    |Disjunction of t list
    |Star of t
    |Detailed_star of t
    |One_or_more of t
    |Optional of t
    |Recoiling_ending of t*t
    |Detailed_chain of t list
;;


end;;






module Gparser_for_ocaml_language=struct

(*

#use"GParser/gparser_for_ocaml_language.ml";;

*)

let double_semicolon=";"^";";;

let prsr_for_comment=
  Gparser.Comment ("(*","*)","\"","\"");;


let prsr_for_sharp_comment=Gparser.Enclosure ("\n#","\n");;

let prsr_for_space=Gparser.Constant " ";;
let prsr_for_tab=Gparser.Constant "\t";;


let prsr_for_space_or_tab=Gparser.Disjunction [prsr_for_space;prsr_for_tab];;
let prsr_for_linebreak=Gparser.Constant "\n";;
let prsr_for_newline=Gparser.Constant "\012";;
let prsr_for_windows_newline=Gparser.Constant "\r";;
let prsr_for_individual_white=Gparser.Disjunction 
[prsr_for_space;prsr_for_tab;prsr_for_linebreak;prsr_for_newline;prsr_for_windows_newline];;

let prsr_for_inline_white_maybe=Gparser.Star prsr_for_space_or_tab;;
let prsr_for_white_maybe=Gparser.Star prsr_for_individual_white;;
let prsr_for_white=Gparser.One_or_more prsr_for_individual_white;;

let prsr_for_special_sharp=Gparser.Chain
   [
     Gparser.Constant "#";
     prsr_for_inline_white_maybe;
     Gparser.Sample_star "0123456789";
     prsr_for_inline_white_maybe;
     Gparser.Constant "\"";
     Gparser.Sample_star "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ/.";
     Gparser.Constant "\"";
     prsr_for_inline_white_maybe;
   ];;

let prsr_for_uncapitalized_word=Gparser.Chain
   [
     Gparser.Sample_char "abcdefghijklmnopqrstuvwxyz_";
     Gparser.Sample_star "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
   ];;

let prsr_for_capitalized_word=Gparser.Chain
   [
     Gparser.Sample_char "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
     Gparser.Sample_star "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ012356789";
   ];;

let prsr_for_pointing_module=Gparser.Chain
   [
     prsr_for_capitalized_word;
     Gparser.Constant ".";
   ];;

let prsr_for_wholly_lowercase_name=
   Gparser.Chain
   [
     Gparser.Sample_char "abcdefghijklmnopqrstuvwxyz_";
     Gparser.Sample_star "abcdefghijklmnopqrstuvwxyz_";
   ];;


let prsr_for_element_in_uple_in_typedef=
   Gparser.Chain
   [
     Gparser.Constant "'";
      prsr_for_uncapitalized_word; 
     prsr_for_white_maybe; 
     Gparser.Constant ",";
     prsr_for_white_maybe; 
   ];;

let prsr_for_parameters1_in_type=
   Gparser.Chain
   [
     Gparser.Constant "'";
      prsr_for_uncapitalized_word; 
     prsr_for_white_maybe; 
   ];;

let prsr_for_parameters2_in_type=
   Gparser.Chain
   [
     Gparser.Constant "(";
     prsr_for_white_maybe; 
     Gparser.Star(prsr_for_element_in_uple_in_typedef);
     prsr_for_white_maybe; 
     Gparser.Constant "'";
     prsr_for_uncapitalized_word; 
     prsr_for_white_maybe; 
     Gparser.Constant ")";
     prsr_for_white_maybe; 
   ];;

   

let prsr_for_parameters_in_type=
   Gparser.Disjunction
   [
     prsr_for_parameters1_in_type;
     prsr_for_parameters2_in_type;
   ];;

let prsr_for_rec_followed_by_white=Gparser.Chain
   [
     Gparser.Optional(Gparser.Constant "rec");
     prsr_for_white;
   ];;
  

module Private=struct
let list_for_value_making=
   [
     Gparser.Constant "let";
     prsr_for_white;
     Gparser.Optional(prsr_for_rec_followed_by_white);
     prsr_for_uncapitalized_word;
     prsr_for_white_maybe;
     Gparser.Enclosure ("","=");
     Gparser.Enclosure ("",double_semicolon);
   ];;
  
end;;

let index_for_name_in_value_parser=Listennou.find_index
   prsr_for_uncapitalized_word Private.list_for_value_making;;

let index_for_content_in_value_parser=Listennou.find_index
   (Gparser.Enclosure ("",double_semicolon)) Private.list_for_value_making;; 
   

let prsr_for_value_making=Gparser.Detailed_chain
   Private.list_for_value_making;;

let prsr_for_type_making=Gparser.Detailed_chain
   [
     Gparser.Constant "type";
     prsr_for_white;
     Gparser.Optional(prsr_for_parameters_in_type);
     prsr_for_uncapitalized_word;
     prsr_for_white_maybe;
     Gparser.Enclosure ("","=");
     Gparser.Enclosure ("",double_semicolon);
   ];;



let prsr_for_exception_making=Gparser.Detailed_chain
     [
     Gparser.Constant "exception";
     prsr_for_white;
     prsr_for_capitalized_word;
     Gparser.Enclosure ("",double_semicolon);
   ];;

let prsr_for_module_opener=
   Gparser.Detailed_chain
   [
     Gparser.Constant "module";
     prsr_for_white;
     prsr_for_capitalized_word;
     prsr_for_white_maybe;
     Gparser.Constant "=";
     prsr_for_white_maybe;
     Gparser.Constant "struct";
   ];;

let prsr_for_module_closer=
   Gparser.Chain
   [
     Gparser.Constant "end";
     prsr_for_white_maybe;
     Gparser.Constant double_semicolon;
   ];;

let prsr_for_module_inclusion=
   Gparser.Detailed_chain
   [
     Gparser.Constant "include ";
     prsr_for_white_maybe;
     prsr_for_capitalized_word;
     prsr_for_white_maybe;
     Gparser.Constant double_semicolon;
   ];;

let prsr_for_special_names=
   Gparser.Disjunction
     [
       Gparser.Constant "add_to_vvv ";
       Gparser.Constant "add_data ";
       Gparser.Constant "add_data\n";
       Gparser.Constant "add_shortcut ";
       Gparser.Constant "define_precedence_set ";
       Gparser.Constant "get_name_for_set ";
     ];;   
   
let prsr_for_specialities=Gparser.Chain
   [
     prsr_for_special_names;
     Gparser.Enclosure ("",double_semicolon);
   ];;   

let index_for_value=1;;
let index_for_type=2;;
let index_for_exception=3;;
let index_for_comment=4;;
let index_for_sharp_comment=5;;
let index_for_special_sharp=6;;
let index_for_module_opener=7;;
let index_for_module_closer=8;;
let index_for_module_inclusion=9;;
let index_for_specialities=10;;
let index_for_white=11;;


let elt_prsr=Gparser.Disjunction 
  (
     List_with_indices.list_with_indices
     [
       index_for_value           ,prsr_for_value_making;
       index_for_type            ,prsr_for_type_making;
       index_for_exception       ,prsr_for_exception_making;
       index_for_comment         ,prsr_for_comment;
       index_for_sharp_comment   ,prsr_for_sharp_comment;
       index_for_special_sharp   ,prsr_for_special_sharp;
       index_for_module_opener   ,prsr_for_module_opener;
       index_for_module_closer   ,prsr_for_module_closer;
       index_for_module_inclusion,prsr_for_module_inclusion;
       index_for_specialities    ,prsr_for_specialities;
       index_for_white           ,prsr_for_white;
     ]
   )
;;


let main_prsr=
   Gparser.Detailed_star elt_prsr;;



   


end;;






module Gparser_fun=struct

(*

#use"GParser/hparser_fun.ml";;

*)

type t=(string->int->(Gparser_result.t option));;


end;;






module Gparser_ocaml_comment=struct

(*

#use"GParser/kparser_ocaml_comment.ml";;

*)

module Private=struct
type mistletoe={
     comment_opener:string;
     comment_closer:string;
     quote_opener:string;
     quote_closer:string;
     processed_argument: string;
     initial_index: int;
};;

type walker={
    current_index : int;
    current_depth : int;
    quote_mode : bool;
    answer : Gparser_result.t option;
    length_of_preceding_backslash_wall :int;
};;

let update_backslash_wall_length (m,wlkr)=
if (Strung.get m.processed_argument wlkr.current_index)='\\'
then (wlkr.length_of_preceding_backslash_wall)+1
else 0;;


let new_walker_in_first_case_in_hwd (m,wlkr)=
   if Substring.is_a_substring_located_at
     m.quote_opener m.processed_argument wlkr.current_index
   then {
        	current_index =wlkr.current_index+(String.length m.quote_opener);
            current_depth =wlkr.current_depth;
            quote_mode=true;
            answer =None;
            length_of_preceding_backslash_wall=0;
        }
   else
   if Substring.is_a_substring_located_at m.comment_opener 
        m.processed_argument wlkr.current_index
   then {
        	current_index =wlkr.current_index+(String.length m.comment_opener);
            current_depth =wlkr.current_depth+1;
            quote_mode=false;
            answer =None;
            length_of_preceding_backslash_wall=0;
        }
   else   
   if not(Substring.is_a_substring_located_at m.comment_closer 
      m.processed_argument wlkr.current_index)
   then {
        	current_index =wlkr.current_index+1;
            current_depth =wlkr.current_depth;
            quote_mode=false;
            answer =None;
            length_of_preceding_backslash_wall=
               update_backslash_wall_length(m,wlkr);
        }
   else    
   if wlkr.current_depth>1
   then {
        	current_index =wlkr.current_index+(String.length m.comment_closer);
            current_depth =wlkr.current_depth-1;
            quote_mode=false;
            answer =None;
            length_of_preceding_backslash_wall=0;
        }
   else  
   let j1=wlkr.current_index+(String.length m.comment_closer) in
   let res=Gparser_result.veil
               (m.initial_index,j1-1)
               []
               j1
               None        in
        {
        	current_index =wlkr.current_index+(String.length m.comment_closer);
            current_depth =wlkr.current_depth-1;
            quote_mode=false;
            answer =Some(res);
            length_of_preceding_backslash_wall=0;
        };;
   

let first_case_in_hwd 
   (m,wlkr)=(m,new_walker_in_first_case_in_hwd (m,wlkr));;
  
let second_case_in_hwd (m,wlkr)=
  if (Substring.is_a_substring_located_at m.quote_closer
      m.processed_argument wlkr.current_index)
     &&
     ((wlkr.length_of_preceding_backslash_wall mod 2)=0) 
  then (m,{
        	current_index =wlkr.current_index+(String.length m.quote_closer);
            current_depth =wlkr.current_depth;
            quote_mode=false;
            answer =None;
            length_of_preceding_backslash_wall=0;
        })
  else (m,{
        	current_index =wlkr.current_index+1;
            current_depth =wlkr.current_depth;
            quote_mode=true;
            answer =None;
            length_of_preceding_backslash_wall=
               update_backslash_wall_length(m,wlkr);
        });;

let pusher_for_hwd w=
  let (m,wlkr)=w in
  if wlkr.answer=None
  then 
       (
         if not(wlkr.quote_mode)
         then first_case_in_hwd w
         else second_case_in_hwd w       
        )
  else w;;  
   
let rec iterator_for_main_prsr w=
   let (m,wlkr)=w in
   if wlkr.answer<>None
   then wlkr.answer
   else
   if wlkr.current_index>(String.length m.processed_argument)
   then None
   else iterator_for_main_prsr (pusher_for_hwd w);; 
  
let starter_for_main_prsr 
(comment_opener,comment_closer) 
 (quote_opener,quote_closer) s i=
  (
    {
     comment_opener=comment_opener;
     comment_closer=comment_closer;
     quote_opener=quote_opener;
     quote_closer=quote_closer;
     processed_argument=s;
     initial_index=i;
    }
  ,
    {
      current_index =i+(String.length comment_opener);
      current_depth =1;
      quote_mode=false;
      answer =None;
      length_of_preceding_backslash_wall=0;
    }
  );;
end;;  

let main_prsr
   (comment_opener,comment_closer)
     (quote_opener,quote_closer)=
   let rec tempf=(fun s i->
        if not(Substring.is_a_substring_located_at comment_opener s i)
        then None 
        else 
          
          Private.iterator_for_main_prsr 
         (Private.starter_for_main_prsr (comment_opener,comment_closer) 
            (quote_opener,quote_closer) s i)
   ) in
   (tempf:Gparser_fun.t);;      
      
(*

main_prsr ("(*","*)") ("\"","\"") "(* Bye \"*)\" bye bird *)456" 1;;
main_prsr ("(*","*)") ("\"","\"") "(* Bye \"uu\" bye bird *)456" 1;;
main_prsr ("(*","*)") ("\"","\"") "(* Bye \"uu\\\" *) \"  *)234" 1;;


*)



         
   


end;;






module Gparser_house_with_doors=struct

(*

#use"GParser/kparser_house_with_doors.ml";;

*)

module Private=struct
type mistletoe={
     main_opener:string;
     main_closer:string;
     other_enclosers: (string*string) list;
     processed_argument: string;
     initial_index: int;
};;

type walker={
    current_index : int;
    current_depth : int;
    awaited_closer : string option;
    answer : Gparser_result.t option;
};;


let new_walker_in_first_case_in_hwd (m,wlkr)=
   let opt1=Option.seek(fun (opener,closer)->
     Substring.is_a_substring_located_at opener 
        m.processed_argument wlkr.current_index
   ) m.other_enclosers in
   if opt1<>None
   then let (op1,cl1)=Option.unpack opt1 in
        {
        	current_index =wlkr.current_index+(String.length op1);
            current_depth =wlkr.current_depth;
            awaited_closer=Some(cl1);
            answer =None;
        }
   else
   if Substring.is_a_substring_located_at m.main_opener 
        m.processed_argument wlkr.current_index
   then {
        	current_index =wlkr.current_index+(String.length m.main_opener);
            current_depth =wlkr.current_depth+1;
            awaited_closer=None;
            answer =None;
        }
   else   
   if not(Substring.is_a_substring_located_at m.main_closer 
      m.processed_argument wlkr.current_index)
   then {
        	current_index =wlkr.current_index+1;
            current_depth =wlkr.current_depth;
            awaited_closer=None;
            answer =None;
        }
   else    
   if wlkr.current_depth>1
   then {
        	current_index =wlkr.current_index+(String.length m.main_closer);
            current_depth =wlkr.current_depth-1;
            awaited_closer=None;
            answer =None;
        }
   else  
   let j1=wlkr.current_index+(String.length m.main_closer) in
   let res=Gparser_result.veil
               (m.initial_index,j1-1)
               []
               j1
               None        in
        {
        	current_index =wlkr.current_index+(String.length m.main_closer);
            current_depth =wlkr.current_depth-1;
            awaited_closer=None;
            answer =Some(res);
        };;
   

let first_case_in_hwd 
   (m,wlkr)=(m,new_walker_in_first_case_in_hwd (m,wlkr));;
  
let second_case_in_hwd (m,wlkr)=
  let rparen=Option.unpack wlkr.awaited_closer in
  if Substring.is_a_substring_located_at rparen 
      m.processed_argument wlkr.current_index
  then (m,{
        	current_index =wlkr.current_index+(String.length rparen);
            current_depth =wlkr.current_depth;
            awaited_closer=None;
            answer =None;
        })
  else (m,{
        	current_index =wlkr.current_index+1;
            current_depth =wlkr.current_depth;
            awaited_closer=wlkr.awaited_closer;
            answer =None;
        });;

let pusher_for_hwd w=
  let (m,wlkr)=w in
  if wlkr.answer=None
  then 
       (
         if wlkr.awaited_closer=None
         then first_case_in_hwd w
         else second_case_in_hwd w       
        )
  else w;;  
   
let rec iterator_for_hwd w=
   let (m,wlkr)=w in
   if wlkr.answer<>None
   then wlkr.answer
   else
   if wlkr.current_index>(String.length m.processed_argument)
   then None
   else iterator_for_hwd (pusher_for_hwd w);; 
  
let starter_for_hwd (main_opener,main_closer) other_enclosers s i=
  (
    {
     main_opener=main_opener;
     main_closer=main_closer;
     other_enclosers=other_enclosers;
     processed_argument=s;
     initial_index=i;
    }
  ,
    {
      current_index =i+(String.length main_opener);
      current_depth =1;
      awaited_closer=None;
      answer =None;
    }
  );;
end;;  

let hwd
   (main_opener,main_closer)
     other_enclosers=
   let rec tempf=(fun s i->
        if not(Substring.is_a_substring_located_at main_opener s i)
        then None 
        else 
          
          Private.iterator_for_hwd 
         (Private.starter_for_hwd (main_opener,main_closer) other_enclosers s i)
   ) in
   (tempf:Gparser_fun.t);;      
      
(*

hwd ("(*","*)") ["\"","\""] "(* Bye \"*)\" bye bird *)456" 1;;

*)



         
   


end;;






module Gparser_apply=struct

(*

#use"GParser/gparser_apply.ml";;

*)

module Private=struct

let enclosure (left_encloser,right_encloser)=
   let tempf=(fun s i1->
   if (not(Substring.is_a_substring_located_at left_encloser s i1))
   then None
   else 
   let i2=i1+(String.length left_encloser) in
   let i3=Substring.leftmost_index_of_in_from right_encloser s i2 in
   if i3<1
   then None 
   else
   let i4=i3+(String.length right_encloser)-1 in
   let res= Gparser_result.veil
               (i1,i4)
               [i2,i3-1]
               (i4+1)
               None in
   Some(res)) in
   (tempf: Gparser_fun.t);;
   
let constant t=
   let tempf=(fun s i1->
   if (not(Substring.is_a_substring_located_at t s i1))
   then None
   else 
   let i2=i1+(String.length t) in
   let res= Gparser_result.veil
               (i1,i2-1)
               []
               i2
               None in
   Some(res)) in
   (tempf: Gparser_fun.t);;


let footless_constant t=
   let tempf=(fun s i1->
   if (not(Substring.is_a_substring_located_at t s i1))
   then None
   else 
   let i2=i1+(String.length t) in
   let res= Gparser_result.veil
               (i1,i2-1)
               []
               (i2-1)
               None in
   Some(res)) in
   (tempf:Gparser_fun.t);;

let sample_char t=
   let lc=Strung.explode t in
   let tempf=(fun s i->
        let c=Strung.get s i in
        if List.mem c lc
        then Some(Gparser_result.veil
               (i,i)
               []
               (i+1)
               None)
        else None) in
   (tempf:Gparser_fun.t);;

let sample_neg t=
   let lc=Strung.explode t in
   let tempf=(fun s i->
        let c=Strung.get s i in
        if not(List.mem c lc)
        then Some(Gparser_result.veil
               (i,i)
               []
               (i+1)
               None)
        else None) in
   (tempf:Gparser_fun.t);;

let sample_star t=
   let lc=Strung.explode t in
   let tempf=(fun s i1->
        let j=Strung.finder (fun c->not(List.mem c lc)) s i1 in
        let better_j=(if j<1 then (String.length s)+1 else j) in
        let res=Gparser_result.veil
               (i1,better_j-1)
               []
               better_j
               None in
   Some(res)) in
   (tempf:Gparser_fun.t);;

let sample_negstar t=
   let lc=Strung.explode t in
   let tempf=(fun s i1->
        let j=Strung.finder (fun c->List.mem c lc) s i1 in
        let better_j=(if j<1 then (String.length s)+1 else j) in
        let res=Gparser_result.veil
               (i1,better_j-1)
               []
               better_j
               None in
   Some(res)) in
   (tempf:Gparser_fun.t);;

let sample_plus t=
   let lc=Strung.explode t in
   let tempf=(fun s i1->
        if i1>(String.length s) then None else
        if (not(List.mem (Strung.get s i1 ) lc)) then None else
        let j=Strung.finder (fun c->not(List.mem c lc)) s i1 in
        let better_j=(if j<1 then (String.length s)+1 else j) in
        let res=Gparser_result.veil
               (i1,better_j-1)
               []
               better_j
               None in
   Some(res)) in
   (tempf:Gparser_fun.t);;
   

let race (continuer,finalizer)=
   let rec tempf=(fun (s,i1,k)->
        if k>(String.length s)
        then None
        else
        if Substring.is_a_substring_located_at continuer s k
        then tempf(s,i1,k+(String.length continuer))
        else
        if (not(Substring.is_a_substring_located_at finalizer s k))
        then tempf(s,i1,k+1)
        else
        let j1=k+(String.length finalizer) in
        let res=Gparser_result.veil
               (i1,j1-1)
               []
               (j1-1)
               None in
        Some(res)) in
   ((fun s i->tempf(s,i,i)):Gparser_fun.t);;   
      
let house_with_doors=Gparser_house_with_doors.hwd;;


type chain_artefact=
     Usual of (int * int) list * Gparser_fun.t list * bytes * int * int 
    |Result_found of Gparser_result.t
    |Failure_found;;

let chain l=
  let main_f=
  	(fun s i->
   		let rec tempf=
   		(
         	fun (imp_ranges,da_ober,s,i0,k)->
      		match da_ober with
      		[]->Some(
           		    	Gparser_result.veil
               			(i0,k-1)
               			imp_ranges
               			k
               			None
          			)
       		|prsr::rest->   
         		(
           			match prsr s k with
            		None->None
           		  |Some(res)->tempf(
           		       imp_ranges@(Gparser_result.important_ranges res),
                       rest,s,i0,Gparser_result.final_cursor_position res)
                )
         )  
    in tempf([],l,s,i,i)
    ) in
  (main_f:Gparser_fun.t);;

let detailed_chain l=
  let main_f=
  	(fun s i->
   		let rec tempf=
   		(
         	fun (imp_ranges,da_ober,s,i0,k)->
      		match da_ober with
      		[]->Some(
           		    	Gparser_result.veil
               			(i0,k-1)
               			(List.rev imp_ranges)
               			k
               			None
          			)
       		|prsr::rest->   
         		(
           			match prsr s k with
            		None->None
           		  |Some(res)->tempf(
           		       (Gparser_result.whole_range res)::imp_ranges,
                       rest,s,i0,Gparser_result.final_cursor_position res)
                )
         )  
    in tempf([],l,s,i,i)
    ) in
  (main_f:Gparser_fun.t);;

let debugful_detailed_chain l=
  let main_f=
  	(fun s i->
   		let rec tempf=
   		(
         	fun (imp_ranges,da_ober,s,i0,k,opt)->
      		match da_ober with
      		[]->let sol=Some(
           		    	Gparser_result.veil
               			(i0,k-1)
               			(List.rev imp_ranges)
               			k
               			None
          			) in
          	     (imp_ranges,da_ober,s,i0,k,sol) 		
       		|prsr::rest->   
         		(
           			match prsr s k with
            		None->(imp_ranges,da_ober,s,i0,k,opt)
           		  |Some(res)->tempf(
           		       (Gparser_result.whole_range res)::imp_ranges,
                       rest,s,i0,Gparser_result.final_cursor_position res,None)
                )
         )  
    in tempf([],l,s,i,i,None)
    ) in
  main_f;;

let disjunction l=
   let indexed_l=Ennig.index_everything l in   
   let rec tempf=(fun
   (da_ober,s,i0)->
      match da_ober with
      []->None 
      |(j,prsr)::rest->
         (
           match prsr s i0 with
             None->tempf(rest,s,i0)
           |Some(res)->
          Some(
             Gparser_result.veil
               (Gparser_result.whole_range res)
               (Gparser_result.important_ranges res)
               (Gparser_result.final_cursor_position res)
               (Some j)
           )
         )   
   ) in
   ((fun s i->tempf (indexed_l,s,i)):Gparser_fun.t);;

let star prsr=
   let rec tempf=(fun
   (imp_ranges,s,i0,k)->
      match prsr s k with
       None->Some(
             Gparser_result.veil
               (i0,k-1)
               (imp_ranges)
               k
               None
            )
      |Some(res)->tempf(imp_ranges@(Gparser_result.important_ranges res),
                       s,i0,Gparser_result.final_cursor_position res)
   
   ) in
   ((fun s i->tempf ([],s,i,i)):Gparser_fun.t);;

let detailed_star prsr=
   let rec tempf=(fun
   (imp_ranges,s,i0,k)->
      match prsr s k with
       None->Some(
             Gparser_result.veil
               (i0,k-1)
               (List.rev(imp_ranges))
               k
               None
            )
      |Some(res)->tempf((Gparser_result.whole_range res)::imp_ranges,
                       s,i0,Gparser_result.final_cursor_position res)
   
   ) in
   ((fun s i->tempf ([],s,i,i)):Gparser_fun.t);;   
   
   
let one_or_more prsr=chain [prsr;star prsr];;

let optional prsr=
   let rec tempf=(fun s i->
      match prsr s i with
       Some(res)->Some(
            Gparser_result.veil
               (Gparser_result.whole_range res)
               (Gparser_result.important_ranges res)
               (Gparser_result.final_cursor_position res)
               None
            )
      |None->Some(
            Gparser_result.veil
               (i,i-1)
               []
               i
               None
            )
   
   ) in
   (tempf:Gparser_fun.t);;


let recoiling_ending x y=
   let tempf=(fun s i->
      match x s i with
       None->None
      |Some(res)->
                  
                  let j=Gparser_result.final_cursor_position res in
                  if y s j=None then None else
                  Some(
                  Gparser_result.veil
                  (i,j-1)
                  (Gparser_result.important_ranges res)
                  j
                  None
                  )
   ) in
   (tempf:Gparser_fun.t);;
     
let rec apply=function        
     Gparser.Constant(s)->constant s
    |Gparser.Enclosure(s1,s2)->enclosure (s1,s2)
    |Gparser.Footless_constant(s)->footless_constant s
    |Gparser.Sample_char(s)->sample_char s
    |Gparser.Sample_neg(s)->sample_neg s
    |Gparser.Sample_star(s)->sample_star s
    |Gparser.Sample_negstar(s)->sample_negstar s
    |Gparser.Sample_plus(s)->sample_plus s
    |Gparser.Race(s1,s2)->race(s1,s2)
    |Gparser.Comment(s1,s2,s3,s4)->Gparser_ocaml_comment.main_prsr(s1,s2)(s3,s4)
    |Gparser.House_with_doors(s1,s2,l)->house_with_doors (s1,s2) l
    |Gparser.Chain(l)->chain(Image.image apply l)
    |Gparser.Disjunction(l)->disjunction(Image.image apply l)
    |Gparser.Star(x)->star(apply x)
    |Gparser.Detailed_star(x)->detailed_star(apply x)
    |Gparser.One_or_more(x)->one_or_more(apply x)
    |Gparser.Optional(x)->optional(apply x)
    |Gparser.Recoiling_ending(x,y)->recoiling_ending (apply x) (apply y)
    |Gparser.Detailed_chain(l)->detailed_chain(Image.image apply l);;
   
end;;   
   
let apply=Private.apply;;   
   
(*


*)   
   


end;;






module Read_ocaml_files=struct

(*

#use"Ocaml_analysis/read_ocaml_files.ml";;

*)

module Private=struct
exception Unreadable of string;;

let accuse_final_excerpt s i=
  let j=min(String.length s)(i+100) in
  raise(Unreadable(Cull_string.interval s i j));;

let read1 s=
  let opt=Gparser_apply.apply Gparser_for_ocaml_language.main_prsr s 1 in
  if opt=None then accuse_final_excerpt s 1 else
  let res=Option.unpack opt in 
  let p=Gparser_result.final_cursor_position res in
  if p<=(String.length s) 
  then accuse_final_excerpt s p
  else 
  let temp1=Gparser_result.important_ranges res in
  Image.image (fun (i,j)->
    let opt=Gparser_apply.apply Gparser_for_ocaml_language.elt_prsr s i in
    let res=Option.unpack opt in
    ((i,j),Option.unpack(Gparser_result.disjunction_index res))
  ) temp1;;
  
let describe_value_item s (i,j)=
     let opt=Gparser_apply.apply Gparser_for_ocaml_language.prsr_for_value_making s i in
     let res=Option.unpack opt in
     let (i1,j1)=List.nth(Gparser_result.important_ranges res) 
          (Gparser_for_ocaml_language.index_for_name_in_value_parser-1)
     and (i2,j2)=List.nth(Gparser_result.important_ranges res) 
          (Gparser_for_ocaml_language.index_for_content_in_value_parser-1) 
     and (i3,j3)=Gparser_result.whole_range res in
       Ocaml_gsyntax_item.make
          Ocaml_gsyntax_category.Value
          (Cull_string.interval s i1 j1)
          (i1,j1)
          (Cull_string.interval s i3 j3)
          (* the -2 of because of the 2 characters in the double semicolon *)
          (Cull_string.interval s i2 (j2-2))
          (i2,j2-2)
          false;;

let describe_type_item s (i,j)=
     let opt=Gparser_apply.apply Gparser_for_ocaml_language.prsr_for_type_making s i in
     let res=Option.unpack opt in
     let (i1,j1)=List.nth(Gparser_result.important_ranges res) 3
     and (i2,j2)=List.nth(Gparser_result.important_ranges res) 6 
     and (i3,j3)=Gparser_result.whole_range res in
       Ocaml_gsyntax_item.make
          Ocaml_gsyntax_category.Type
          (Cull_string.interval s i1 j1)
          (i1,j1)
          (Cull_string.interval s i3 j3)
          (* the -2 of because of the 2 characters in the double semicolon *)
          (Cull_string.interval s i2 (j2-2))
          (i2,j2-2)
          false;;

let describe_exception_item s (i,j)=
     let opt=Gparser_apply.apply Gparser_for_ocaml_language.prsr_for_exception_making s i in
     let res=Option.unpack opt in
     let (i1,j1)=List.nth(Gparser_result.important_ranges res) 2
     and (i2,j2)=List.nth(Gparser_result.important_ranges res) 3 
     and (i3,j3)=Gparser_result.whole_range res in
       Ocaml_gsyntax_item.make
          Ocaml_gsyntax_category.Exception
          (Cull_string.interval s i1 j1)
          (i1,j1)
          (Cull_string.interval s i3 j3)
          (* the -2 of because of the 2 characters in the double semicolon *)
          (Cull_string.interval s i2 (j2-2))
          (i2,j2-2)
          false;;

let describe_module_opener_item s (i,j)=
     let opt=Gparser_apply.apply Gparser_for_ocaml_language.prsr_for_module_opener s i in
     let res=Option.unpack opt in
     let (i1,j1)=List.nth(Gparser_result.important_ranges res) 2
     and (i3,j3)=Gparser_result.whole_range res in 
       Ocaml_gsyntax_item.make
          Ocaml_gsyntax_category.Module_opener
          (Cull_string.interval s i1 j1)
          (i1,j1)
          (Cull_string.interval s i3 j3)
          ""
          (0,0)
          false;;


let describe_module_closer_item=
       Ocaml_gsyntax_item.make
          Ocaml_gsyntax_category.Module_closer
          ""
          (0,0)
          ""
          ""
          (0,0)
          false;;


let describe_module_inclusion_item s (i,j)=
     let opt=Gparser_apply.apply Gparser_for_ocaml_language.prsr_for_module_inclusion s i in
     let res=Option.unpack opt in
     let (i1,j1)=List.nth(Gparser_result.important_ranges res) 2 
     and (i3,j3)=Gparser_result.whole_range res in 
       Ocaml_gsyntax_item.make
          Ocaml_gsyntax_category.Module_inclusion
          (Cull_string.interval s i1 j1)
          (i1,j1)
          (Cull_string.interval s i3 j3)
          ""
          (0,0)
          false;;
          
 let describe_item s ((i,j),idx)=
   if idx=Gparser_for_ocaml_language.index_for_value
   then Some(describe_value_item s (i,j))
   else
   if idx=Gparser_for_ocaml_language.index_for_type
   then Some(describe_type_item s (i,j))
   else
   if idx=Gparser_for_ocaml_language.index_for_exception
   then Some(describe_exception_item s (i,j))
   else
   if idx=Gparser_for_ocaml_language.index_for_module_opener
   then Some(describe_module_opener_item s (i,j))
   else
   if idx=Gparser_for_ocaml_language.index_for_module_closer
   then Some(describe_module_closer_item)
   else          
   if idx=Gparser_for_ocaml_language.index_for_module_inclusion
   then Some(describe_module_inclusion_item s (i,j))
   else None;;
   
let read2 s=
   Option.filter_and_unpack (describe_item s) (read1 s);;   
   
let module_inclusion_in_pusher    
   (graet,current_full_namespace,current_names) x=
    let included_module=x.Ocaml_gsyntax_item.name in
  		  let full_namespace=current_full_namespace^"."^included_module in
  		  let maybe_included_items=List.filter(
  		     fun y->let nm_y=y.Ocaml_gsyntax_item.name in
  		     (Substring.begins_with nm_y full_namespace)
  		     ||
  		     (Substring.begins_with nm_y included_module)  
  		  ) graet in 
  		  (* local redifinition has priority over an outside definition *)
  		  let chosen_namespace=(if
  		    List.exists(fun y->
  		      y.Ocaml_gsyntax_item.name=included_module
  		    ) maybe_included_items
  		    then included_module
  		    else full_namespace
  		  ) in
         let included_items=List.filter(
         	fun y->y.Ocaml_gsyntax_item.name=chosen_namespace
         ) maybe_included_items in
         let renamed_included_items=Image.image 
         (Ocaml_gsyntax_item.include_in_new_namespace full_namespace )
         included_items in
         (List.rev_append renamed_included_items graet,current_full_namespace,current_names);;
   
let first_pusher_from_level2_to_level3  
   walker_state x=
   let (graet,current_full_namespace,current_names)=walker_state in
  match x.Ocaml_gsyntax_item.category with
    Ocaml_gsyntax_category.Value                                                                          
  | Ocaml_gsyntax_category.Type
  | Ocaml_gsyntax_category.Exception->
          let new_x=Ocaml_gsyntax_item.prepend_prefix current_full_namespace x in
          (new_x::graet,current_full_namespace,current_names)
  | Ocaml_gsyntax_category.Module_opener->
          let new_name=x.Ocaml_gsyntax_item.name in
          let new_names=current_names@[new_name] in
          let new_full_namespace=String.concat "." new_names in
          (graet,new_full_namespace,new_names)
  | Ocaml_gsyntax_category.Module_closer->
          let new_names=List.rev(List.tl(List.rev(current_names))) in
          let new_full_namespace=String.concat "." new_names in
          (graet,new_full_namespace,new_names)
  | Ocaml_gsyntax_category.Module_inclusion->
         module_inclusion_in_pusher (graet,current_full_namespace,current_names) x;;

exception Pusher23_exn;;

let pusher_from_level2_to_level3 (walker_state,da_ober)=
   match da_ober with
   []->raise(Pusher23_exn)
   |x::peurrest->(first_pusher_from_level2_to_level3 walker_state x,peurrest);;    

         
let rec iterator_from_level2_to_level3 (walker_state,da_ober)=
   if da_ober=[] 
   then let  (graet,_,_)=walker_state in List.rev graet
   else iterator_from_level2_to_level3(pusher_from_level2_to_level3 (walker_state,da_ober));; 


let from_level2_to_level3 data_before (current_module,l)=
    iterator_from_level2_to_level3 
      ((data_before,current_module,Strung.split '.' current_module),l);;

end;;

let read_ocaml_files l_ap=
   let temp1=Image.image( fun ap->
   let s_ap=Absolute_path.to_string ap
   and text=Io.read_whole_file ap in
   let unpointed=Father_and_son.father s_ap '.' in
   let module_name=String.capitalize_ascii (Father_and_son.son unpointed '/') in
   (module_name,Private.read2 text)   
   ) l_ap in 
   List.fold_left Private.from_level2_to_level3 [] temp1;;
   
   
(*

let g1=German_wrapper.data();;
let g2=List.filter Modulesystem_data.ml_present g1;;
let g3=List.flatten (image Modulesystem_data.acolytes g2);;
let g4=List.filter (fun mlx->snd(Mlx_filename.decompose mlx)=Ocaml_ending.ml) g3;;
let g5=image Mlx_filename.to_absolute_path g4;;

let g6=read3 g5;;


let g6=image (fun ap->let s=Io.read_whole_file ap in
  (-(String.length s),(ap,s))
) g5 ;;
let g7=image snd (ofo(Tidel2.diforchan g6));;
let g8=Explicit.image (fun (ap,s)->(ap,read2 s)) g7;;
let g9=Explicit.image (fun (ap,l)->
  from_level2_to_level3 ([],"Moody") l
) g8;;

*)

  
(*  

let s1="let jiving=234  ;;";;
describe_value_item s1 (1,String.length s1);;

let s2="type ('a,'b) sister=('a list)*'b*string;;";;
describe_type_item s2 (1,String.length s2);;

let s3="type sister=(int list)*float*string;;";;
describe_type_item s3 (1,String.length s3);;

let s4="exception Foobar of string*int;;";;
describe_exception_item s4 (1,String.length s4);;

let s5="exception Foobar;;";;
describe_exception_item s5 (1,String.length s5);;

let s6="module  Foobar=struct";;
describe_module_opener_item s6 (1,String.length s6);;

let s7="end\n;;";;
describe_module_opener_item s7 (1,String.length s7);;

let s8="include Leap\n;;";;
describe_module_inclusion_item s8 (1,String.length s8);;
   
*)   
   
     
  

end;;






module Ocaml_library=struct


(* 




#use"Makefile_makers/ocaml_library.ml";;


*)


type t=NumLib |StrLib |UnixLib;;

let correspondances=[NumLib,"num";StrLib,"str";UnixLib,"unix"];;

exception Unknown_lib of string;;

let of_string s=
  try (fst(Option.find (fun (x,y)->y=s) correspondances))
  with _->raise(Unknown_lib(s));;

let to_string lib=snd(Option.find (fun (x,y)->x=lib) correspondances);;  


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
       then Some(Mlx_ended_absolute_path.join name edg)
       else None
  ) Ocaml_ending.all_endings;;
  

let registered_endings dt=
  List.filter (fun edg->
    check_presence edg dt 
  ) Ocaml_ending.all_endings;;

let short_paths dt=Image.image Mlx_ended_absolute_path.short_path (acolytes dt);;
  

let compute_modification_times hm=
  let dir=Half_dressed_module.bundle_main_dir hm in
  Ocaml_ending.exhaustive_uple (fun edg->
    let mlx=Mlx_ended_absolute_path.join hm edg in
    let file=(Directory_name.connectable_to_subpath dir)^(Mlx_ended_absolute_path.to_string mlx) in
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
      then Some(Mlx_ended_absolute_path.join hm edg)
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
   let s_root=Directory_name.connectable_to_subpath(
        Half_dressed_module.bundle_main_dir(name dt)
   ) in
   let dirs=
   (*  
     String.concat(" ")
    (Image.image(fun y->let z=
    Subdirectory.connectable_to_subpath(y) in
     if z="" then "" else "-I "^z )
    dt.needed_directories)
	 *)
   "-I "^s_root^"_build"
  and libs=String.concat(" ")
    (Image.image(fun z->Ocaml_library.file_for_library(z)^extension)
    dt.needed_libraries) in
    String.concat " " ["";dirs;libs;""];;

let needed_dirs_and_libs_for_several is_optimized l_dt=
   let extension=(if is_optimized then ".cmxa" else ".cma") in
   let pre_dirs1=Image.image (fun dt->Tidel.diforchan(dt.needed_directories)) l_dt in
   let pre_dirs2=Ordered.forget_order (Tidel.big_teuzin pre_dirs1) in
   let dirs=String.concat(" ")
    (Image.image(fun y->let z=Subdirectory.connectable_to_subpath(y) in 
    if z="" then "" else "-I "^z )
    pre_dirs2) in
   let pre_libs1=Image.image (fun dt->Tidel.diforchan(dt.needed_libraries)) l_dt in
   let pre_libs2=Ordered.forget_order (Tidel.big_teuzin pre_libs1) in 
   let libs=String.concat(" ")
    (Image.image(fun z->Ocaml_library.file_for_library(z)^extension)
    pre_libs2) in
    String.concat " " ["";dirs;libs;""];;

let principal_mlx x=
   if x.mll_present then Mlx_ended_absolute_path.join x.name Ocaml_ending.mll else
   if x.mly_present then Mlx_ended_absolute_path.join x.name Ocaml_ending.mly else
   if x.ml_present then Mlx_ended_absolute_path.join x.name Ocaml_ending.ml else
   Mlx_ended_absolute_path.join x.name Ocaml_ending.mli;;
   
let principal_path x=Mlx_ended_absolute_path.to_path (principal_mlx x);;  

let ml_path x=Mlx_ended_absolute_path.to_path (Mlx_ended_absolute_path.join x.name Ocaml_ending.ml);;   

let rename_endsubdirectory (old_subdir,new_subdirname) x=
    let ren=Half_dressed_module.rename_endsubdirectory (old_subdir,new_subdirname) 
    and ren_sub=Subdirectory.rename_endsubdirectory (old_subdir,new_subdirname) in
{                                                                 
      name = ren (x.name);
      ml_present = x.ml_present;
      mli_present = x.mli_present;
      mll_present = x.mll_present;
      mly_present = x.mly_present;
      ml_modification_time =  x.ml_modification_time;
      mli_modification_time = x.mli_modification_time;
      mll_modification_time = x.mll_modification_time;
      mly_modification_time = x.mly_modification_time;
      needed_libraries = x.needed_libraries;
      direct_fathers = Image.image ren x.direct_fathers;
      all_ancestors = Image.image ren x.all_ancestors;
      needed_directories = Image.image ren_sub x.needed_directories;
};;

let directories_from_list l=
  let temp2=Image.image (
    fun dt->
       let hm=name dt in
       Half_dressed_module.subdirectory hm
  ) l in
  let temp3=Tidel.diforchan(temp2) in
  Ordered.forget_order temp3;;

  
let industrial_separator1=Industrial_separator.modulesystem_data1;;  
let industrial_separator2=Industrial_separator.modulesystem_data2;;    


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
     Nonblank.make(String.concat industrial_separator2 (Image.image Subdirectory.without_trailing_slash x.needed_directories));
   ];;

let unarchive s=
   let l1=Str.split (Str.regexp_string industrial_separator1) s in
   let v1=Str.split (Str.regexp_string industrial_separator2) (Nonblank.decode(List.nth l1  9))
   and v2=Str.split (Str.regexp_string industrial_separator2) (Nonblank.decode(List.nth l1 10))
   and v3=Str.split (Str.regexp_string industrial_separator2) (Nonblank.decode(List.nth l1 11))
   and v4=Str.split (Str.regexp_string industrial_separator2) (Nonblank.decode(List.nth l1 12)) in
   let hm=Half_dressed_module.unarchive(List.hd l1) in
   let dir=Half_dressed_module.bundle_main_dir hm in
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






module Find_value_definition=struct

(*

#use"Ocaml_analysis/find_value_definition.ml";;

if the string argument has a dot inside it, we interpret it
as a value inside a module.
Otherwise we interpret it as a mere string.

*)

let fvd mdata s=
   if not(String.contains s '.')
   then None
   else
   let j1=String.index(s)('.')+1 in
   let module_name=Cull_string.beginning (j1-1) s in
   let opt=Option.seek (fun md->
   Half_dressed_module.naked_module(Modulesystem_data.name md)=
   Naked_module.of_string(String.uncapitalize_ascii(module_name)) ) mdata in
   if opt=None
   then None
   else
   let md1=Option.unpack opt in
   let hm1=Modulesystem_data.name md1 in
   let ap1=Mlx_ended_absolute_path.to_path(Mlx_ended_absolute_path.join hm1 
   	 Ocaml_ending.Ml) in
   let temp1=Read_ocaml_files.read_ocaml_files [ap1] in	 
   Option.seek (
      fun itm->Ocaml_gsyntax_item.name(itm)=s
   ) temp1;;
   


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
NO_DEPENDENCIES of Mlx_ended_absolute_path.t
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
NO_DEPENDENCIES(mlx)->Mlx_ended_absolute_path.to_string mlx
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

let hm_to_na hm=Naked_module.to_string(Half_dressed_module.naked_module hm);;

let uprooted_filename_for_realized_target tgt=
  match tgt with
  NO_DEPENDENCIES(mlx)->Mlx_ended_absolute_path.to_string mlx
 |ML_FROM_MLL(hm)->(hm_to_na hm)^".ml"
 |ML_FROM_MLY(hm)->(hm_to_na hm)^".ml" 
 |CMI(hm)->"_build/"^(hm_to_na hm)^".cmi"
 |CMO(hm)->"_build/"^(hm_to_na hm)^".cmo"
 |DCMO(hm)->"_build/"^(hm_to_na hm)^".d.cmo"
 |CMA(hm)->"_build/"^(hm_to_na hm)^".cma"
 |CMX(hm)->"_build/"^(hm_to_na hm)^".cmx"
 |EXECUTABLE(hm)->"_build/"^(hm_to_na hm)^".caml_executable"
 |DEBUGGABLE(hm)->"_build/"^(hm_to_na hm)^".caml_debuggable"
 |TOPLEVEL(name,l)->name;;

let test_target_existence dir tgt=
let d=Directory_name.connectable_to_subpath dir in
Sys.file_exists(d^(uprooted_filename_for_realized_target tgt));; 

let path dir tgt=
let d=Directory_name.connectable_to_subpath dir in
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

let has_dependencies=function
NO_DEPENDENCIES(_)->false
|_->true;;

let adhoc_test_for_renaming old_name=function
NO_DEPENDENCIES(mlx)->(Mlx_ended_absolute_path.half_dressed_core mlx)<>old_name
|_->true;;

let main_module=function
NO_DEPENDENCIES(mlx)->Some(Mlx_ended_absolute_path.half_dressed_core mlx)
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


let to_shortened_string =function
NO_DEPENDENCIES(mlx)->
   (*
     we do not shorten those because the makefile will
     see them as file targets, and will need to know
     their precise location in order to know if they are
     up-to-date or not.
   *)
   Mlx_ended_absolute_path.to_string mlx
|ML_FROM_MLL(hm)->(Half_dressed_module.to_shortened_string hm)^".ml"
|ML_FROM_MLY(hm)->(Half_dressed_module.to_shortened_string hm)^".ml" 
|CMI(hm)->(Half_dressed_module.to_shortened_string hm)^".cmi"
|CMO(hm)->(Half_dressed_module.to_shortened_string hm)^".cmo"
|DCMO(hm)->(Half_dressed_module.to_shortened_string hm)^".d.cmo"
|CMA(hm)->(Half_dressed_module.to_shortened_string hm)^".cma"
|CMX(hm)->(Half_dressed_module.to_shortened_string hm)^".cmx"
|EXECUTABLE(hm)->(Half_dressed_module.to_shortened_string hm)^".caml_executable"
|DEBUGGABLE(hm)->(Half_dressed_module.to_shortened_string hm)^".caml_debuggable"
|TOPLEVEL(name,l)->name;;


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
NO_DEPENDENCIES(mlx)->(Mlx_ended_absolute_path.half_dressed_core mlx)=hm0
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
ML_FROM_MLL(hm)->Some(Mlx_ended_absolute_path.join hm Ocaml_ending.ml)
|ML_FROM_MLY(hm)->Some(Mlx_ended_absolute_path.join hm Ocaml_ending.ml)
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



let rename_endsubdirectory (old_subdir,new_subdirname) x=
let on_half_dressed_module=Half_dressed_module.rename_endsubdirectory in
match x with
NO_DEPENDENCIES(mlx)->
no_dependencies(Mlx_ended_absolute_path.rename_endsubdirectory (old_subdir,new_subdirname) mlx)
|ML_FROM_MLL(hm)->ml_from_mll(on_half_dressed_module (old_subdir,new_subdirname) hm)
|ML_FROM_MLY(hm)->ml_from_mly(on_half_dressed_module (old_subdir,new_subdirname) hm)
|CMI(hm)->cmi(on_half_dressed_module (old_subdir,new_subdirname) hm)
|CMO(hm)->cmo(on_half_dressed_module (old_subdir,new_subdirname) hm)
|DCMO(hm)->dcmo(on_half_dressed_module (old_subdir,new_subdirname) hm)
|CMA(hm)->cma(on_half_dressed_module (old_subdir,new_subdirname) hm)
|CMX(hm)->cmx(on_half_dressed_module (old_subdir,new_subdirname) hm)
|EXECUTABLE(hm)->executable(on_half_dressed_module (old_subdir,new_subdirname) hm)
|DEBUGGABLE(hm)->debuggable(on_half_dressed_module (old_subdir,new_subdirname) hm)
|TOPLEVEL(name,l)->
      let new_l=Image.image (on_half_dressed_module (old_subdir,new_subdirname)) l in
      TOPLEVEL(name,new_l);;



let industrial_separator1=Industrial_separator.ocaml_target1;;  
let industrial_separator2=Industrial_separator.ocaml_target2;;    


let prepare_archive=function
NO_DEPENDENCIES(mlx)->["nodep";Mlx_ended_absolute_path.archive mlx]
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
if c="nodep" then NO_DEPENDENCIES(Mlx_ended_absolute_path.unarchive ms) else
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

let cooperation_for_two=
			((fun x y->Ordered.cooperation_for_two cmp x y):>
			 (set->set->set*set*set ));; 		
    
let expand_boolean_algebra=
 ((fun x->Ordered.expand_boolean_algebra cmp x):>(set list->(set list)));; 
 
 


end;;






module Dircopy_diff=struct

(*

#use"dircopy_diff.ml";;

*)

type t={
   recently_deleted : string list;
   recently_changed : string list;
   recently_created : string list;
};;

let recently_deleted x=x.recently_deleted;;
let recently_created x=x.recently_created;;
let recently_changed x=x.recently_changed;;

let veil a b c={
   recently_deleted =Recently_deleted.to_string_list a;
   recently_changed =Recently_changed.to_string_list b;
   recently_created =Recently_created.to_string_list c;
};;

let display x=
   let tempf=(fun msg l->
   "\n"::msg::(Image.image(fun w->"\t\t"^w) l)
   ) in
   let temp1=tempf "Deleted : " (x.recently_deleted)
   and temp2=tempf "Created : " (x.recently_created)
   and temp3=tempf "Changed : " (x.recently_changed) in
   let temp4=String.concat "\n" (temp1@temp2@temp3) in
   (print_string temp4;
    flush stdout);;

module Private=struct

let summarize_short_path s=
   String.capitalize_ascii(Father_and_son.son (Father_and_son.invasive_father s '.') '/');;
 
let summarize_short_path_list l=
    let temp1=Image.image summarize_short_path l in
    Ordered.forget_order(Ordered_string.diforchan temp1);;

end;;

let explain x=
   let tempf=(fun (msg,l)->
     if l=[]
     then None
     else Some(msg^" "^(String.concat "," l)^".")
   ) in
   let temp1=Option.filter_and_unpack tempf
   [
     "Deleted",Private.summarize_short_path_list(x.recently_deleted);
     "Created",Private.summarize_short_path_list(x.recently_created);
     "Modified",Private.summarize_short_path_list(x.recently_changed);
   ] in
   if temp1=[] then "" else
   let temp2=(String.uncapitalize_ascii (List.hd temp1))::(List.tl temp1) in
   String.concat " " temp2;; 
   
let is_empty x=
  (x.recently_deleted,x.recently_created,x.recently_changed)=
   ([],[],[]);;   
   
   
   
   
   
   

end;;






module German_unregister_outside_file=struct


(* 

#use"Country/Germany/german_unregister_outside_file.ml";;


*)

let on_outside_directories (o_files,o_dirs) ap=
  let main_dir=German_constant.root in
  let tempf=(fun ap2->
  	let temp1=Directory_name.cut_beginning main_dir (Absolute_path.to_string ap2) in
  	let s_sdir=Father_and_son.father temp1 '/' in
  	Subdirectory.of_string s_sdir 
  ) in
  let sdir=tempf ap in 
  let new_ofiles=List.filter (fun ap2->ap2<>ap) o_files in
  let new_odirs=(
       if List.exists(fun z->tempf(z)=sdir) new_ofiles
       then o_dirs
       else List.filter (fun sdir2->sdir2<>sdir) o_dirs
  )  in
  (new_ofiles,new_odirs);;     



end;;






module German_directories=struct


(* 

#use"Country/Germany/german_directories.ml";;


*)


let from_data mdata=
  let temp2=Image.image (
    fun dt->
       let hm=Modulesystem_data.name dt in
       Half_dressed_module.subdirectory hm
  ) mdata in
  let temp3=Tidel.diforchan(temp2) in
  Ordered.forget_order temp3;;
 



end;;






module Interesting_modules=struct

(*

#use"interesting_modules.ml";;

*)


let list=
  [
     "Country/Germany/german_pervasives";
     "Creators/small_int_based_rational";
     "Creators/big_int_based_rational";
     "debugger";
     "inclusion";
     "interpolation";
     "legendre_symbol";
     "modularize";
     "Ocaml_analysis/gdecompose_ocaml_text";
     "Optional/simplex_method";
     "optimize_polynomial";
     "Php_analizer/level_one";
     "Php_analizer/Php_syntax_types/php_yuze_modifier";
     "uutf"; (* external module for char recognition *)
     
  ];;



end;;






module Debugged_name=struct

(*

#use"debugger_name.ml";;

*)


let debugger_name="debugged";;



end;;






module Alaskan_data=struct


(* 

#use"Country/Alaska/alaskan_data.ml";;


*)

let all_mlx_files mdata=
  List.flatten
  (Image.image Modulesystem_data.acolytes mdata);; 

let all_mlx_paths mdata=Image.image Mlx_ended_absolute_path.to_absolute_path 
  (all_mlx_files mdata);;  

let all_short_paths mdata=List.flatten(
  Image.image Modulesystem_data.short_paths mdata
);;


let compute_subdirectories_list mdata=
  let temp1=Image.image (
      fun md->
       let hm=Modulesystem_data.name md in
       Subdirectory.without_trailing_slash(Half_dressed_module.subdirectory hm)
  ) mdata in
  let temp2=Ordered_string.diforchan temp1 in
  let temp3=Ordered_string.forget_order temp2 in
  Image.image Subdirectory.of_string temp3;;


let default_toplevel main_toplevel_name mdata=
  let temp2=List.filter Modulesystem_data.is_not_optional mdata in
  let temp3=Image.image Modulesystem_data.name temp2 in
  let temp4=List.filter (fun hm->
     Half_dressed_module.to_string(hm)<>Debugged_name.debugger_name
  ) temp3
  in
  Ocaml_target.toplevel main_toplevel_name temp4;; 
 
let find_module_registration mdata hm=
  Option.seek(fun a->Modulesystem_data.name a=hm) mdata;;   

let default_targets main_toplevel_name mdata=
    let temp1=Image.image Ocaml_target.from_modulesystem_data mdata 
    and temp2=Image.image Modulesystem_data.name mdata in
    let temp3=List.flatten temp1  in
    temp3@[Ocaml_target.toplevel main_toplevel_name temp2]
    ;;

let industrial_separator=Industrial_separator.alaskan_or_german_data;;    

let archive mdata=
       Nonblank.make(String.concat industrial_separator 
       (Image.image Modulesystem_data.archive mdata));;
      
  
let unarchive s=
     let v1=Str.split (Str.regexp_string industrial_separator) (Nonblank.decode(s)) in
     Image.image Modulesystem_data.unarchive v1;;
       
  
      

end;;






module German_data=struct


(* 

#use"Country/Germany/german_data.ml";;

*)



let filedata_selector mdata ending x=List.filter 
   (Modulesystem_data.check_presence ending) mdata;;
 
  
let inside_files mdata=Image.image Mlx_ended_absolute_path.to_path 
(Alaskan_data.all_mlx_files mdata);;  

 let see_if_file_is_registered mdata mlx=
    let hm=Mlx_ended_absolute_path.half_dressed_core mlx
    and edg=Mlx_ended_absolute_path.ending mlx in  
    match Option.seek (fun a->Modulesystem_data.name a=hm) mdata with
    None->false
    |Some(dt)->Modulesystem_data.check_presence edg dt;;
 
let check_presences mdata hm=
    match Option.seek (fun a->Modulesystem_data.name a=hm) mdata with
    None->Ocaml_ending.exhaustive_uple (fun _->false)
    |Some(dt)->Ocaml_ending.exhaustive_uple 
     (fun edg->Modulesystem_data.check_presence edg dt);;
 
 let acolytes mdata hm=
    match Option.seek (fun a->Modulesystem_data.name a=hm) 
      mdata with
     None->[]
    |Some(dt)->Modulesystem_data.acolytes dt;;    
   
 
 let outdated_files mdata=
   let temp1=Image.image Modulesystem_data.outdated_acolytes mdata in
   List.flatten temp1;;
 

   
 let descendants mdata names=
    let temp1=List.filter(
      fun dt->List.exists (fun t->List.mem t names) 
        (Modulesystem_data.all_ancestors dt)
    ) mdata in
    temp1;;
    
let optionality_partition mdata=
  let (before,core,after)=Three_parts.select_center_element 
    Modulesystem_data.is_optional mdata in
  (before,Option.add_perhaps core after);;    

exception Deletability_issue of Mlx_ended_absolute_path.t;;

let is_deletable mdata mlxfile=
   let hm=Mlx_ended_absolute_path.half_dressed_core mlxfile in
   if (descendants mdata [hm])<>[]
   then false
   else let edg=Mlx_ended_absolute_path.ending mlxfile in
        if List.mem edg [Ocaml_ending.ml;Ocaml_ending.mli]
        then true
        else 
        if List.mem edg [Ocaml_ending.mll;Ocaml_ending.mly]
        then let opt=Option.seek (fun a->Modulesystem_data.name a=hm) mdata in
             (
               match opt with
               None->true
               |Some(dt)->not(Modulesystem_data.ml_present dt)
             )
        else raise(Deletability_issue(mlxfile));; 
 
let unregistered_mlx_files mdata=
   let temp1=Mlx_ended_absolute_path.complete_ls German_constant.root in
   List.filter (fun mlx->
     not(see_if_file_is_registered mdata mlx)
   ) temp1;;
 
let system_size mdata=List.length(mdata);;
 
exception  Non_registered_module of Half_dressed_module.t;;
 
let above mdata hm=
   match Option.seek(fun dt->Modulesystem_data.name dt=hm) mdata with
    None->raise(Non_registered_module(hm))
   |Some(dt)->Modulesystem_data.all_ancestors dt;;
   
let below mdata hm=
   Option.filter_and_unpack(fun dt->
   if List.mem hm (Modulesystem_data.all_ancestors dt)
   then Some(Modulesystem_data.name dt)
   else None) mdata;;   
 
let directly_below mdata hm=
   Option.filter_and_unpack(fun dt->
   if List.mem hm (Modulesystem_data.direct_fathers dt)
   then Some(Modulesystem_data.name dt)
   else None) mdata;;    


let files_containing_string mdata some_string=
   let temp1=Alaskan_data.all_mlx_paths mdata in
   List.filter (fun ap->Substring.is_a_substring_of 
     some_string (Io.read_whole_file ap)) temp1;;
 


let default_toplevel =Alaskan_data.default_toplevel 
   German_constant.main_toplevel_name;; 
 
let deletable_files mdata=
  let temp2=Image.image Modulesystem_data.name mdata in
  let is_interesting=(fun hm->List.mem 
     (Half_dressed_module.to_string hm) 
     Interesting_modules.list
  ) in
  let temp3=List.filter (
    fun hm->
    (not(is_interesting hm))
    &&
    (List.for_all(fun hm2->not(is_interesting hm2)) (below mdata  hm))
  ) temp2 in
  let (temp4,temp5)=List.partition (fun hm->
    let s_hm=Half_dressed_module.to_string hm in
    not(Substring.begins_with s_hm "Optional/")
   ) temp3 in
  (temp4,temp5);;    

let outdated_interesting_modules mdata=
	List.filter (
       fun s->match Option.seek(
         fun md->Half_dressed_module.to_string(Modulesystem_data.name md)=s
       ) mdata with
       None->true
       |Some(md0)->
         let hm0=Modulesystem_data.name md0 in
        (below mdata hm0)<>[]
    ) Interesting_modules.list;;
  


 
 
 


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
     let name=Half_dressed_module.naked_module hm in
     if List.mem name temp1
     then Some(info)
     else None
   ) in
   Option.filter_and_unpack selecter l;;

let find_needed_data l mlx=
   let fn=Mlx_ended_absolute_path.to_path mlx in
   find_needed_data_for_file l fn;;

let find_needed_names l mlx=
   let temp1=find_needed_data l mlx in
   Image.image Modulesystem_data.name temp1;;

 let find_needed_libraries mlx genealogy=
   let fn=Mlx_ended_absolute_path.to_path mlx in
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
   let s_mlx=Mlx_ended_absolute_path.to_string mlx in
   let temp2=(fun bowl->
       if bowl 
       then let new_subdir=Subdirectory.of_string(Father_and_son.father s_mlx '/') in
            Tidel.singleton(new_subdir)::temp1
       else temp1
   )(String.contains s_mlx '/') in    
   let temp3=Tidel.big_teuzin temp2 in
   Ordered.forget_order temp3;;
 
 let check_presences l hm=
 match Option.seek (fun a->Modulesystem_data.name a=hm) l with
    None->Ocaml_ending.exhaustive_uple (fun _->false)
    |Some(dt)->Ocaml_ending.exhaustive_uple 
     (fun edg->Modulesystem_data.check_presence edg dt);;
 
 
 let complete_info l mlx=
   let (hm,edg)=Mlx_ended_absolute_path.decompose mlx in
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
  let opt=Option.seek(fun a->Modulesystem_data.name a=hm) l in
  let dt=Option.unpack opt in
  let edg=List.hd(Modulesystem_data.registered_endings dt) in
  let mlx=Mlx_ended_absolute_path.join hm edg in
  complete_info l mlx;;
        
    
let quick_update l x=
  let hm=Modulesystem_data.name (x) in
  if (Half_dressed_module.to_string hm)=Debugged_name.debugger_name
  then None
  else
  let new_values=Modulesystem_data.compute_modification_times hm 
  and old_values=Modulesystem_data.modification_times x in
  if old_values=new_values
  then None
  else
  let (n_ml,n_mli,n_mll,n_mly)=new_values in
  let edg=List.hd(Modulesystem_data.registered_endings x) in
  let mlx=Mlx_ended_absolute_path.join hm edg in
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
  then let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
       [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm;Ocaml_target.cmo hm]
  else 
  if Modulesystem_data.mly_present dt
  then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
       [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm;Ocaml_target.cmo hm]
  else
  if Modulesystem_data.ml_present dt
  then 
       let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
       [ml_target;Ocaml_target.cmi hm;Ocaml_target.cmo hm]
  else let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
       [mli_target;Ocaml_target.cmi hm];;  


let debuggable_targets_from_ancestor_data dt=
  let hm=Modulesystem_data.name dt in
  if Modulesystem_data.mll_present dt
  then let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
       [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm;Ocaml_target.dcmo hm]
  else 
  if Modulesystem_data.mly_present dt
  then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
       [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm;Ocaml_target.dcmo hm]
  else
  if Modulesystem_data.ml_present dt
  then 
       let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
       [ml_target;Ocaml_target.cmi hm;Ocaml_target.dcmo hm]
  else let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
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
  then let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
       [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm;Ocaml_target.cmx hm]
  else 
  if Modulesystem_data.mly_present dt
  then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
       [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm;Ocaml_target.cmx hm]
  else
  if Modulesystem_data.ml_present dt
  then 
       let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
       [ml_target;Ocaml_target.cmi hm;Ocaml_target.cmx hm]
  else let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
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
  let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
  [mll_target];;

let immediate_ingredients_for_ml_from_mly hm=
  let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
  [mly_target];;

let immediate_ingredients_for_cmi dt hm=
    if Modulesystem_data.mll_present dt
    then let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
         [mll_target;Ocaml_target.ml_from_mll hm]
    else 
    if Modulesystem_data.mly_present dt
    then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
         [mly_target;Ocaml_target.ml_from_mly hm]
    else
  if Modulesystem_data.mli_present dt
  then let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
       [mli_target]
  else let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
       [ml_target];; 

let immediate_ingredients_for_cmo dt hm=
    if Modulesystem_data.mll_present dt
    then let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
         [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm]
    else 
    if Modulesystem_data.mly_present dt
    then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
         [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm]
    else
  if Modulesystem_data.ml_present dt
  then let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
       [ml_target;Ocaml_target.cmi hm]
  else let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
       [mli_target;Ocaml_target.cmi hm];;  


let immediate_ingredients_for_dcmo=immediate_ingredients_for_cmo;;

let immediate_ingredients_for_cma=immediate_ingredients_for_cmo;;

let immediate_ingredients_for_cmx dt hm=
    if Modulesystem_data.mll_present dt
    then let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
         [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm]
    else 
    if Modulesystem_data.mly_present dt
    then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
         [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm]
    else
  if Modulesystem_data.ml_present dt
  then let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
       [ml_target;Ocaml_target.cmi hm]
  else let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
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
  let mlfile=Mlx_ended_absolute_path.join hm Ocaml_ending.Ml in
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
  let hm=Mlx_ended_absolute_path.half_dressed_core mlx in
  module_dependency_for_ocaml_target mdata [hm] tgt;;

let mlx_list_dependency_for_ocaml_target mdata l_mlx tgt=
 List.exists (fun mlx->mlx_dependency_for_ocaml_target mdata mlx tgt) l_mlx;;


end;;






module Alaskan_force_modification_time=struct


(* 


#use"Country/Alaska/alaskan_force_modification_time.ml";;


*)

exception Non_existent_mtime of Mlx_ended_absolute_path.t;;

let update dir mdata mlx=
   let hm=Mlx_ended_absolute_path.half_dressed_core mlx
   and edg=Mlx_ended_absolute_path.ending mlx in
   let (before,opt,after)=Three_parts.select_center_element  (fun dt->
      Modulesystem_data.name dt=hm) mdata in
   if opt=None
   then raise(Non_existent_mtime(mlx))
   else 
   let dt=Option.unpack opt in
   let file=(Directory_name.connectable_to_subpath dir)^(Mlx_ended_absolute_path.to_string mlx) in
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


let ocamlc="ocamlc  -bin-annot ";;
let ocamlopt="ocamlopt  -bin-annot ";;
let cee=" -c ";;

exception Command_called_on_nodep of Mlx_ended_absolute_path.t;;
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
    let s_hm2=Half_dressed_module.to_shortened_string hm2 in
    Some("_build/"^s_hm2^".cmx")
 |_->None;;

let dcmo_manager=function
 Ocaml_target.DCMO(hm2)->
    let s_hm2=Half_dressed_module.to_shortened_string hm2 in
    Some("_build/"^s_hm2^".d.cmo")
 |_->None;;

let command_for_nodep mlx=[];;

let command_for_ml_from_mll dir hm=
          let s_hm=Half_dressed_module.to_string hm in
          let s_root=Directory_name.connectable_to_subpath dir in
          let s_fhm=s_root^s_hm in
          [
            "ocamllex  -o "^s_fhm^".ml "^s_fhm^".mll";
          ];;
 
let command_for_ml_from_mly dir hm=
          let s_hm=Half_dressed_module.to_string hm in
          let s_root=Directory_name.connectable_to_subpath dir in
          let s_fhm=s_root^s_hm in
          [
            "ocamlyacc "^s_fhm^".mly"
          ];;  

let command_for_cmi dir mdata hm=
          let opt=Alaskan_data.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_cmi(hm)) else 
          let dt=Option.unpack opt in
          let s_root=Directory_name.connectable_to_subpath(dir) in
          let s_hm=Half_dressed_module.to_string hm in
          let s_fhm=s_root^s_hm in
          let ending=(
          if Modulesystem_data.mli_present dt
          then ".mli"
          else ".ml"
          ) in
          let central_cmd=
            "ocamlc  -bin-annot "^
            (Modulesystem_data.needed_dirs_and_libs false dt)^
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
                [
                 "mv "^full_mli^" "^dummy_mli;
                 central_cmd;
                 "mv "^s_fhm^".cm* "^s_root^"_build/";
                 "mv "^dummy_mli^" "^full_mli
                ] 
          else  [
                   central_cmd;
                   "mv "^s_fhm^".cm* "^s_root^"_build/"
                 ];;

let command_for_cmo dir mdata hm=
          let opt=Alaskan_data.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_cmo(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s_root=Directory_name.connectable_to_subpath(dir) in
          let s_fhm=s_root^s_hm in
          let dirs_and_libs=Modulesystem_data.needed_dirs_and_libs false dt in
          [ 
            "ocamlc -bin-annot "^dirs_and_libs^" -o "^s_fhm^".cmo -c "^s_fhm^".ml";
            "mv "^s_fhm^".cm* "^s_root^"_build/"
          ];;

let command_for_dcmo dir mdata hm=
          let opt=Alaskan_data.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_dcmo(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s_root=Directory_name.connectable_to_subpath(dir) in
          let s_fhm=s_root^s_hm in
          let dirs_and_libs=Modulesystem_data.needed_dirs_and_libs false dt in
          [ 
            "ocamlc -bin-annot -g "^dirs_and_libs^" -o "^s_fhm^".d.cmo -c "^s_fhm^".ml";
            "mv "^s_fhm^".d.cm* "^s_root^"_build/"
          ];;

let command_for_cma dir mdata hm=
          let opt=Alaskan_data.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_cma(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s_root=Directory_name.connectable_to_subpath(dir) in
          let s_fhm=s_root^s_hm in
          let dirs_and_libs=Modulesystem_data.needed_dirs_and_libs false dt in
          [ 
            "ocamlopt -bin-annot -a "^dirs_and_libs^" -o "^s_fhm^".cma -c "^s_fhm^".ml";
            "mv "^s_fhm^".cm* "^s_root^"_build/"
          ];;
          
let command_for_cmx dir mdata hm=
          let opt=Alaskan_data.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_cmx(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s_root=Directory_name.connectable_to_subpath(dir) in
          let s_fhm=s_root^s_hm in
          let dirs_and_libs=Modulesystem_data.needed_dirs_and_libs true dt in
          [ 
            "ocamlopt -bin-annot "^dirs_and_libs^" -o "^s_fhm^".cmx -c "^s_fhm^".ml";
            "mv "^s_fhm^".cm* "^s_root^"_build/"
          ];;
          
 

let command_for_executable dir mdata hm=
          let opt=Alaskan_data.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_executable(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s_root=Directory_name.connectable_to_subpath(dir) in
          let s_fhm=s_root^s_hm in
          let temp1=ingr mdata (Ocaml_target.EXECUTABLE(hm)) in
          let temp2=Option.filter_and_unpack cmx_manager temp1 in
          let long_temp2=Image.image (fun t->s_root^t) temp2 in
          let dirs_and_libs=Modulesystem_data.needed_dirs_and_libs true dt in
          [ 
            "ocamlopt -bin-annot "^dirs_and_libs^" -o "^s_fhm^".ocaml_executable "^
                (String.concat " " long_temp2);
            "mv "^s_fhm^".cm* "^s_root^"_build/";
            "mv "^s_fhm^".ocaml_executable "^s_root^"_build/"
          ];;
          
let command_for_debuggable dir mdata hm=
          let opt=Alaskan_data.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_debuggable(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s_root=Directory_name.connectable_to_subpath(dir) in
          let s_fhm=s_root^s_hm in
          let temp1=ingr mdata (Ocaml_target.DEBUGGABLE(hm)) in
          let temp2=Option.filter_and_unpack dcmo_manager temp1 in
          let long_temp2=Image.image (fun t->s_root^t) temp2 in
          let dirs_and_libs=Modulesystem_data.needed_dirs_and_libs false dt in

          [ 
            "ocamlc -bin-annot -g "^dirs_and_libs^" -o "^s_fhm^".ocaml_debuggable "^
                (String.concat " " long_temp2);
            "mv "^s_fhm^".ocaml_debuggable "^s_root^"_build/"
          ];;          
  
let command_for_toplevel dir mdata name l=
          let temp1=Image.image (fun hm->(hm,Alaskan_data.find_module_registration mdata hm)) l  in
          let temp2=List.filter (fun x->snd(x)=None) temp1 in
          if temp2<>[]
          then let temp3=Image.image fst temp2 in
               raise(Unregistered_modules_in_toplevel(name,temp3))
          else
          let l_dt=Image.image (fun (_,y)->Option.unpack y) temp1 in
          let s_root=Directory_name.connectable_to_subpath(dir) in
          let long_temp4=Image.image (fun fd->
             let hm=Modulesystem_data.name fd in
             let s_hm=(Half_dressed_module.to_string hm) in
             let short_s_hm=Father_and_son.son s_hm '/' in
             if Modulesystem_data.ml_present fd 
             then s_root^"_build/"^short_s_hm^".cmo"
             else " "
          ) l_dt in 
          let long_s_lhm=String.concat " " long_temp4 in
          let dirs_and_libs=Modulesystem_data.needed_dirs_and_libs_for_several false l_dt in
          [
          "ocamlmktop "^dirs_and_libs^" -o "^s_root^name^" "^long_s_lhm^" ";
          "mv "^s_root^name^" "^s_root^"_build/";
          ];;   
 
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
   

 


end;;






module Alaskan_make_ocaml_target=struct

(*

#use"Country/Alaska/alaskan_make_ocaml_target.ml";;

*)



let cmd_for_tgt=Alaskan_command_for_ocaml_target.command_for_ocaml_target;;

let ingr_for_tgt =Alaskan_ingredients_for_ocaml_target.ingredients_for_ocaml_target;;
let ingr_for_top =Alaskan_ingredients_for_ocaml_target.marked_ingredients_for_unprepared_toplevel;;


let is_up_to_date dir tgts tgt=
  if Ocaml_target.is_a_debuggable tgt
  then false
  else 
  if Ocaml_target.test_target_existence dir tgt
  then List.mem tgt tgts
  else false;;

let unit_make dir (bowl,(mdata,tgts)) tgt=
  if (not bowl)
  then (bowl,(mdata,tgts))
  else
  if is_up_to_date dir tgts tgt
  then (true,(mdata,tgts))
  else 
  let temp1=Image.image Unix_command.uc (cmd_for_tgt dir mdata tgt) in 
  if List.for_all (fun i->i=0) temp1
  then let opt_tgt=(if Ocaml_target.is_a_debuggable tgt 
                    then None 
                    else (Some tgt)) in
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



let pusher_for_toplevel dir (successful_ones,to_be_treated,ts)=
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
       let root=Half_dressed_module.bundle_main_dir hm in
       let s_root=Directory_name.connectable_to_subpath root in
  	   let (rejects,remains)=List.partition
       (fun (tgtt,_)->
         Alaskan_ingredients_for_ocaml_target.module_dependency_for_ocaml_target 
         (fst ts) [hm] tgtt
       ) others in
       let _=Image.image (
         fun (tgtt,_)->
         if Ocaml_target.has_dependencies tgtt
         then let s_ap=s_root^"_build/"^
                  (Ocaml_target.to_shortened_string tgtt) in
              let _=Unix_command.uc("rm -f "^s_ap) in
              ()
       ) ((tgt,is_an_ending_or_not)::rejects) in
       (successful_ones,remains,ts2);; 

let rec  iterator_for_toplevel dir (successful_ones,to_be_treated,ts)=
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






module German_unregister_module=struct


(* 


#use"Country/Germany/german_unregister_module.ml";;


*)


 
 exception Non_registered_module of Half_dressed_module.t;;  
 exception Derelict_children of Half_dressed_module.t*(Half_dressed_module.t list);;  

 
let on_monitored_modules mdata hm=
    let desc=German_data.descendants mdata [hm] in
    if desc<>[]
    then let temp1=Image.image Modulesystem_data.name desc in
         raise(Derelict_children(hm,temp1))
    else
    let (before,opt,after)=Three_parts.select_center_element  (fun dt->
      Modulesystem_data.name dt=hm) mdata in
    if opt=None 
    then raise(Non_registered_module(hm))  
    else 
    let acolytes=Modulesystem_data.acolytes(Option.unpack opt) in
    let short_paths=Image.image Mlx_ended_absolute_path.short_path acolytes in
    (before@after,short_paths);;
    
let on_targets (old_mdata,old_tgts) hm=
 let (new_mdata,short_paths)=on_monitored_modules old_mdata hm in
 let new_dirs=German_directories.from_data new_mdata 
 and new_tgts=List.filter (fun tgt->
   match Ocaml_target.main_module tgt with
   None->false |Some(hm2)->hm2<>hm
 ) old_tgts in
 let default_top=(German_data.default_toplevel new_mdata) in
 let (new_mdata2,new_tgts2)=
   snd(Alaskan_make_ocaml_target.make 
       German_constant.root (new_mdata,new_tgts) default_top) in
  ((new_mdata2,new_dirs,new_tgts2),short_paths);;   
  


end;;






module German_unregister_mlx_file=struct


(* 


#use"Country/Germany/german_unregister_mlx_file.ml";;


*)

 exception Non_registered_file of Mlx_ended_absolute_path.t;;  
 exception Abandoned_children of Mlx_ended_absolute_path.t*(Half_dressed_module.t list);;
 

 let on_monitored_modules mdata mlxfile=
    let hm=Mlx_ended_absolute_path.half_dressed_core mlxfile in
    let desc=German_data.descendants mdata [hm] in
    if desc<>[]
    then let temp1=Image.image Modulesystem_data.name desc in
         raise(Abandoned_children(mlxfile,temp1))
    else
    let (before,opt,after)=Three_parts.select_center_element  (fun dt->
      Modulesystem_data.name dt=hm) mdata in
    match opt with
     None->raise(Non_registered_file(mlxfile))
    |Some(dt)->
      let edg=Mlx_ended_absolute_path.ending mlxfile in
      if (not(Modulesystem_data.check_presence edg dt))
      then raise(Non_registered_file(mlxfile))
      else 
      let new_dt=Modulesystem_data.make_absence edg dt in
      if (Modulesystem_data.registered_endings new_dt)=[]
      then before@after
      else before@(new_dt::after);;
    
 let on_targets (old_mdata,old_tgts) mlx=
  let hm=Mlx_ended_absolute_path.half_dressed_core mlx in
  let new_mdata=on_monitored_modules old_mdata mlx in
  let new_dirs=German_directories.from_data new_mdata
  and new_tgts=List.filter (fun tgt->
   	match Ocaml_target.main_module tgt with
   	None->false |Some(hm2)->hm2<>hm
  ) old_tgts in
  let default_top=(German_data.default_toplevel new_mdata) in
  let (new_mdata2,new_tgts2)=
    snd(Alaskan_make_ocaml_target.make
      German_constant.root
     (new_mdata,new_tgts) default_top) in
  (new_mdata2,new_dirs,new_tgts2);;   
  


end;;






module Find_suitable_ending=struct

(*

#use"find_suitable_ending.ml";;

*)

(*

Note that the order in Ocaml_ending.correspondances is important

*)

exception No_suitable_location of Directory_name.t*(Subdirectory.t list)*string;;

let find_file_location dir l_subdir old_x=
  let x=String.uncapitalize_ascii old_x in
  let s_dir=Directory_name.connectable_to_subpath(dir) in
  let original_endings=Ocaml_ending.all_string_endings in
  let endings=(
     if List.exists (fun edg->Substring.ends_with x edg) original_endings
     then [""]
     else original_endings
  ) in
  let temp1=Cartesian.product(l_subdir) endings in
  let tempf=(fun (sd,edg)->
  	let s1=s_dir^(Subdirectory.connectable_to_subpath sd)^x^edg in
  	if Sys.file_exists s1
  	then Some(Absolute_path.of_string s1)
  	else None
  ) in
  let opt=Option.find_and_stop tempf temp1 in
  if opt=None
  then raise(No_suitable_location(dir ,l_subdir,x))
  else  Option.unpack(opt);;

end;;






module German_arrange_positions_in_modulesystem=struct


(* 

There are two main rules :

1) The ancestors of a file x always come before x.
2) Non-optional files always come before optional files.

We assume that the first file is not optional, if the file system is not empty.

#use"Country/Germany/german_arrange_positions_in_modulesystem.ml";;


*)


let reserved_item_for_beginning_of_optional_files=None;;

let index_necessarily_before_optional_file mdata=
   let l1=Ennig.index_everything (mdata) in
   match Option.seek(fun (j,info)->Modulesystem_data.is_optional info
   ) l1 with
   None->Some(List.length(l1),reserved_item_for_beginning_of_optional_files)
   |Some(j1,_)->Some(j1-1,reserved_item_for_beginning_of_optional_files);;

let index_necessarily_after_nonoptional_file mdata=
   let l1=Ennig.index_everything (mdata) in
   match Option.seek(fun (j,info)->Modulesystem_data.is_optional info
   ) l1 with
   None->None
   |Some(j1,_)->Some(j1,reserved_item_for_beginning_of_optional_files);;

let formalize_other_bounds mdata l=
  let l1=Ennig.index_everything (mdata) in
  let localize=(fun anv->
    match Option.seek (fun (j,info)->(Modulesystem_data.name info)=anv) l1 with
    None->None
    |Some(j1,_)->Some(j1,Some(anv))
  ) in
  Image.image localize l;;

let lower_bound mdata x l_other_bounds_before=
  let name_of_x=Modulesystem_data.name x in
  let l1=Ennig.index_everything mdata in
  let temp1=(fun l->
     if l=[] then None else
     let last_parent_name=List.hd(List.rev l) in
     let j=fst(Option.find (fun (j,info)->
     (Modulesystem_data.name info)=last_parent_name) l1) in
     Some(j,Some(last_parent_name))
  )(Modulesystem_data.direct_fathers x) in
  let temp2=(fun bowl->
     if bowl
     then index_necessarily_before_optional_file mdata
     else None
  )(Half_dressed_module.is_optional name_of_x ) in
 let temp3=temp1::temp2::(formalize_other_bounds mdata l_other_bounds_before) in
 let temp4=Max.maximize_it_if_possible(fst)(temp3) in
 Option.propagate fst temp4;;
  
  
let upper_bound mdata x l_other_bounds_after=
  let name_of_x=Modulesystem_data.name x in
  let l1=Ennig.index_everything mdata in
  let temp0=Option.seek (fun (j,info)->List.mem name_of_x (Modulesystem_data.all_ancestors info)) l1 in
  let temp1=(function 
     None->None
     |Some(j1,data1)->Some(j1,Some(Modulesystem_data.name data1))
  )(temp0) in
  let temp2=(fun bowl->
     if bowl
     then None
     else index_necessarily_after_nonoptional_file mdata
  )(Half_dressed_module.is_optional name_of_x ) in
   let temp3=temp1::temp2::(formalize_other_bounds mdata l_other_bounds_after) in
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
  
 let insertion_index mdata dt lower_bound upper_bound=
    if upper_bound=None
    then List.length(mdata)
    else let (j_up,data_up)=Option.unpack(upper_bound) in
         if lower_bound=None
         then (* here we use the fact that the first file is not optional, 
              if the file system is not empty *)
              j_up-1
         else let (j_down,data_down)=Option.unpack(lower_bound) in
              if (j_down>j_up)
              then treat_insertion_error dt (j_down,data_down) (j_up,data_up)
              else j_up-1;;

 let insert_data mdata x (l_other_bounds_before,l_other_bounds_after)=
   let lower_bound=lower_bound mdata x l_other_bounds_before
   and upper_bound=upper_bound mdata x l_other_bounds_after
   in
   let i=insertion_index mdata x lower_bound upper_bound
   and l1=Ennig.index_everything (mdata) in
   let (temp1,temp2)=List.partition (fun (j,t)->j<=i) l1 in
   let temp3=Image.image snd temp1
   and temp4=Image.image snd temp2 in
   temp3@(x::temp4);;
   



end;;






module German_modify_modulesystem=struct


(* 


#use"Country/Germany/german_modify_modulesystem.ml";;


*)


 
 exception Non_registered_file of Mlx_ended_absolute_path.t;;  
 exception Non_registered_module of Half_dressed_module.t;;  
 exception Abandoned_children of Mlx_ended_absolute_path.t*(Half_dressed_module.t list);;
 exception Derelict_children of Half_dressed_module.t*(Half_dressed_module.t list);;  

 let unregister_mlx_file_and_keep_old_data mdata mlxfile=
    let hm=Mlx_ended_absolute_path.half_dressed_core mlxfile in
    let desc=German_data.descendants mdata [hm] in
    if desc<>[]
    then let temp1=Image.image Modulesystem_data.name desc in
         raise(Abandoned_children(mlxfile,temp1))
    else
    let (before,opt,after)=Three_parts.select_center_element  (fun dt->
      Modulesystem_data.name dt=hm) mdata in
    match opt with
     None->raise(Non_registered_file(mlxfile))
    |Some(dt)->
      let edg=Mlx_ended_absolute_path.ending mlxfile in
      if (not(Modulesystem_data.check_presence edg dt))
      then raise(Non_registered_file(mlxfile))
      else 
      let new_dt=Modulesystem_data.make_absence edg dt in
      if (Modulesystem_data.registered_endings new_dt)=[]
      then (dt,before@after)
      else (dt,before@(new_dt::after));;
    
   
 let unregister_mlx_file mdata mlxfile=snd(unregister_mlx_file_and_keep_old_data mdata mlxfile);;
   
 
 let unregister_module mdata hm=
    let desc=German_data.descendants mdata [hm] in
    if desc<>[]
    then let temp1=Image.image Modulesystem_data.name desc in
         raise(Derelict_children(hm,temp1))
    else
    let (before,opt,after)=Three_parts.select_center_element  (fun dt->
      Modulesystem_data.name dt=hm) mdata in
    if opt=None 
    then raise(Non_registered_module(hm))  
    else before@after;;
    
   
  
exception Already_registered_file of Mlx_ended_absolute_path.t;;  
exception Overcrowding of Mlx_ended_absolute_path.t*(Ocaml_ending.t list);;
exception Bad_pair of Mlx_ended_absolute_path.t*Ocaml_ending.t;; 
exception Name_conflict of Half_dressed_module.t * Half_dressed_module.t;; 
   
 
 let register_mlx_file mdata mlx_file =
   let hm=Mlx_ended_absolute_path.half_dressed_core mlx_file
   and ending=Mlx_ended_absolute_path.ending mlx_file in 
   let nm=Half_dressed_module.naked_module hm in
   let (before,opt,after)=Three_parts.select_center_element  (fun dt->
      Half_dressed_module.naked_module(Modulesystem_data.name dt)=nm) 
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
  
  let try_to_register_mlx_file mdata mlx_file=
    try(Some(register_mlx_file mdata mlx_file)) with _->None;;  

   let try_to_register_mlx_files mdata mlx_files=
   let rec tempf=(fun
    (vdata,failures,yet_untreated)->
      match yet_untreated with
      []->(failures,vdata)
      |mlx::others->
      (
        match try_to_register_mlx_file vdata mlx with
        None->tempf(vdata,mlx::failures,others)
        |Some(nfs)->tempf(nfs,failures,others)
      )
   ) in
   tempf(mdata,[],mlx_files);;
  
   
  exception NonoptDependingOnOpt of Half_dressed_module.t*(Half_dressed_module.t list);;  
  exception Nonregistered_module of Half_dressed_module.t;;  
  
   
let reposition mdata hm (l_before,l_after)=
   let (before,opt,after)=Three_parts.select_center_element
   (fun dt->
      Modulesystem_data.name dt=hm) mdata in
   if opt=None
   then raise(Nonregistered_module(hm))
   else 
   let info=Option.unpack opt 
   and amputated_data=before@after in
   German_arrange_positions_in_modulesystem.insert_data amputated_data info (l_before,l_after);;  

exception Non_existent_mtime of Mlx_ended_absolute_path.t;;

    
let recompute_module_info mdata hm=
   let (before,_,after)=Three_parts.select_center_element(
      fun md->Modulesystem_data.name md=hm
   ) mdata in
  let new_md=Read_info_on_file_in_system.recompute_complete_info_for_module
       mdata hm in 
  before@(new_md::after);;
   

     
   
   

end;;






module Alaskan_remove_debuggables=struct

(*

#use"Country/Alaska/alaskan_remove_debuggables.ml";;

*)



let rd dir mdata=
   let sbuild=(Directory_name.connectable_to_subpath dir)^"_build/" in
   Unix_command.uc("rm -f "^sbuild^"*.d.cm*"^" "^sbuild^"*.ocaml_debuggable");;
   
  



end;;






module German_start_debugging=struct

(*

#use"Country/Germany/german_start_debugging.ml";;

*)



let sd (mdata,tgts)=
    let _=Alaskan_remove_debuggables.rd German_constant.root mdata in
    let dbg=Debugged_name.debugger_name in
	let dir=German_constant.root in
	let rdir=German_directories.from_data mdata in
	let ap=Find_suitable_ending.find_file_location dir rdir 
	     (dbg^".ml") in
	let hm=Half_dressed_module.of_path_and_root ap dir in
	let mdata2=German_modify_modulesystem.recompute_module_info mdata hm in
	let tgt=Ocaml_target.debuggable hm in
	let answer=Alaskan_make_ocaml_target.make_nontoplevel German_constant.root
	(mdata2,tgts) tgt in
	let msg=(
	  if (fst answer)
	  then "\n\n Now, start \n\nocamldebug _build/"^dbg^".ocaml_debuggable\n\nin another terminal\n\n"
	  else "\n\n Something went wrong, see above. \n\n"
	) in
	let _=(
	  print_string msg;
	  flush stdout
	) in
	answer;;   
   



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
  x.needed_directories
 );;






 

end;;






module German_rename_module=struct


(* 


#use"Country/Germany/german_rename_module.ml";;

The functions of this module are supposed to be used
on an already up-to-date modulesystem.

We separate renaming from relocating. The latter changes
directories while the former changes only the name, staying in the
same directory. 



*)



exception Nonregistered_module of Half_dressed_module.t;;  


let on_monitored_modules mdata old_name new_name=
   let interm_list=Image.image
   (Abstract_renamer.abstractify old_name) mdata in
   let opt=Alaskan_data.find_module_registration mdata old_name in
   if opt=None
   then raise(Nonregistered_module(old_name))
   else 
   let old_dt=Option.unpack opt in
   let old_acolytes=Modulesystem_data.acolytes old_dt in
   let old_files=Image.image (fun mlx->Mlx_ended_absolute_path.short_path mlx) old_acolytes in 
   let new_acolytes=Image.image (fun mlx->Mlx_ended_absolute_path.do_file_renaming mlx new_name) old_acolytes in
   let new_files=Image.image (fun mlx->Mlx_ended_absolute_path.short_path mlx) new_acolytes in 
   let new_hm=Mlx_ended_absolute_path.half_dressed_core(List.hd new_acolytes) in
   let old_mname=Half_dressed_module.naked_module old_name
   and new_mname=Half_dressed_module.naked_module new_hm
   in
   let changer=Look_for_module_names.change_module_name_in_file
   old_mname new_mname in
   let desc=German_data.descendants mdata [old_name] in
   let temp1=Image.image Modulesystem_data.acolytes desc in
   let temp2=List.flatten temp1 in
   let temp3=Image.image Mlx_ended_absolute_path.to_path temp2 in
   let temp4=Option.filter_and_unpack (
     fun s->try Some(Absolute_path.of_string s) with _->None
   ) [
       German_constant.name_for_pervasivesfile;
       German_constant.name_for_printersfile;
     ] in
   
   let _=Image.image changer (temp3@temp4) in
   let s_root=Directory_name.connectable_to_subpath(German_constant.root) in     
   let _=Unix_command.uc
       ("rm -f "^s_root^"_build/"^(Half_dressed_module.to_string old_name)^".cm* ") in
   let new_list=Image.image
   (Abstract_renamer.unabstractify new_hm) interm_list in
   (new_list,(old_files,new_files));;
 
let on_targets (old_mdata,old_tgts) old_name new_name= 
  let untouched_tgts=List.filter
   (fun tgt->not(Alaskan_ingredients_for_ocaml_target.module_dependency_for_ocaml_target
   old_mdata [old_name] tgt)&&(Ocaml_target.main_module(tgt)<>Some(old_name)) ) old_tgts in
  let (new_mdata,(old_files,new_files))=on_monitored_modules old_mdata old_name new_name in
  let default_top=(German_data.default_toplevel new_mdata) in
  let (new_mdata2,new_tgts2)=
    snd(Alaskan_make_ocaml_target.make 
 	   German_constant.root
 	    (new_mdata,untouched_tgts) default_top) in
  ((new_mdata2,new_tgts2),(old_files,new_files));;   
 
 
 


 

end;;






module German_rename_directory=struct


(* 

#use"Country/Germany/german_rename_directory.ml";;

Renaming directories inside the Ocaml main directory.


*)

let on_short_path=Rename_endsubdirectory.re;;

let on_subdirectory=Subdirectory.rename_endsubdirectory;;

let on_half_dressed_module=Half_dressed_module.rename_endsubdirectory;;

let on_ms_data=Modulesystem_data.rename_endsubdirectory;; 

let on_absolute_path=Rename_endsubdirectory.on_absolute_path;;

let on_mlx_ended_absolute_path=
  Mlx_ended_absolute_path.rename_endsubdirectory;;

let on_ocaml_target=Ocaml_target.rename_endsubdirectory;;

let on_half_dressed_modules (old_subdir,new_subdirname) l=
    Image.image (on_half_dressed_module (old_subdir,new_subdirname)) l ;; 

let on_data (old_subdir,new_subdirname) ldata=
   Image.image (on_ms_data (old_subdir,new_subdirname)) ldata;;
 
let on_subdirectories (old_subdir,new_subdirname) l_subdir=
   Image.image (on_subdirectory (old_subdir,new_subdirname)) l_subdir;; 
   
let on_up_to_date_targets (old_subdir,new_subdirname) l_tgts=
   Image.image (on_ocaml_target (old_subdir,new_subdirname)) l_tgts;;    

let on_outside_files (old_subdir,new_subdirname) l=
    Image.image (on_absolute_path (old_subdir,new_subdirname)) l ;; 

let on_deleted_files (old_subdir,new_subdirname) rl=
    let l=Recently_deleted.to_string_list rl in
    Recently_deleted.of_string_list(
     Image.image (on_short_path (old_subdir,new_subdirname)) l 
    );; 
   
let on_changed_files (old_subdir,new_subdirname) rl=
    let l=Recently_changed.to_string_list rl in
    Recently_changed.of_string_list(
     Image.image (on_short_path (old_subdir,new_subdirname)) l 
    );;    
   
let on_created_files (old_subdir,new_subdirname) rl=
    let l=Recently_created.to_string_list rl in
    Recently_created.of_string_list(
     Image.image (on_short_path (old_subdir,new_subdirname)) l 
    );;       
   
   
let on_delchacre_files (old_subdir,new_subdirname) l=
    Image.image (on_short_path (old_subdir,new_subdirname)) l ;; 
   


 
 
 


end;;






module German_relocate_module=struct


(* 


#use"Country/Germany/german_relocate_module.ml";;

We separate renaming from relocating. The latter changes
directories while the former changes only the name, staying in the
same directory. 

Speech follows action : file displacement takes place first, 
then the Ocaml data values are updated.

*)


exception NonoptDependingOnOpt of Half_dressed_module.t*(Half_dressed_module.t list);;  
exception Nonregistered_module of Half_dressed_module.t;;  
 
let on_monitored_modules mdata old_name new_subdir=
   let (before,opt,after)=Three_parts.select_center_element  (fun dt->
      Modulesystem_data.name dt=old_name) mdata in
   if opt=None
   then raise(Nonregistered_module(old_name))
   else 
   let old_dt=Option.unpack opt in
   let old_acolytes=Modulesystem_data.acolytes old_dt in
   let old_files=Image.image (fun mlx->Mlx_ended_absolute_path.short_path mlx) old_acolytes in 
   let new_acolytes=Image.image (fun mlx->Mlx_ended_absolute_path.do_file_displacing mlx new_subdir) old_acolytes in
   let new_files=Image.image (fun mlx->Mlx_ended_absolute_path.short_path mlx) new_acolytes in 
   let new_name=Mlx_ended_absolute_path.half_dressed_core(List.hd new_acolytes) in
   let desc=German_data.descendants mdata [old_name] in
   let data_renamer=Modulesystem_data.rename (old_name,new_name) in
   let bowls=(	Half_dressed_module.is_optional old_name,
				Half_dressed_module.is_optional new_name) in
   let s_root=Directory_name.connectable_to_subpath(German_constant.root) in     
   let _=Unix_command.uc
       ("rm -f "^s_root^"_build/"^(Half_dressed_module.to_string old_name)^".cm* ") in
   if bowls=(false,true)
   then 
        let mandatory_desc=List.filter Modulesystem_data.is_not_optional desc in
        if mandatory_desc<>[]
        then let temp1=Image.image Modulesystem_data.name mandatory_desc in
             raise(NonoptDependingOnOpt(new_name,temp1))
        else 
        let (before2,after2)=German_data.optionality_partition after in
        let part1=before@before2
        and part2=Image.image data_renamer (old_dt::after2) in
        (part1@part2,(old_files,new_files))
   else
   if bowls=(true,false)
   then let (before1,after1)=German_data.optionality_partition before in
        let part2=Image.image data_renamer (old_dt::after1@after) in
        (before1@part2,(old_files,new_files))
   else let part2=Image.image data_renamer (old_dt::after) in
        (before@part2,(old_files,new_files));;

let on_targets (old_mdata,old_tgts) old_name new_subdir= 
  let untouched_tgts=List.filter
   (fun tgt->not(Alaskan_ingredients_for_ocaml_target.module_dependency_for_ocaml_target
   old_mdata [old_name] tgt)&&(Ocaml_target.main_module(tgt)<>Some(old_name)) ) old_tgts in
  let (new_mdata,(old_files,new_files))=on_monitored_modules old_mdata old_name new_subdir in
  let default_top=(German_data.default_toplevel new_mdata) in
  let (new_mdata2,new_tgts2)=
   snd(Alaskan_make_ocaml_target.make 
     German_constant.root
    (new_mdata,untouched_tgts) default_top) in
  ((new_mdata2,new_tgts2),(old_files,new_files));;   
 


end;;






module German_register_outside_file=struct


(* 

#use"Country/Germany/german_register_outside_file.ml";;


*)


let on_outside_directories (o_files,o_dirs) ap=
  let main_dir=German_constant.root in
  let temp1=Directory_name.cut_beginning main_dir (Absolute_path.to_string ap) in
  let s_sdir=Father_and_son.father temp1 '/' in
  let sdir=Subdirectory.of_string s_sdir in
  let new_odirs=(if List.mem sdir o_dirs then o_dirs else o_dirs@[sdir]) 
  and new_ofiles=(if List.mem ap o_files then o_files else ap::o_files) in
  (new_ofiles,new_odirs);;
  
(*  

let unregister_outside_file (o_files,o_dirs) ap=
  let main_dir=German_constant.root in
  let tempf=(fun ap2->
  	let temp1=Directory_name.cut_beginning main_dir (Absolute_path.to_string ap2) in
  	let s_sdir=Father_and_son.father temp1 '/' in
  	Subdirectory.of_string s_sdir 
  ) in
  let sdir=tempf ap in 
  let new_ofiles=List.filter (fun ap2->ap2<>ap) o_files in
  let new_odirs=(
       if List.exists(fun z->tempf(z)=sdir) new_ofiles
       then o_dirs
       else List.filter (fun sdir2->sdir2<>sdir) o_dirs
  )  in
  (new_ofiles,new_odirs);;     

*)

end;;






module Reconstruct_linear_poset=struct

(*

#use"reconstruct_linear_poset.ml";;

Computes the (canonical) maximal acyclic sub-poset of a given poset, returns
it as a list L where each element of L is a triple (a,anc_a,a_is_clean)
where anc_a is the list of all ancestors of a, ordered as in L, and a_is_clean
is a boolean indicating if a is the ancestor or a descendant of an "active"
element.

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
    match Option.seek(fun (x,y)->Tidel.elfenn x temp1) between with
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






module Update_ancs_libs_and_dirs_in_modulesystem=struct


(* 


#use"Makefile_makers/update_ancs_libs_and_dirs_in_modulesystem.ml";;

Takes a modulesystem where everything is correct except possibly
the libs and dirs. Updates everything recursively starting
from the modules with no dependencies. 

*)


let moduledata_hshtbl=Hashtbl.create 500;;

exception Iterator_for_update_exn;; 

let iterator_for_update (graet,da_ober)=match da_ober with
  []->raise(Iterator_for_update_exn)
  |(md,atoms_for_md)::peurrest->
     let hm=Modulesystem_data.name md 
     and mlx=Modulesystem_data.principal_mlx md in 
     let new_ancestor_names=Image.image Modulesystem_data.name atoms_for_md in
     let genealogy=Image.image (Hashtbl.find moduledata_hshtbl) new_ancestor_names in
     let new_libs=Read_info_on_file_in_system.find_needed_libraries mlx genealogy
     and new_dirs=Read_info_on_file_in_system.find_needed_directories mlx genealogy in
     let new_md=
     {
    	Modulesystem_data.name=md.Modulesystem_data.name;
        ml_present=md.Modulesystem_data.ml_present;
        mli_present=md.Modulesystem_data.mli_present;
        mll_present=md.Modulesystem_data.mll_present;
        mly_present=md.Modulesystem_data.mly_present;
        ml_modification_time=md.Modulesystem_data.ml_modification_time;
        mli_modification_time=md.Modulesystem_data.mli_modification_time;
        mll_modification_time=md.Modulesystem_data.mll_modification_time;
        mly_modification_time=md.Modulesystem_data.mly_modification_time;
        needed_libraries=new_libs;
        direct_fathers=md.Modulesystem_data.direct_fathers;
        all_ancestors=new_ancestor_names;
        needed_directories=new_dirs;
     } in
     let _=Hashtbl.add moduledata_hshtbl hm new_md in
     (new_md::graet,peurrest);;
     
let rec computer_for_update (graet,da_ober)=
  if da_ober=[]
  then List.rev(graet)
  else computer_for_update(iterator_for_update (graet,da_ober));;   

let update l=computer_for_update ([],l);;


     
     

end;;






module German_recompile=struct


(* 


#use"Country/Germany/german_recompile.ml";;


*)

module Private=struct

let message_about_circular_dependencies printer cycles= 
  if cycles=[]
  then ""
  else
  let temp1=Image.image(fun cycle->
    let ttemp1=Image.image printer cycle in
     String.concat " -> " ttemp1 
  ) cycles in
  let temp2=String.concat "\n\n" temp1 in
  temp2;;

exception Circular_dependencies of string;;

let treat_circular_dependencies tolerate_cycles printer cycles=
  if cycles=[]
  then ()
  else let msg=message_about_circular_dependencies printer cycles in  
       if tolerate_cycles
       then (print_string msg;flush stdout)     
       else raise(Circular_dependencies(msg));; 
       
let message_about_changed_modules changed_modules=
  let temp1=Image.image Half_dressed_module.to_string changed_modules in
  "\n\n\n"^
  "The following modules have been directly changed :\n"^
  (String.concat "\n" temp1)^
  "\n\n\n"
;;       
       
let announce_changed_modules changed_modules=
  if changed_modules=[]
  then ()
  else (print_string(message_about_changed_modules changed_modules);flush stdout);;
         

let put_md_list_back_in_order tolerate_cycles md_list initially_active_hms=
  let coat=Memoized.make (fun md->
    let anc_md=Modulesystem_data.direct_fathers(md) in
    List.filter (
      fun md2->List.mem(Modulesystem_data.name md2) anc_md
    ) md_list
  ) in
  let (cycles,old_list)=Reconstruct_linear_poset.reconstruct_linear_poset 
     coat md_list in
  let _=treat_circular_dependencies tolerate_cycles
       (fun md->Half_dressed_module.to_string(Modulesystem_data.name md)) 
       cycles in     
  let final_list=Update_ancs_libs_and_dirs_in_modulesystem.update old_list in 
  let active_descendants=Option.filter_and_unpack (
      fun md->
        let hm=Modulesystem_data.name md in
        if List.mem hm initially_active_hms
        then Some(hm)
        else
        if List.exists (fun hm2->List.mem hm2 initially_active_hms) 
             (Modulesystem_data.all_ancestors md)
        then Some(hm)
        else None
  ) final_list in  
  (final_list,active_descendants);;
 
end;; 
 
let on_monitored_modules tolerate_cycles mdata =
  let ref_for_changed_modules=ref[] 
  and ref_for_changed_shortpaths=ref[] in
  let declare_changed=(fun md->
    let hm=Modulesystem_data.name md in
    ref_for_changed_modules:=hm::(!ref_for_changed_modules);
    ref_for_changed_shortpaths:=((!ref_for_changed_shortpaths)@
                                (Modulesystem_data.short_paths md))
  ) in
  let new_md_list=Image.image(
     fun md->
       match Read_info_on_file_in_system.quick_update mdata md with
       None->md
       |Some(new_md)->
         let _=declare_changed(new_md) in
         new_md
  ) mdata in
  let changed_modules=List.rev(!ref_for_changed_modules) in
  if changed_modules=[] then ((mdata,[]),[]) else
  let _=Private.announce_changed_modules changed_modules in
  (Private.put_md_list_back_in_order tolerate_cycles new_md_list changed_modules,
  (!ref_for_changed_shortpaths));;  
  
let on_targets tolerate_cycles (old_mdata,old_tgts)=
    let ((new_mdata,hms_to_be_updated),short_paths)=
      on_monitored_modules tolerate_cycles old_mdata in
	if hms_to_be_updated=[] then None else
	let new_dirs=German_directories.from_data new_mdata 
 	and new_tgts1=Ocaml_target.still_up_to_date_targets hms_to_be_updated old_tgts in
 	let dir=German_constant.root in
 	let checker=Ocaml_target.test_target_existence dir in
 	let new_tgts=List.filter checker new_tgts1 in
 	let default_top=(German_data.default_toplevel new_mdata) in
 	let (new_mdata2,new_tgts2)=
 	  snd(Alaskan_make_ocaml_target.make 
 	   German_constant.root
 	  (new_mdata,new_tgts) default_top) in
    Some((new_mdata2,new_dirs,new_tgts2),short_paths);;   


   




end;;






module German_make_module_optional=struct


(* 


#use"Country/Germany/german_make_module_optional.ml";;


*)


exception Already_optional of Half_dressed_module.t;;

let on_targets (mdata,old_tgts) old_name=
    let s_old_name=Half_dressed_module.to_string(old_name) in
    if Substring.begins_with s_old_name "Optional/"
    then raise(Already_optional(old_name))
    else 
    German_relocate_module.on_targets 
    (mdata,old_tgts) old_name (Subdirectory.of_string "Optional");;    


end;;






module My_global_replace=struct

(*


#use"my_global_replace.ml";;

The my_global_replace is a replacement for Ocaml's Str.global_replace which has
the disadvantage of applying certain transforms to the replacement string.


*)


exception Ambiguity of string;;

let my_global_replace (a,b) s=
  let n=String.length(s) and na=String.length(a) in
  let indices=Substring.occurrences_of_in a s in
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
  let unchanged_intervals=Option.filter_and_unpack coords (Ennig.ennig 1 m) in
  if List.exists (fun (x,y)->x>y) unchanged_intervals
  then raise(Ambiguity(a)) 
  else 
  let unchanged_substrings=Image.image (fun (x,y)->Cull_string.interval s x y) unchanged_intervals in
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






module Replace_inside=struct

(*

#use"replace_inside.ml";;

*)


let replace_inside_string (a,b) s=
  My_global_replace.my_global_replace (a,b) s;;
 
let replace_several_inside_string l t=List.fold_left 
(fun s (a,b)->replace_inside_string (a,b) s) t l;;  
 
let replace_inside_file (a,b) fn=
    let s1=Io.read_whole_file fn in
    let la=String.length(a) in
    if List.exists (fun j->(String.sub s1 j la)=a) (Ennig.ennig 0 ((String.length s1)-la))
    then let s2=replace_inside_string (a,b) s1 in
         Io.overwrite_with fn s2
    else ();; 
    
let replace_several_inside_file l fn=
    let s1=Io.read_whole_file fn in
    let s2=replace_several_inside_string l s1  in
    Io.overwrite_with fn s2;; 

exception Absent_beginning_marker of string;;
exception Absent_ending_marker of string;; 
 
let overwrite_between_markers_inside_string ovw_b (bm,em)
   s1=
     let b=Overwriter.to_string ovw_b in
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
    let s1=Io.read_whole_file fn in
    let s2=overwrite_between_markers_inside_string ovw_b (bm,em) s1 in
    Io.overwrite_with fn s2;;      


let overwrite_and_dump_markers_inside_string ovw_b (bm,em)
   s1=
     let b=Overwriter.to_string ovw_b in
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
    let s1=Io.read_whole_file fn in
    let s2=overwrite_and_dump_markers_inside_string ovw_b (bm,em) s1 in
    Io.overwrite_with fn s2;;      

let show ()=Unix_command.uc "ocamlc -i replace_inside.ml";;  
 
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






module Current_date=struct

(*

#use"current_date.ml";;

*)

let current_date ()=
  let temp=Unix.gmtime(Unix.time()) in
  let year=string_of_int(temp.Unix.tm_year+1900)
  and month1=string_of_int(temp.Unix.tm_mon+1)
  and day1=string_of_int(temp.Unix.tm_mday) in
  let month=Cull_string.resize_from_right month1 2 '0'
  and day=Cull_string.resize_from_right day1 2 '0' in
  year^"_"^month^"_"^day;;



end;;






module German_forget_unregistered_file=struct

(*

#use"Country/Germany/german_forget_unregistered_file.ml";;

*)


let forget ap=
   let s_dir=Directory_name.connectable_to_subpath(German_constant.root) in
   let n_dir=String.length s_dir in
   let s_ap=Absolute_path.to_string ap in
   let subpath=Cull_string.cobeginning n_dir s_ap in
   let new_subpath=(Current_date.current_date())^"_"^
         (Replace_inside.replace_inside_string ("/","_dir_") subpath) in
   let _=Unix_command.uc ("mkdir -p "^s_dir^"Forgotten") in
   let _=Unix_command.uc ("touch "^s_dir^"Forgotten/"^new_subpath) in
   let _=Unix_command.uc ("mv "^s_ap^" "^s_dir^"Forgotten/"^new_subpath) in
   subpath;;




end;;






module German_forget_module=struct

(*

#use"Country/Germany/german_forget_module.ml";;

*)

    
exception ModuleWithDependencies of 
	Half_dressed_module.t*(Half_dressed_module.t list);;
exception Non_registered_module of Half_dressed_module.t;;



let on_targets (mdata,dirs,tgts) hm=
  match Alaskan_data.find_module_registration mdata hm with
   None->raise(Non_registered_module(hm))
  |Some(dt)->
   let bel=German_data.below mdata hm in
    if bel=[]
    then let (answer,short_paths)=German_unregister_module.on_targets (mdata,tgts) hm in
         let sfn=Half_dressed_module.to_shortened_string hm in
         let _=Image.image
         (fun edg->
          let cmd="rm -f _build/"^sfn^edg in
          Unix_command.uc(cmd))
         [".cm*";".d.cm*";".caml_debuggable"] in
         let temp1=Image.image (fun t->
            Absolute_path.of_string(Directory_name.join (German_constant.root) t)
         ) short_paths in
         let _=Image.image German_forget_unregistered_file.forget temp1 in
         (answer,short_paths)
    else raise(ModuleWithDependencies(hm,bel));;


end;;






module German_forget_file=struct

(*

#use"Country/Germany/german_forget_file.ml";;

*)

    
exception FileWithDependencies of 
	Mlx_ended_absolute_path.t*(Half_dressed_module.t list);;


let on_targets triple ap=
  let (mdata,dirs,tgts)=triple in
  let root=German_constant.root in
  let hm=Half_dressed_module.of_path_and_root ap root 
  and mlx=Mlx_ended_absolute_path.of_path_and_root ap root  in
  match Alaskan_data.find_module_registration mdata hm with
   None->triple
  |Some(_)->
   let bel=German_data.below mdata (Mlx_ended_absolute_path.half_dressed_core mlx) in
    if bel=[]
    then let s_hm=Half_dressed_module.to_string hm in
         let fn=(Directory_name.connectable_to_subpath(root))^s_hm in
         let _=Image.image
         (fun edg->Unix_command.uc("rm -f "^fn^edg^"*"))
         [".cm";".d.cm";".caml_debuggable"] in
         German_unregister_mlx_file.on_targets (mdata,tgts) mlx
    else raise(FileWithDependencies(mlx,bel));;


end;;






module Prepare_dircopy_update=struct

(*

#use"prepare_dircopy_update.ml";;

*)


let compute_deleted_in_diff sourcedir destdir=
   let s_sourcedir=Directory_name.connectable_to_subpath sourcedir
   and s_destdir=Directory_name.connectable_to_subpath destdir in
   let temp1=More_unix.quick_beheaded_complete_ls s_destdir in
   List.filter(
       fun s->(s<>"README")
              &&(not(Substring.begins_with s ".git/")) 
              &&(not(Sys.file_exists(s_sourcedir^s)))
   ) temp1;;
   
let compute_nondeleted_in_diff (sourcedir,l) destdir=
   let s_sourcedir=Directory_name.connectable_to_subpath sourcedir
   and s_destdir=Directory_name.connectable_to_subpath destdir in
   let created_accu=ref[]
   and changed_accu=ref[] in
   let _=Image.image(
   	  fun s->
   	    if (not(Sys.file_exists(s_destdir^s)))
   	    then created_accu:=s::(!created_accu)
   	    else 
   	    (
   	    let txt1=Io.read_whole_file
   	    (Absolute_path.of_string(s_sourcedir^s))
   	    and txt2=Io.read_whole_file
   	    (Absolute_path.of_string(s_destdir^s)) in
   	    if txt1<>txt2
   	    then changed_accu:=s::(!changed_accu)
   	    )
   ) l in
   (Recently_created.of_string_list (!created_accu),
    Recently_changed.of_string_list (!changed_accu));;   
   
  
let compute_diff (sourcedir,l) destdir=
   let (created,changed)=compute_nondeleted_in_diff (sourcedir,l) destdir in
   Dircopy_diff.veil
   
   	(Recently_deleted.of_string_list(compute_deleted_in_diff sourcedir destdir))
   	changed
   	created
   ;;
   
let greedy_list sourcedir=
   let source_paths=More_unix.complete_ls_with_nondirectories_only sourcedir in
   Image.image (fun ap->
   Directory_name.cut_beginning sourcedir (Absolute_path.to_string ap) ) source_paths;;
      
   
let compute_greedy_diff sourcedir destdir=
   compute_diff (sourcedir,greedy_list sourcedir) destdir;;
   
let commands_for_update destination_dir diff=
   if Dircopy_diff.is_empty diff
   then []
   else 
   let s_destination=Directory_name.connectable_to_subpath destination_dir in
   let created_ones=Dircopy_diff.recently_created diff  in
   let temp2=Option.filter_and_unpack
   (fun fn->
     if String.contains fn '/'
     then let dn=Father_and_son.father fn '/' in
          Some("mkdir -p "^s_destination^dn)
     else None 
    )
   created_ones in
   let temp3=Ordered.forget_order(Ordered_string.diforchan temp2) in
   let s_source=Directory_name.connectable_to_subpath German_constant.root in
   let temp4=Image.image(
      fun fn->
      "cp "^s_source^fn^" "^s_destination^(Father_and_son.father fn '/')
   ) created_ones in
   let changed_ones=Dircopy_diff.recently_changed diff in
   let temp5=Image.image(
      fun fn->
      "cp "^s_source^fn^" "^s_destination^fn
   ) changed_ones in
   let temp7=Image.image(
      fun fn->
      "rm "^s_destination^fn
   ) (Dircopy_diff.recently_deleted diff) in
   (temp3@temp4@temp5@temp7);;  
   
   

end;;






module German_delchacre_from_scratch=struct

(*

#use"Country/Germany/german_delchacre_from_scratch.ml";;

*)

let dfs (mdata,ofiles)=
   let temp1=Alaskan_data.all_mlx_paths mdata in
   let temp3=temp1@ofiles in
   let temp4=Image.image (fun ap->Directory_name.cut_beginning 
    (German_constant.root) (Absolute_path.to_string ap)) temp3 in
  let destination_dir=German_constant.dir_for_backup in
  Prepare_dircopy_update.compute_diff
     (German_constant.root,temp4) destination_dir;;
 






   
   
  

end;;






module German_created_or_deleted=struct


(* 


#use"Country/Germany/german_created_or_deleted.ml";;



*)

let update (r_deleted,r_created) 
   (deleted,created)=
  let o_del=Ordered_string.safe_set (Recently_deleted.to_string_list deleted)
  and o_cre=Ordered_string.safe_set (Recently_created.to_string_list created) 
  and o_rdel=Ordered_string.safe_set r_deleted 
  and o_rcre=Ordered_string.safe_set r_created in
  
  let o_rdel1=Tidel.kengeij o_rdel o_cre
  and o_rdel2=Tidel.lemel   o_rdel o_cre
  and o_rcre1=Tidel.kengeij o_rcre o_del
  and o_rcre2=Tidel.lemel   o_rcre o_del in
  
  let o_del2=Ordered_string.teuzin o_del o_rdel2
  and o_cre2=Ordered_string.teuzin o_cre o_rcre2 in
  
  let o_del3=Ordered_string.lemel o_del2 o_rcre1
  and o_cre3=Ordered_string.lemel o_cre2 o_rdel1 in
  
  (Recently_deleted.of_string_list(Ordered.forget_order o_del3),
   Recently_created.of_string_list(Ordered.forget_order o_cre3));;



end;;






module German_changed=struct


(* 


#use"Country/Germany/german_changed.ml";;



*)

let update r_changed changed=
  let r_chan=Ordered_string.safe_set r_changed
  and   chan=Ordered_string.safe_set (Recently_changed.to_string_list changed) in
  let whole=Ordered_string.teuzin r_chan chan in
  Recently_changed.of_string_list(Ordered.forget_order whole);;



end;;






module German_backup_target_system=struct

(*

#use"Country/Germany/german_backup_target_system.ml";;

*)



let github_after_backup=ref(true);;

let commands_for_backup diff=
   if Dircopy_diff.is_empty diff
   then ([],[])
   else 
   let destination_dir=German_constant.dir_for_backup in
   let s_destination=Directory_name.connectable_to_subpath destination_dir in
   let created_ones=Dircopy_diff.recently_created diff in
   let temp2=Option.filter_and_unpack
   (fun fn->
     if String.contains fn '/'
     then let dn=Father_and_son.father fn '/' in
          Some("mkdir -p "^s_destination^dn)
     else None 
    )
   created_ones in
   let temp3=Ordered.forget_order(Ordered_string.diforchan temp2) in
   let s_source=Directory_name.connectable_to_subpath German_constant.root in
   let temp4=Image.image(
      fun fn->
      "cp "^s_source^fn^" "^s_destination^(Father_and_son.father fn '/')
   ) created_ones in
   let changed_ones=Dircopy_diff.recently_changed diff in
   let temp5=Image.image(
      fun fn->
      "cp "^s_source^fn^" "^s_destination^fn
   ) changed_ones in
   let temp6=Image.image(
      fun fn->
      "git add "^fn
   ) (created_ones@changed_ones) in
   let temp7=Image.image(
      fun fn->
      "git rm "^fn
   ) (Dircopy_diff.recently_deleted diff) in
   (temp3@temp4@temp5,temp6@temp7);;

let backup_with_message diff msg=
  let destination_dir=German_constant.dir_for_backup  
  and (nongit_cmds,git_cmds)=commands_for_backup diff in
  let s_destination=Directory_name.connectable_to_subpath destination_dir in
  let _=Image.image Unix_command.uc nongit_cmds in
  let _=(
  if (!github_after_backup)
  then let cwd=Sys.getcwd() in
       let _=Sys.chdir s_destination in
       let _=Image.image Unix_command.uc git_cmds in
       let _=Image.image Unix_command.uc
       [
         "git commit -m \""^msg^"\"";
         "git push"
       ] in
       Sys.chdir cwd
  else ()
  ) in
  ();;

let backup diff opt=
  let msg=(
   match opt with
    None->Dircopy_diff.explain diff
   |Some(msg0)->msg0) in
  backup_with_message diff msg;;
  







   
   
  

end;;






module Alaskan_write_makefile=struct


(* 

#use"Country/Alaska/alaskan_write_makefile.ml";;


*)

let slice_targets tgts=
  let temp1=Sliced_string.make_aggregates_if_possible 
                   (Separator.of_string " ") 
                   (Image.image Ocaml_target.to_string tgts) in
  Sliced_string.to_string_list temp1;;

let slice_shortened_targets tgts=
  let temp1=Sliced_string.make_aggregates_if_possible 
                   (Separator.of_string " ") 
                   (Image.image Ocaml_target.to_shortened_string tgts) in
  Sliced_string.to_string_list temp1;;

let write_makefile_element main_root mdata tgt=
 let ingrs=Alaskan_ingredients_for_ocaml_target.ingredients_for_ocaml_target mdata tgt in
 let sliced_ingrs=slice_shortened_targets ingrs in
 let cmds=Alaskan_command_for_ocaml_target.command_for_ocaml_target 
                       main_root mdata tgt in
 let s1=(Ocaml_target.to_shortened_string tgt)^" : " 
 and s2=String.concat " \\\n" sliced_ingrs
 and s3="\n\t"
 and s4=String.concat "\n\t" cmds in
 String.concat "" [s1;s2;s3;s4];;
 
 
let write_makefile (main_root,main_toplevel_name) mdata=
 let temp1=Alaskan_data.default_targets German_constant.main_toplevel_name mdata in
 let temp2=Image.image (write_makefile_element main_root mdata) temp1 in
 let temp5=slice_targets  temp1 in
 let temp6=String.concat " \\\n" temp5 in
 let temp7="clean :\n\trm -r -f "^temp6^"\n\n" in
 String.concat "\n\n" (temp2@[temp7]);;

 

end;;






module Alaskan_up_to_date_targets=struct


(* 

#use"Country/Alaska/alaskan_up_to_date_targets.ml";;


*)

let loadings (main_root,name_for_loadingsfile) (dirs,tgts)=
  let s_root=Directory_name.connectable_to_subpath main_root in
  let part1="\n(*\n #use\""^s_root^(name_for_loadingsfile)^"\";"^";\n*)\n\n" in
  let temp5=Image.image(
     fun sd->
     "#directory\""^s_root^(Subdirectory.connectable_to_subpath sd)^"\";"^";"
  ) ((Subdirectory.of_string "_build")::dirs) in
  let part2=String.concat "\n" temp5 
  and part3="\n\n#load\"nums.cma\";"^";\n#load\"str.cma\";"^";\n#load\"unix.cma\";"^";\n\n\n" in
  let temp2=Option.filter_and_unpack (
    function (Ocaml_target.CMO(x))->
      let s=Father_and_son.son (Half_dressed_module.to_string x) '/' in
      Some("#load\""^s^".cmo\";"^";") 
    |_->None
  ) tgts in
  let temp3="\n\n\n"::(List.rev ("\n\n\n"::temp2)) in
  let part4=String.concat "\n" temp3 in
  part1^part2^part3^part4;; 
  


let add_target_perhaps opt_tgt l=Option.add_perhaps opt_tgt l;;



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
      "#install_printer "^(Half_dressed_module.capitalized_module_name x)^".print_out;"^";"
  ) printer_equipped_types in
  let temp3="\n\n\n"::(List.rev ("\n\n\n"::temp2)) in
  let part2=String.concat "\n" temp3 in
  part2;;  
 
let declare_printer hm0 l=hm0::l;;
         
let undeclare_printer hm0 l=
  List.filter (fun hm->hm<>hm0) l;;    
 

end;;






module Alaskan_save_all=struct


(* 

#use"Country/Alaska/alaskan_save_all.ml";;


*)

module Private=struct

  let save_makefile (root,main_toplevel_name,location_for_makefile) mdata=
    let s1="# This makefile was automatocally written by\n"^
    "# the write_makefile function in the ml_manager module. \n\n"^
    (Alaskan_write_makefile.write_makefile 
      (root,main_toplevel_name)
    mdata) in
    let lm=Directory_name.force_join root location_for_makefile in
    Io.overwrite_with (Absolute_path.of_string lm) s1;;
  
  let save_loadingsfile (root,location_for_loadingsfile) (dirs,tgts)=
     let s=Alaskan_up_to_date_targets.loadings (root,location_for_loadingsfile)
      (dirs,tgts)
     and lm=Directory_name.force_join root  location_for_loadingsfile in
     Io.overwrite_with (Absolute_path.of_string lm) s;;
  
  let save_printersfile (root,location_for_printersfile) printer_equipped_types=
     let s=Alaskan_printer_equipped_types.instructions printer_equipped_types
     and lm=Directory_name.force_join root  location_for_printersfile in
     let beg_mark="(*Registered printers start here *)"
     and end_mark="(*Registered printers end here *)" in
     Replace_inside.overwrite_between_markers_inside_file
     (Overwriter.of_string s)
     (beg_mark,end_mark)
     (Absolute_path.of_string lm);;
  
  let industrial_separator1=Industrial_separator.alaskan_save_all1;;  
  let industrial_separator2=Industrial_separator.alaskan_save_all2;;    

  
  let archive 
    (mdata,directories,up_to_date_targets,
      outside_files,outside_directories,
      recently_deleted,recently_changed,recently_created,
      printer_equipped_types)=
      
     let temp2=Image.image (fun w->Nonblank.make(Subdirectory.without_trailing_slash w)) directories 
     and temp3=Image.image Ocaml_target.archive up_to_date_targets 
     and temp4=Image.image (fun w->Absolute_path.to_string w) outside_files 
     and temp5=Image.image (fun w->Nonblank.make(Subdirectory.without_trailing_slash w)) outside_directories 
     and temp6=Image.image (fun w->Nonblank.make w) (Recently_deleted.to_string_list recently_deleted) 
     and temp7=Image.image (fun w->Nonblank.make w) (Recently_changed.to_string_list recently_changed) 
     and temp8=Image.image (fun w->Nonblank.make w) (Recently_created.to_string_list recently_created) 
     and temp9=Image.image Half_dressed_module.archive printer_equipped_types in
     String.concat industrial_separator1
     [
       Alaskan_data.archive mdata;
       Nonblank.make(String.concat industrial_separator2 temp2);
       Nonblank.make(String.concat industrial_separator2 temp3);
       Nonblank.make(String.concat industrial_separator2 temp4);
       Nonblank.make(String.concat industrial_separator2 temp5);
       Nonblank.make(String.concat industrial_separator2 temp6);
       Nonblank.make(String.concat industrial_separator2 temp7);
       Nonblank.make(String.concat industrial_separator2 temp8);
       Nonblank.make(String.concat industrial_separator2 temp9);
     ];;
  
  let save_targetfile (root,location_for_targetfile) uple=
    let s1=archive uple in
    let lt=Directory_name.force_join root location_for_targetfile in
    Io.overwrite_with (Absolute_path.of_string lt) s1;;
  
  end;;
  
  let read_all s=
     let l1=Str.split (Str.regexp_string Private.industrial_separator1) s in
     let v1=List.nth l1 0 in
     let v2=Str.split (Str.regexp_string Private.industrial_separator2) (Nonblank.decode(List.nth l1  1)) 
     and v3=Str.split (Str.regexp_string Private.industrial_separator2) (Nonblank.decode(List.nth l1  2)) 
     and v4=Str.split (Str.regexp_string Private.industrial_separator2) (Nonblank.decode(List.nth l1  3)) 
     and v5=Str.split (Str.regexp_string Private.industrial_separator2) (Nonblank.decode(List.nth l1  4))   
     and v6=Str.split (Str.regexp_string Private.industrial_separator2) (Nonblank.decode(List.nth l1  5))
     and v7=Str.split (Str.regexp_string Private.industrial_separator2) (Nonblank.decode(List.nth l1  6)) 
     and v8=Str.split (Str.regexp_string Private.industrial_separator2) (Nonblank.decode(List.nth l1  7))   
     and v9=Str.split (Str.regexp_string Private.industrial_separator2) (Nonblank.decode(List.nth l1  8))
     in
     let new_mdata=Alaskan_data.unarchive v1 in
     let new_directories=Image.image (fun v->Subdirectory.of_string(Nonblank.decode v)) v2 in
     let new_targets=Image.image Ocaml_target.unarchive v3 in
     let new_ofiles=Image.image Absolute_path.of_string v4 in
     let new_odirectories=Image.image (fun v->Subdirectory.of_string(Nonblank.decode v)) v5 in
     let new_dfiles=Recently_deleted.of_string_list(Image.image Nonblank.decode v6) in
     let new_chfiles=Recently_changed.of_string_list(Image.image Nonblank.decode v7) in
     let new_crfiles=Recently_created.of_string_list(Image.image Nonblank.decode v8) in
     let new_pe_types=Image.image Half_dressed_module.unarchive v9 in
     
  (
      new_mdata,
      new_directories,
      new_targets,
      new_ofiles,
      new_odirectories,
      new_dfiles,
      new_chfiles,
      new_crfiles,
      new_pe_types
  );;
  
  let write_all 
  (root,main_toplevel_name,
    location_for_makefile,
    location_for_targetfile,
    location_for_loadingsfile,
    location_for_printersfile
    )
    uple= 
    let (mdata,directories,up_to_date_targets,
      _,_,_,_,_,printer_equipped_types)=uple in
     (
      Private.save_makefile (root,main_toplevel_name,location_for_makefile) mdata;
      Private.save_loadingsfile (root,location_for_loadingsfile) (directories,up_to_date_targets);
      Private.save_targetfile (root,location_for_targetfile) uple;
      Private.save_printersfile (root,location_for_printersfile) printer_equipped_types;
     );;
  
  


end;;






module Alaskan_arrange_positions_in_modulesystem=struct


(* 

There are two main rules :

1) The ancestors of a file x always come before x.
2) Non-optional files always come before optional files.

We assume that the first file is not optional, if the file system is not empty.

#use"Country/Alaska/alaskan_arrange_positions_in_modulesystem.ml";;


*)


let reserved_item_for_beginning_of_optional_files=None;;

let index_necessarily_before_optional_file mdata=
   let l1=Ennig.index_everything (mdata) in
   match Option.seek(fun (j,info)->Modulesystem_data.is_optional info
   ) l1 with
   None->Some(List.length(l1),reserved_item_for_beginning_of_optional_files)
   |Some(j1,_)->Some(j1-1,reserved_item_for_beginning_of_optional_files);;

let index_necessarily_after_nonoptional_file mdata=
   let l1=Ennig.index_everything (mdata) in
   match Option.seek(fun (j,info)->Modulesystem_data.is_optional info
   ) l1 with
   None->None
   |Some(j1,_)->Some(j1,reserved_item_for_beginning_of_optional_files);;

let formalize_other_bounds mdata l=
  let l1=Ennig.index_everything (mdata) in
  let localize=(fun anv->
    match Option.seek (fun (j,info)->(Modulesystem_data.name info)=anv) l1 with
    None->None
    |Some(j1,_)->Some(j1,Some(anv))
  ) in
  Image.image localize l;;

let lower_bound mdata x l_other_bounds_before=
  let name_of_x=Modulesystem_data.name x in
  let l1=Ennig.index_everything mdata in
  let temp1=(fun l->
     if l=[] then None else
     let last_parent_name=List.hd(List.rev l) in
     let j=fst(Option.find (fun (j,info)->
     (Modulesystem_data.name info)=last_parent_name) l1) in
     Some(j,Some(last_parent_name))
  )(Modulesystem_data.direct_fathers x) in
  let temp2=(fun bowl->
     if bowl
     then index_necessarily_before_optional_file mdata
     else None
  )(Half_dressed_module.is_optional name_of_x ) in
 let temp3=temp1::temp2::(formalize_other_bounds mdata l_other_bounds_before) in
 let temp4=Max.maximize_it_if_possible(fst)(temp3) in
 Option.propagate fst temp4;;
  
  
let upper_bound mdata x l_other_bounds_after=
  let name_of_x=Modulesystem_data.name x in
  let l1=Ennig.index_everything mdata in
  let temp0=Option.seek (fun (j,info)->List.mem name_of_x (Modulesystem_data.all_ancestors info)) l1 in
  let temp1=(function 
     None->None
     |Some(j1,data1)->Some(j1,Some(Modulesystem_data.name data1))
  )(temp0) in
  let temp2=(fun bowl->
     if bowl
     then None
     else index_necessarily_after_nonoptional_file mdata
  )(Half_dressed_module.is_optional name_of_x ) in
   let temp3=temp1::temp2::(formalize_other_bounds mdata l_other_bounds_after) in
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
  
 let insertion_index mdata dt lower_bound upper_bound=
    if upper_bound=None
    then List.length(mdata)
    else let (j_up,data_up)=Option.unpack(upper_bound) in
         if lower_bound=None
         then (* here we use the fact that the first file is not optional, 
              if the file system is not empty *)
              j_up-1
         else let (j_down,data_down)=Option.unpack(lower_bound) in
              if (j_down>j_up)
              then treat_insertion_error dt (j_down,data_down) (j_up,data_up)
              else j_up-1;;

 let insert_data mdata x (l_other_bounds_before,l_other_bounds_after)=
   let lower_bound=lower_bound mdata x l_other_bounds_before
   and upper_bound=upper_bound mdata x l_other_bounds_after
   in
   let i=insertion_index mdata x lower_bound upper_bound
   and l1=Ennig.index_everything (mdata) in
   let (temp1,temp2)=List.partition (fun (j,t)->j<=i) l1 in
   let temp3=Image.image snd temp1
   and temp4=Image.image snd temp2 in
   temp3@(x::temp4);;
   



end;;






module Alaskan_reposition_module=struct


(* 


#use"Country/Alaska/alaskan_reposition_module.ml";;


*)

exception Nonregistered_module of Half_dressed_module.t;;  
  
   
let rpm mdata hm (l_before,l_after)=
   let (before,opt,after)=Three_parts.select_center_element
   (fun dt->
      Modulesystem_data.name dt=hm) mdata in
   if opt=None
   then raise(Nonregistered_module(hm))
   else 
   let info=Option.unpack opt 
   and amputated_data=before@after in
   Alaskan_arrange_positions_in_modulesystem.insert_data 
    amputated_data info (l_before,l_after);;  



end;;






module Alaskan_register_mlx_file=struct


(* 

#use"Country/Alaska/alaskan_register_mlx_file.ml";;


*)

exception Already_registered_file of Mlx_ended_absolute_path.t;;  
exception Overcrowding of Mlx_ended_absolute_path.t*(Ocaml_ending.t list);;
exception Bad_pair of Mlx_ended_absolute_path.t*Ocaml_ending.t;; 
exception Name_conflict of Half_dressed_module.t * Half_dressed_module.t;; 
   
 
let on_monitored_modules mdata mlx_file =
   let hm=Mlx_ended_absolute_path.half_dressed_core mlx_file
   and ending=Mlx_ended_absolute_path.ending mlx_file in 
   let nm=Half_dressed_module.naked_module hm in
   let (before,opt,after)=Three_parts.select_center_element  (fun dt->
      Half_dressed_module.naked_module(Modulesystem_data.name dt)=nm) 
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
  let hm=Mlx_ended_absolute_path.half_dressed_core mlx in
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
   let _=Image.image (fun s->
     Unix_command.uc ("touch "^s_main_dir^"/"^s)
      ) [Debugged_name.debugger_name^".ml";"neptu.ml";
         "my_loadings.ml";"my_printers.ml";"my_pervasives.ml";".ocamlinit"]   in
   let _=Io.overwrite_with 
        (Absolute_path.of_string (s_main_dir^"/.ocamlinit"))
        "\n#use\"my_loadings.ml\";;\n#use\"my_printers.ml\";;\n#use\"my_pervasives.ml\";;\n" in   
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
     ["neptu.ml";
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
  let s_dir=Directory_name.connectable_to_subpath main_dir in
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
          match Option.seek (fun (_,(_,s1))->s1=t) temp1 with
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
      Mlx_ended_absolute_path.try_from_path_and_root ap dir
   ) l in
   Alaskan_try_to_register.mlx_files [] temp1;;


let usual_outsiders=
    [
      "neptu.ml";
      "my_loadings.ml";
      "my_pervasives.ml";
      "my_printers.ml";
    ];;

let from_main_directory dir opt_topl_name special_outsiders=
	let old_s=Directory_name.connectable_to_subpath(dir) in
	let s1=Cull_string.coending 1 old_s in (* mind the trailing slash *)
	let temp1=select_good_files s1 in
    let temp2=clean_list_of_files dir temp1 in
    let temp3=compute_dependencies temp2 in
    let (failures,mdata1)=from_prepared_list dir temp3 in
    let preqt=Alaskan_printer_equipped_types.from_data mdata1 in
    let topl_name=(if opt_topl_name=None then "ecaml" else Option.unpack opt_topl_name) in
    let topl=(Alaskan_data.default_toplevel topl_name mdata1) in
 	let (mdata2,new_tgts2)=snd(Alaskan_make_ocaml_target.make dir (mdata1,[]) topl) in
  let temp2=List.filter (fun x->not(List.mem x special_outsiders)) usual_outsiders in 
 	let old_outsiders=temp2@special_outsiders in
 	let new_outsiders=Option.filter_and_unpack (fun t->
 	   let s_ap=Directory_name.join dir t in
 	   if Sys.file_exists s_ap
 	   then Some(Absolute_path.of_string s_ap)
 	   else None
 	) old_outsiders in
 	(mdata2,new_tgts2,new_outsiders,preqt);;



end;;






module German_wrapper=struct

(*

#use"Country/Germany/german_wrapper.ml";;


*)

module Private=struct

let data_ref=ref([]:Modulesystem_data.t list);;
let directories_ref=ref([]:Subdirectory.t list);;
let up_to_date_targets_ref=ref([]:Ocaml_target.t list);;
let outside_files_ref=ref([]:Absolute_path.t list);;
let outside_directories_ref=ref([]:Subdirectory.t list);;
let recently_deleted_ref=ref(Recently_deleted.of_string_list []);;
let recently_changed_ref=ref(Recently_changed.of_string_list []);;
let recently_created_ref=ref(Recently_created.of_string_list []);;
let printer_equipped_types_ref=ref([]:Half_dressed_module.t list);;

let whole ()=(
	(!data_ref),
	(!directories_ref),
	(!up_to_date_targets_ref),
	(!outside_files_ref),
	(!outside_directories_ref),
	(!recently_deleted_ref),
	(!recently_changed_ref),
	(!recently_created_ref),
	(!printer_equipped_types_ref)
);;

let save_all ()=Alaskan_save_all.write_all 
  (German_constant.root,German_constant.main_toplevel_name, 
    German_constant.name_for_makefile,
    German_constant.name_for_targetfile,
    German_constant.name_for_loadingsfile,
    German_constant.name_for_printersfile
  )
  (
	  whole()
  );;

let recompile ()=
   match German_recompile.on_targets false (!data_ref,!up_to_date_targets_ref) with
    None->false
   |Some((new_mdata,new_dirs,new_tgts),short_paths)->
       let changes=German_changed.update short_paths
             (!recently_changed_ref) in
       let _=(
         data_ref:=new_mdata;
         directories_ref:=new_dirs;
         up_to_date_targets_ref:=new_tgts;
         recently_changed_ref:=changes;
         save_all();
       ) in
       true;;

let diff ()=
    Dircopy_diff.veil
        (!(recently_deleted_ref))
        (!(recently_changed_ref))
        (!(recently_created_ref))
    ;;

end;;


let backup opt_msg=
  let _=German_backup_target_system.backup (Private.diff()) opt_msg in
  
  (
		Private.recently_deleted_ref:=Recently_deleted.of_string_list [];
		Private.recently_changed_ref:=Recently_changed.of_string_list [];
		Private.recently_created_ref:=Recently_created.of_string_list [];
        Private.save_all();
   );;  

let data ()=(!Private.data_ref);;

let declare_printer_equipped_type hm=
  (
  (Private.printer_equipped_types_ref:=
  (!Private.printer_equipped_types_ref)@[hm]);
  Private.save_all ()
  );;


let diff=Private.diff;;
let directories ()=(!Private.directories_ref);;


 
 
let end_debugging ()=
    let _= Alaskan_remove_debuggables.rd 
                German_constant.root (!Private.data_ref) in
                
    let new_tgts=List.filter Ocaml_target.is_not_a_debuggable
         (!Private.up_to_date_targets_ref)  in 
       (
         Private.up_to_date_targets_ref:=new_tgts;
         Private.save_all();
       );;   
   
let forget_unregistered_file ap=
    let _=Private.recompile() in
    let _=German_forget_unregistered_file.forget ap in
    let s_ap=Absolute_path.to_string ap in  
    let (ndel,ncre)=German_created_or_deleted.update 
        ([],[Directory_name.cut_beginning German_constant.root s_ap])   
        ((!Private.recently_deleted_ref,!Private.recently_created_ref))    in 
       (
         Private.recently_deleted_ref:=ndel;
         Private.recently_created_ref:=ncre;
         Private.save_all();
       );;       
       
let forget_file ap=
   let _=Private.recompile() in
   let (new_mdata,new_dirs,new_tgts)= 
    German_forget_file.on_targets 
      (data(),directories(),(!Private.up_to_date_targets_ref)) ap in
   let s_ap=Absolute_path.to_string ap in  
   let (ndel,ncre)=German_created_or_deleted.update 
        ([],[Directory_name.cut_beginning German_constant.root s_ap])   
        ((!Private.recently_deleted_ref,!Private.recently_created_ref))    in       
       (
         Private.data_ref:=new_mdata;
         Private.directories_ref:=new_dirs;
         Private.up_to_date_targets_ref:=new_tgts;
         Private.recently_deleted_ref:=ndel;
         Private.recently_created_ref:=ncre;
         Private.save_all();
       );;         
    
let forget_module ap=
   let _=Private.recompile() in
   let ((new_mdata,new_dirs,new_tgts),short_paths)= 
    German_forget_module.on_targets
      (data(),directories(),(!Private.up_to_date_targets_ref)) ap in
   let (ndel,ncre)=German_created_or_deleted.update 
        (short_paths,[])
        ((!Private.recently_deleted_ref,!Private.recently_created_ref))    in    
       (
         Private.data_ref:=new_mdata;
         Private.directories_ref:=new_dirs;
         Private.up_to_date_targets_ref:=new_tgts;
         Private.recently_deleted_ref:=ndel;
         Private.recently_created_ref:=ncre;
         Private.save_all();
       );;         
    
 let initialize ()=
   let s_ap=Directory_name.join German_constant.root  German_constant.name_for_targetfile in
   let ap=Absolute_path.of_string s_ap in
   let the_archive=Io.read_whole_file ap in
   let 
   (
    mdata,
    directories,
    targets,
    ofiles,
    odirectories,
    dfiles,
    chfiles,
    crfiles,
    pe_types
   )=Alaskan_save_all.read_all the_archive in
   (
	Private.data_ref:= mdata;
	Private.directories_ref:= directories;
	Private.up_to_date_targets_ref:= targets;
	Private.outside_files_ref:= ofiles;
	Private.outside_directories_ref:= odirectories;
	Private.recently_deleted_ref:= dfiles;
	Private.recently_changed_ref:= chfiles;
	Private.recently_created_ref:= crfiles;
	Private.printer_equipped_types_ref:= pe_types;
  );;

   
    
 let make_module_optional old_name=
    let _=Private.recompile() in
    let ((new_mdata,new_tgts),(old_files,new_files))=
      German_make_module_optional.on_targets (data(),(!Private.up_to_date_targets_ref)) 
     old_name in
     let (ndel,ncre)=German_created_or_deleted.update 
        (old_files,new_files)
        ((!Private.recently_deleted_ref,!Private.recently_created_ref)) in   
       (
         Private.data_ref:=new_mdata;
         Private.up_to_date_targets_ref:=new_tgts;
         Private.recently_deleted_ref:=ndel;
         Private.recently_created_ref:=ncre;
         Private.save_all();
       );;          

let outside_files ()=(!Private.outside_files_ref);;
let outside_directories ()=(!Private.outside_directories_ref);;

let printer_equipped_types ()=(!Private.printer_equipped_types_ref);;

let recompile=Private.recompile;;  

let refresh ()=
  let old_outsiders=Image.image (
     fun ap->
       let s_ap=Absolute_path.to_string ap in
       Directory_name.cut_beginning German_constant.root s_ap
  ) (!(Private.outside_files_ref)) in
  let (new_mdata,new_tgts,new_outsiders,new_ptypes)=
  Alaskan_create_target_system.from_main_directory 
       German_constant.root
       (Some(German_constant.main_toplevel_name))
       old_outsiders
   in 
  let new_dirs=German_directories.from_data new_mdata in
  let new_diff=German_delchacre_from_scratch.dfs(new_mdata,new_outsiders) in
  
  (
        Private.data_ref:=new_mdata;
		Private.directories_ref:=new_dirs;
		Private.up_to_date_targets_ref:=new_tgts;
		Private.outside_files_ref:=new_outsiders;
		Private.outside_directories_ref:=[];
		Private.recently_deleted_ref:=Recently_deleted.of_string_list(Dircopy_diff.recently_deleted new_diff);
		Private.recently_changed_ref:=Recently_changed.of_string_list(Dircopy_diff.recently_changed new_diff);
		Private.recently_created_ref:=Recently_created.of_string_list(Dircopy_diff.recently_created new_diff);
		Private.printer_equipped_types_ref:=new_ptypes;
        Private.save_all();
   );;  

    
let register_mlx_file mlx=
   let _=Private.recompile() in
   let (new_mdata,new_dirs,new_tgts)= 
   Alaskan_register_mlx_file.on_targets 
    (data(),directories(),(!Private.up_to_date_targets_ref)) 
        mlx in
   let (ndel,ncre)=German_created_or_deleted.update 
        ([],[Mlx_ended_absolute_path.short_path mlx])
        ((!Private.recently_deleted_ref,!Private.recently_created_ref))    in
   let default_top=(German_data.default_toplevel new_mdata) in     
   let (_,(new_mdata2,new_tgts2))=
 	  Alaskan_make_ocaml_target.make 
 	   German_constant.root
 	  (new_mdata,new_tgts) default_top in
 	      
      (
         Private.data_ref:=new_mdata2;
         Private.directories_ref:=new_dirs;
         Private.up_to_date_targets_ref:=new_tgts2;
         Private.recently_deleted_ref:=ndel;
         Private.recently_created_ref:=ncre; 
         Private.save_all();
       ) ;;     
 
 let register_outside_file ap=
   let _=Private.recompile() in
   let (new_ofiles,new_odirs)= 
    German_register_outside_file.on_outside_directories 
     (!Private.outside_files_ref,!Private.outside_directories_ref) ap in
   let s_ap=Absolute_path.to_string ap in  
   let (ndel,ncre)=German_created_or_deleted.update 
        ([],[Directory_name.cut_beginning German_constant.root s_ap])
        ((!Private.recently_deleted_ref,!Private.recently_created_ref))    in  
       (
         Private.outside_files_ref:=new_ofiles;
         Private.outside_directories_ref:=new_odirs;
         Private.recently_deleted_ref:=ndel;
         Private.recently_created_ref:=ncre; 
         Private.save_all();
       );;        
    
 let relocate_module old_name new_subdir=
    let _=Private.recompile() in
    let ((new_mdata,new_tgts),(old_files,new_files))=
      German_relocate_module.on_targets (data(),(!Private.up_to_date_targets_ref)) 
         old_name new_subdir in
    let (ndel,ncre)=German_created_or_deleted.update 
        (old_files,new_files)
        ((!Private.recently_deleted_ref,!Private.recently_created_ref)) in
       (
         Private.data_ref:=new_mdata;
         Private.up_to_date_targets_ref:=new_tgts;
         Private.recently_deleted_ref:=ndel;
         Private.recently_created_ref:=ncre;
         Private.save_all();
         
       );;       
 
 let rename_directory (old_subdir,new_subdirname)=
    let _=Private.recompile() in
    let _=Rename_endsubdirectory.in_unix_world (old_subdir,new_subdirname) in
    let pair=(old_subdir,new_subdirname) in
    let new_data=German_rename_directory.on_data pair (!Private.data_ref)
    and new_dirs=German_rename_directory.on_subdirectories pair (!Private.directories_ref)
    and new_tgts=German_rename_directory.on_up_to_date_targets pair (!Private.up_to_date_targets_ref)
    and new_ofiles=German_rename_directory.on_outside_files pair (!Private.outside_files_ref)
    and new_odirs=German_rename_directory.on_subdirectories pair (!Private.outside_directories_ref)
    and new_rdel=German_rename_directory.on_deleted_files pair (!Private.recently_deleted_ref)
    and new_rchan=German_rename_directory.on_changed_files pair (!Private.recently_changed_ref)
    and new_rcre=German_rename_directory.on_created_files pair (!Private.recently_created_ref)
    and new_peqt=German_rename_directory.on_half_dressed_modules pair (!Private.printer_equipped_types_ref)
    in
       (
         Private.data_ref:=new_data;
         Private.directories_ref:=new_dirs;
         Private.up_to_date_targets_ref:=new_tgts;
         Private.outside_files_ref:=new_ofiles;
         Private.outside_directories_ref:=new_odirs;
         Private.recently_deleted_ref:=new_rdel;
         Private.recently_changed_ref:=new_rchan;
         Private.recently_created_ref:=new_rcre;
         Private.printer_equipped_types_ref:=new_peqt;
         Private.save_all();
       );;   
    
 let rename_module old_name new_name=
    let _=Private.recompile() in
    let ((new_mdata,new_tgts),(old_files,new_files))=
      German_rename_module.on_targets (data(),(!Private.up_to_date_targets_ref)) old_name new_name in
    let (ndel,ncre)=German_created_or_deleted.update 
        (old_files,new_files)
        ((!Private.recently_deleted_ref,!Private.recently_created_ref)) in  
       (
         Private.data_ref:=new_mdata;
         Private.up_to_date_targets_ref:=new_tgts;
         Private.recently_deleted_ref:=ndel;
         Private.recently_created_ref:=ncre;
         Private.save_all();
       );;   
   
let reposition_module hm (l_before,l_after)=
    let _=Private.recompile() in
    let new_mdata=
      Alaskan_reposition_module.rpm (data()) hm (l_before,l_after) in
       (
         Private.data_ref:=new_mdata;
         Private.save_all();
       );;      
  
 let start_debugging ()=
    let _=Private.recompile() in
    let (bowl,(new_mdata,new_tgts))=
      German_start_debugging.sd (data(),(!Private.up_to_date_targets_ref))  in
    if (not(bowl))
    then ()
    else  
       (
         Private.data_ref:=new_mdata;
         Private.up_to_date_targets_ref:=new_tgts;
         Private.save_all();
       );;   
    

    
 let save_all=Private.save_all;;   
    
 let undeclare_printer_equipped_type hm=
  (Private.printer_equipped_types_ref:=
  List.filter (fun x->x<>hm) (!Private.printer_equipped_types_ref);
  Private.save_all ());;   
    
 let unregister_mlx_file mlx=
   let _=Private.recompile() in
   let (new_mdata,new_dirs,new_tgts)= 
    German_unregister_mlx_file.on_targets (data(),(!Private.up_to_date_targets_ref)) mlx in
   let (ndel,ncre)=German_created_or_deleted.update 
        ([Mlx_ended_absolute_path.short_path mlx],[])
        ((!Private.recently_deleted_ref,!Private.recently_created_ref))    in
       (
         Private.data_ref:=new_mdata;
         Private.directories_ref:=new_dirs;
         Private.up_to_date_targets_ref:=new_tgts;
         Private.recently_deleted_ref:=ndel;
         Private.recently_created_ref:=ncre; 
         Private.save_all();
       );;  

let unregister_module mlx=
   let _=Private.recompile() in
   let ((new_mdata,new_dirs,new_tgts),short_paths)= 
    German_unregister_module.on_targets (data(),(!Private.up_to_date_targets_ref)) mlx in
   let (ndel,ncre)=German_created_or_deleted.update 
        (short_paths,[])
        ((!Private.recently_deleted_ref,!Private.recently_created_ref))    in 
       (
         Private.data_ref:=new_mdata;
         Private.directories_ref:=new_dirs;
         Private.up_to_date_targets_ref:=new_tgts;
         Private.recently_deleted_ref:=ndel;
         Private.recently_created_ref:=ncre; 
         Private.save_all();
       );;        
   
 let unregister_outside_file ap=
   let _=Private.recompile() in
   let (new_ofiles,new_odirs)= 
    German_unregister_outside_file.on_outside_directories 
     (!Private.outside_files_ref,!Private.outside_directories_ref) ap in
   let s_ap=Absolute_path.to_string ap in  
   let (ndel,ncre)=German_created_or_deleted.update 
        ([Directory_name.cut_beginning German_constant.root s_ap],[])
        ((!Private.recently_deleted_ref,!Private.recently_created_ref))    in
       (
         Private.outside_files_ref:=new_ofiles;
         Private.outside_directories_ref:=new_odirs;
         Private.recently_deleted_ref:=ndel;
         Private.recently_created_ref:=ncre; 
         Private.save_all();
       );;          
   
let up_to_date_targets ()=(!Private.up_to_date_targets_ref);;   
   

let view_definition s=
  let opt=Find_value_definition.fvd (!(Private.data_ref)) s in
  if opt=None then () else
  let itm=Option.unpack opt in
  let text="\n\n"^(Ocaml_gsyntax_item.whole itm)^"\n\n" in
  (print_string text;flush stdout);;   

   
let whole=Private.whole;;
 
 

end;;






module Overwrite_at_intervals=struct

(*

#use"overwrite_at_intervals.ml";;

*)



let inside_string replacings s=
  let n=String.length s
  and r=List.length replacings in
  let x_coord=(fun j->
    if j=1 then 1 else
    snd(fst(List.nth replacings ((j-3)/2)))+1
  ) and y_coord=(fun j->
   if j=2*r+1 then n else
    fst(fst(List.nth replacings ((j-1)/2)))-1
  ) in
  let xy_substring=(fun j->
    Cull_string.interval s (x_coord j) (y_coord j)
  ) in
  let all_parts=Ennig.doyle (
    fun j->
      if (j mod 2)=1
      then xy_substring j
      else Overwriter.to_string(snd(List.nth replacings ((j-2)/2)))
  ) 1 (2*r+1) in
  String.concat "" all_parts;;

(*

inside_string
 [(7,12),"garfield";(23,24),"jack";(30,30),"gas"]
 "12345678901234567890123456789012345678901234567890";;
 

*)

let inside_file replacings fn=
  let old_t=Io.read_whole_file fn in
  let new_t=inside_string replacings old_t in
  Io.overwrite_with fn new_t;;  
  







end;;






module Isolated_occurrences=struct

(*

#use"isolated_occurences.ml";;

Used to detect mentions of previously defined names in
the same OCaml module.

An occurrence of a substring is isolated when it 
cannot be extended to a meaningful Ocaml name. So we look at
the surrounding characters, on the left and on the right.


*)

module Private=struct

exception Unclear_left_char of char;;
exception Unclear_right_char of char;;

let rejected_left_chars=
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

let admitted_left_chars=
  [
   	'(' ; ')' ; ';' ; ' ' ;'\n';'\r';'=';'<';'>';'+';'*';'/';'-'; '.'; ',';
  ];;

let rejected_right_chars=
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

let admitted_right_chars=
  [
   	'(' ; ')' ; ';' ; ' ' ;'\n';'\r';'=';'<';'>';'+';'*';'/';'-'; '.'; ',';
  ];;

let test_for_left_admissiblity c=
   if List.mem c rejected_left_chars then false else
   if List.mem c admitted_left_chars then true else
   raise(Unclear_left_char(c));;
   
let test_for_right_admissiblity c=
   if List.mem c rejected_right_chars then false else
   if List.mem c admitted_right_chars then true else
   raise(Unclear_right_char(c));;   
   
let leftmost_small_test  s j=
   if j=0 
   then true 
   else test_for_left_admissiblity (String.get s (j-1));;

let rightmost_small_test  s j=
   if j=((String.length s)+1) 
   then true 
   else test_for_right_admissiblity (String.get s (j-1));;   
   

end;;

let of_in substr s=
  let l_substr=String.length substr 
  and n=String.length(s) in
  let main_test= (
    fun k->
      if ((String.sub s (k-1) l_substr)<>substr)
      then false
      else 
      ( Private.leftmost_small_test s (k-1) )
      &&
      ( Private.rightmost_small_test s (k+l_substr) )
      
  ) in
  Option.filter_and_unpack(
     fun k->
       if main_test k
       then Some(k,k+l_substr-1)
       else None
  ) (Ennig.ennig 1 (n+1-l_substr));;

   
(*   
   
of_in "garfield" 
"let x=garfield in let y=subgarfield and z=garfield2 in";;

of_in "garfield" "garfield is a cat";;

of_in "Boogie.Woogie.c" "48+Boogie.Woogie.c";;


*)   

end;;






module German_vague_string=struct

(*

#use"Country/Germany/german_vague_string.ml";;

*)


let to_path x=Find_suitable_ending.find_file_location 
   German_constant.root (German_wrapper.directories()) x;;

exception Absent_module of string;;

let to_module x=
  let s=Father_and_son.invasive_father x '.' in
  match (Option.find_and_stop(
      fun edg->try(Some(to_path(s^edg))) with _->None
  ) Ocaml_ending.all_string_endings) with
  None->raise(Absent_module(x))
  |Some(ap)->Half_dressed_module.of_path_and_root ap German_constant.root;;
   



end;;






module Rename_value_inside_module=struct

(*

#use"Ocaml_analysis/rename_value_inside_module.ml";;

Changes a value's name inside a module. 
This function should never be used by itself except for debugging.

*)

exception No_module_given of string;;

exception No_value_with_name of string;;

let get_module_inside_name s=
   let f=Father_and_son.father s '/' in
   if f=""
   then raise(No_module_given(s))
   else f;;
   
let rename_value_inside_module s new_name=
   let j=Substring.leftmost_index_of_in "." s in
   if j<0 
   then raise(No_module_given(s))
   else 
   let module_name=Cull_string.beginning (j-1) s in
   let hm=German_vague_string.to_module module_name 
   and path=German_vague_string.to_path module_name in 
   let temp1=German_wrapper.data() in
   let md1=Option.find (fun md->Modulesystem_data.name md=hm) temp1 in
   let temp2=(Modulesystem_data.all_ancestors md1)@[hm] in
   let all_files=Image.image  (fun hm2->
   	 Mlx_ended_absolute_path.to_path(Mlx_ended_absolute_path.join hm2 Ocaml_ending.Ml)
   ) temp2 in
   let temp3=Read_ocaml_files.read_ocaml_files all_files in
   let opt_temp4=Option.seek (fun itm->
     (itm.Ocaml_gsyntax_item.name)=s
   ) temp3 in
   if opt_temp4=None
   then raise(No_value_with_name(s))
   else
   let temp4=Option.unpack(opt_temp4) in
   let (i1,j1)=temp4.Ocaml_gsyntax_item.interval_for_name in
   let _=Overwrite_at_intervals.inside_file [(i1,j1),new_name] path in
   let temp3_again=Read_ocaml_files.read_ocaml_files all_files in
   let beheaded_name=Cull_string.cobeginning j s in
   let s_new_beheaded_name=(fun (fa,nn)->if fa="" then nn else fa^"."^nn)
   (Father_and_son.father beheaded_name '.',Overwriter.to_string new_name) in
   let new_beheaded_name=Overwriter.of_string s_new_beheaded_name in
   let s_new_full_name=module_name^"."^s_new_beheaded_name in
   let temp4_again=Option.find (fun itm->
     (itm.Ocaml_gsyntax_item.name)=s_new_full_name
   ) temp3_again in
   let k1=Listennou.find_index temp4_again temp3_again in
   let temp5=Listennou.big_tail k1 temp3_again in
   
   let temp6=Option.filter_and_unpack(
      fun itm->
        let txt=itm.Ocaml_gsyntax_item.content in
        let ttemp7=Isolated_occurrences.of_in 
           beheaded_name txt in
        if ttemp7<>[]
        then  let isoc=Isolated_occurrences.of_in beheaded_name txt in
              let replacings=Image.image (fun p->(p,new_beheaded_name)) isoc in
              let new_txt=Overwrite_at_intervals.inside_string
                   replacings txt in
             Some(itm.Ocaml_gsyntax_item.interval_for_content,
                  Overwriter.of_string new_txt)
        else None   
   ) temp5 in
   Overwrite_at_intervals.inside_file temp6 path;;
   
   
   
   
   
   
   
   
   
   
   
   
   

end;;






module Longest_shared_module=struct

(*

#use"Ocaml_analysis/longest_shared_module.ml";;

Given two ocaml items, find the longest namespace (module, in fact)
in which they are both contained.

For example, for the two values Foo.Baz.Bar.x and Foo.Baz.Barmaid.y, the 
answer is Foo.Baz.

*)

let lsm name1 name2=
  let m=min(String.length name1)(String.length name2) in
  let opt=Option.seek(fun k->
     (Strung.get name1 k)<>(Strung.get name2 k)
  )(Ennig.ennig 1 m) in
  let j1=(fun ()->if opt=None then m else (Option.unpack opt)-1)() in
  let shared_part=Cull_string.beginning j1 name1 in
  let p1=Substring.rightmost_index_of_in "." shared_part in
  if p1<0 
  then "" 
  else Cull_string.beginning (p1-1) shared_part;;
  
(*

lsm "weapon" "bag";;

lsm "May.weapon" "May.bag";;

lsm "Foo.Baz.Bar.x" "Foo.Baz.Barmaid.y";;

*)  

   


end;;






module Find_value_descendants=struct

(*

#use"Ocaml_analysis/find_value_descendants.ml";;

*)

let fvd all_ocaml_items s=
   let j1=String.index(s)('.')+1 in
   let module_name=Cull_string.beginning (j1-1) s in
   let (native_ones,foreign_ones)=
   List.partition (
      fun itm->
        let s1=Ocaml_gsyntax_item.name itm in
        let j=String.index(s1)('.')+1 in
        (Cull_string.beginning (j-1) s1)=module_name
   ) all_ocaml_items in
   let native_descendants=
   Option.filter_and_unpack(
     fun itm->
        let current_namespace=Longest_shared_module.lsm 
           s (Ocaml_gsyntax_item.name itm) in
        let l=(if current_namespace="" 
               then 0 
               else 1+(String.length current_namespace)) in   
        let shortened_name=Cull_string.cobeginning l s in
        if Isolated_occurrences.of_in shortened_name (Ocaml_gsyntax_item.content itm)<>[]
        then Some(Ocaml_gsyntax_item.name itm)
        else None   
   ) native_ones 
   and foreign_descendants=
   Option.filter_and_unpack(
     fun itm->
        if Isolated_occurrences.of_in s (Ocaml_gsyntax_item.content itm)<>[]
        then Some(Ocaml_gsyntax_item.name itm)
        else None   
   ) foreign_ones in
   (native_descendants,foreign_descendants);;
   


end;;






module Compute_all_ocaml_items=struct

(*

#use"Ocaml_analysis/compute_all_ocaml_items.ml";;

if the string argument has a dot inside it, we interpret it
as a value inside a module.
Otherwise we interpret it as a mere string.

*)


let caoi mdata=
   let temp1=List.filter Modulesystem_data.ml_present mdata in
   let temp2=Image.image (fun md->
     let hm=Modulesystem_data.name md in
     let mlx=Mlx_ended_absolute_path.join hm Ocaml_ending.ml in
     Mlx_ended_absolute_path.to_absolute_path mlx
   ) temp1 in
   Read_ocaml_files.read_ocaml_files temp2
  ;;
   
  


end;;






module German_values_in_modules=struct


(* 

#use"Country/Germany/german_values_in_modules.ml";;

*)


let replace_string mdata old_string new_string=
  let temp1=German_data.files_containing_string mdata old_string in
  let m=String.length(Directory_name.connectable_to_subpath(German_constant.root)) in
  let temp2=Image.image (fun ap->
    Cull_string.cobeginning m (Absolute_path.to_string ap)) temp1 in
  let message="\n\n The following files will be rewritten : \n\n"^
  (String.concat "\n" temp2) in
  let _=(print_string message;flush stdout) in
  List.iter (Replace_inside.replace_inside_file (old_string,new_string)) temp1;;

(*

if the string argument has a dot inside it, we interpret it
as a value inside a module.
Otherwise we interpret it as a mere string.

*)


let rename_string_or_value mdata old_name new_name=
  if not(String.contains old_name '.')
  then replace_string mdata old_name new_name
  else 
    let new_full_name=(Father_and_son.father old_name '.')^"."^new_name in
    (Rename_value_inside_module.rename_value_inside_module 
            old_name (Overwriter.of_string new_name); 
     replace_string mdata old_name new_full_name
    );;


let list_values_from_module_in_file module_name file=
   let s=Io.read_whole_file file in
   let temp1=Look_for_module_names.indices_in_file file in
   let temp2=List.filter (fun (t,(i,j))->
     (t=My_str_example.index_for_pointed_case)&&
     (Cull_string.interval s i j=(String.capitalize_ascii module_name))
   ) temp1 in
   let temp3=Image.image(fun (t,(i,j))->
    let opt=After.after_star 
     Charset.strictly_alphanumeric_characters
     s (j+2) in
    let end_idx=(match opt with Some(k)->k-1 |None->String.length s) in
     Cull_string.interval s (j+2) end_idx
   ) temp2 in
   Ordered_string.diforchan temp3;;

let list_values_from_module_in_modulesystem module_name mdata=
   let temp1=Alaskan_data.all_mlx_paths mdata in
   let temp2=Image.image (fun ap->
    let ttemp1=list_values_from_module_in_file module_name ap in
    Ordered_string.image (fun x->(x,ap) ) ttemp1
    ) temp1 in
   let temp3=List.flatten temp2 in
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
   let temp1=Substring.occurrences_of_in t s in
   Image.image (fun j->Cull_string.closeup_around_index 
      s j
   ) temp1;; 
 
 let modules_using_value mdata value_name =
   Option.filter_and_unpack (fun md->
    let ap=Modulesystem_data.principal_path md in
    if (list_value_occurrences_in_file value_name ap)<>[] 
    then Some(Modulesystem_data.name md)
    else None ) mdata;;
 
 


let show_value_occurrences_in_modulesystem t mdata=
   let m=String.length(Directory_name.connectable_to_subpath(German_constant.root)) in
   let temp1=Alaskan_data.all_mlx_paths mdata in
   let temp2=Image.image (fun ap->
    let ttemp1=list_value_occurrences_in_file t ap in
    let mname=Cull_string.cobeginning(m)(Absolute_path.to_string ap) in
    Image.image (fun x->mname^":\n"^x ) ttemp1
    ) temp1 in
   let temp3=List.flatten temp2 in
   let temp4=String.concat "\n\n\n" (""::temp3@[""]) in 
   print_string temp4;;





end;;






module German_update_copied_compiler=struct

(*

#use"Country/Germany/german_update_copied_compiler.ml";;

*)

let prepare destdir=
  let l1=Alaskan_data.all_short_paths (German_wrapper.data()) in
  let l2=Image.image (
    fun ap->
      let s_ap=Absolute_path.to_string ap in
      Directory_name.cut_beginning German_constant.root s_ap
  ) (German_wrapper.outside_files()) in
  let main_diff=Prepare_dircopy_update.compute_diff 
        (German_constant.root,l1@l2) destdir in
  Prepare_dircopy_update.commands_for_update destdir main_diff;;

let prepare_special_file destdir filename=
  let the_file=Absolute_path.create_file(Directory_name.join destdir filename) in
  Replace_inside.replace_inside_file
  (Directory_name.connectable_to_subpath German_constant.root,
   Directory_name.connectable_to_subpath destdir)
  the_file;;


    
let ucc destdir=
  let s_dir=Directory_name.connectable_to_subpath destdir in 
  let _=Unix_command.uc ("mkdir -p "^s_dir^"_build") in
  let _=Image.image Unix_command.uc (prepare destdir) in
  let _=Image.image (prepare_special_file destdir)
    ["my_pervasives.ml";"my_printers.ml";"my_loadings.ml"]
   in 
  Alaskan_create_target_system.from_main_directory destdir None [];;
       
       






   
   
  

end;;






module German_pervasives=struct

(*

#use"Country/Germany/german_pervasives.ml";;

*)


let cdir=German_constant.root;;

let s_cdir=Directory_name.connectable_to_subpath cdir;;
let current_registered_directories ()=German_wrapper.directories();;

let current_directories()=
  let temp1=List.filter (
    fun x->x<>Subdirectory.SD ""
  ) (current_registered_directories()) in
  (Subdirectory.SD "")::(temp1@
  [Subdirectory.SD "Remembered";Subdirectory.SD "Forgotten"]);;

let fl=German_vague_string.to_path;;
let hmx=German_vague_string.to_module;;

let fmr x=Alaskan_data.find_module_registration (German_wrapper.data()) (hmx x);;
let abo x=German_data.above (German_wrapper.data()) (hmx x);;
let bel x=German_data.below (German_wrapper.data()) (hmx x);;
let dbel x=German_data.directly_below (German_wrapper.data()) (hmx x);;


let ren_without_backup x y=German_wrapper.rename_module (hmx x) (No_slashes.of_string y);;
let relo_without_backup x y=German_wrapper.relocate_module (hmx x) y;;
let mmo_without_backup x=German_wrapper.make_module_optional (hmx x) ;;



let fg_without_backup x=
   if String.contains x '.'
   then German_wrapper.forget_file (fl x)
   else German_wrapper.forget_module (hmx x);;

let regi_without_backup x= 
  let mlx=Mlx_ended_absolute_path.of_path_and_root (fl x) cdir in
  German_wrapper.register_mlx_file mlx;;

let rego_without_backup x=
   German_wrapper.register_outside_file (Absolute_path.of_string x);;

let ureg_without_backup x=
  if List.exists (fun edg->Substring.ends_with x edg) [".ml";".mll";".mly"] 
  then let mlx=Mlx_ended_absolute_path.of_path_and_root (fl x) cdir in
       German_wrapper.unregister_mlx_file mlx 
  else German_wrapper.unregister_module (hmx x);;

let double_semicolon=";"^";";;

let cf t1 t2=
   let ap1=fl t1 in
   let s_ap1=Absolute_path.to_string ap1 in
   let s_ap2=(Father_and_son.invasive_father s_ap1 '/')^"/"^t2^".ml" in
   let _=Unix_command.uc ("cp "^s_ap1^" "^s_ap2) in
   let ap2=Absolute_path.of_string s_ap2 in
   let s1=Cull_string.cobeginning (String.length s_cdir) s_ap1
   and s2=Cull_string.cobeginning (String.length s_cdir) s_ap2 in
   let txt1="#use\""^s1^"\""^double_semicolon
   and txt2="#use\""^s2^"\""^double_semicolon in
   let _=Replace_inside.replace_inside_file 
    (txt1,txt2) ap2  in 
   Unix_command.uc ("open -a \"/Applications/Visual Studio Code.app\" "^s_ap2);;   

let vo s=
  let temp1=Find_suitable_ending.find_file_location cdir (current_directories()) s in
  let s1=Absolute_path.to_string temp1 in
  Unix_command.uc ("open -a \"/Applications/Visual Studio Code.app\" "^s1);;


let syz()=German_data.system_size (German_wrapper.data());;

let init=German_wrapper.initialize;;

let reco_without_backup ()=
  German_wrapper.recompile();;


let pbk ()=Dircopy_diff.display(German_wrapper.diff());;
let bk=German_backup_target_system.backup_with_message (German_wrapper.diff());;

let rd ()=Alaskan_remove_debuggables.rd German_constant.root (German_wrapper.data());;
let sd=German_wrapper.start_debugging;;


let rv_without_backup x y=German_values_in_modules.rename_string_or_value (German_wrapper.data()) x y;;
let srv_without_backup x y=German_values_in_modules.replace_string (German_wrapper.data()) x y;;


let sv wal=German_values_in_modules.show_value_occurrences_in_modulesystem wal (German_wrapper.data()) ;;
let vfm modname =German_values_in_modules.list_values_from_module_in_modulesystem modname (German_wrapper.data()) ;;
let muv x=German_values_in_modules.modules_using_value (German_wrapper.data()) x;;

let ed =German_wrapper.end_debugging;;


let vd=German_wrapper.view_definition;;
let fvd=Find_value_descendants.fvd 
  (Compute_all_ocaml_items.caoi(German_wrapper.data())) ;;

let rsh_without_backup=German_wrapper.refresh;;

let am ()=
    let temp1=German_wrapper.data() in
    let temp2=Image.image (fun md->
     Naked_module.to_string(
       Half_dressed_module.naked_module(Modulesystem_data.name md))
    ) temp1 in
    temp2;;
    
let tw x=
  let hm=hmx x in
  let s_hm=Half_dressed_module.to_string hm in
  let fn=(Directory_name.connectable_to_subpath(cdir))^s_hm in    
  Sys.command ("open -a /Applications/TextWrangler.app "^fn^".ml");;

let oim ()=German_data.outdated_interesting_modules (German_wrapper.data());;
let df () =German_data.deletable_files (German_wrapper.data());;
let ucc ()=German_update_copied_compiler.ucc 
 (Directory_name.of_string "/Users/ewandelanoy/Documents/OCaml/Cherokee");;

let reco ()=let bowl=reco_without_backup () in (if bowl then German_wrapper.backup None);;
let reco_with_comment s=let bowl=reco_without_backup () in (if bowl then German_wrapper.backup (Some s));;


let fg x=(fg_without_backup x;German_wrapper.backup None);;
let mmo x=(mmo_without_backup x;reco());;

let regi x=(regi_without_backup x;German_wrapper.backup None);;
let rndir p=(German_wrapper.rename_directory p;reco());;

let rego x=(rego_without_backup x;reco());;
let relo x y=(relo_without_backup x y;reco());;
let ren  x y=(ren_without_backup  x y;reco());;
let rsh ()=(rsh_without_backup ();German_wrapper.backup None);;
let rwc =reco_with_comment;;
let rv x y=(rv_without_backup x y;reco());;
let srv x y=(srv_without_backup x y;reco());;
let ureg x=(ureg_without_backup x;reco());;





end;;

