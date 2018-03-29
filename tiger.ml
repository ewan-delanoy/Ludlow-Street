(*

#use"tiger.ml";;

*)

let peggy_counter=ref(1);;

let peggy_get_counter ()=
   let j=(!peggy_counter+1) in
   let _=(peggy_counter:=j) in j;;

let reference_for_peggy=ref ([]:((int * int) * (int * int)) list);;

exception Conflict of (int*int)*(int*int)*(int*int);;

let peggy_add_one_rel (a,b)=
   let old_ref=(!reference_for_peggy) in
   match Option.seek(fun (x,y)->x=a) old_ref with
   Some((_,b0))->if b0=b then () else raise(Conflict(a,b0,b))
   |None->(reference_for_peggy:=((a,b)::old_ref));;

let rec peggy_add_several_rels da_ober=match da_ober with
   []->()
   |u::peurrest->(peggy_add_one_rel u; peggy_add_several_rels peurrest);;

let imp a b=peggy_add_one_rel (a,b);;

let peggy_eval (i,j)=
  match Option.seek(fun (x,y)->x=(i,j)) (!reference_for_peggy) with
  Some((_,y0))->("V",y0)
   |None->("F",(i,j));;

let peggy_double_eval (i1,j1)=
    let opt1=Option.seek(fun (x,_)->x=(i1,j1)) (!reference_for_peggy) in
    if opt1=None then ("FF",(i1,j1)) else
    let (i2,j2)=snd(Option.unpack opt1) in
    let opt2=Option.seek(fun (x,_)->x=(i2,j2)) (!reference_for_peggy) in
    if opt2=None then ("F",(i2,j2)) else
    let (i3,j3)=snd(Option.unpack opt2) in 
    ("V",(i3,j3));;

let peggy_check ( (v1,(i1,j1)),(v2,(i2,j2)) )= 
    (v1="V") && (v2="V") && (i1=i2) && (j1=j2+1);;    

let peggy_compute (i,j)=
    (peggy_double_eval (i,j),peggy_eval (i,j-1));;

let peggy_visualize (i1,j1) (i2,j2)=
    let si1=string_of_int i1 
    and si2=string_of_int j1
    and si3=string_of_int i2
    and si4=string_of_int j2 in
    let msg="\n Imposing : f("^si1^","^si2^")=("^si3^","^si4^")\n" in
    (print_string msg; flush stdout) ;;

let see ()=
    let temp1=List.rev_map (fun (a,b)->peggy_compute a) (!reference_for_peggy) in
    List.filter (fun x->not(peggy_check x)) temp1;;

let jmp x y=let _=imp x y in see();;

let one_more_step ()=
   let ( (_,(i1,j1)),(v2,(i2,j2)) )=List.hd( see()) in
   let (beg_x,end_x)=(
       if v2="V" then ((i1,j1),(i2,j2+1)) else
       if v2="F" then ((i2,j2),(peggy_get_counter(),0)) else
       failwith("Tear in your hand")
   ) in
   (peggy_visualize beg_x end_x;imp beg_x end_x;see());;


jmp (1,0) (peggy_get_counter(),0);;   

let fibonacci =Memoized.recursive(fun old_f n->
   if n<2 then 1 else
   (old_f(n-1))+ (old_f(n-2))
);;

let alf=function
  1->0 |2|3->2 |k->((k+3)/2);;

let beth=function
  1->1 |k->k-1;;  

let gam (j,minus_k)=
   let k=(-minus_k) in  
  (alf j)*(fibonacci k)+(beth j)*(fibonacci (k+1))+1;;

(*

for k=1 to 2000 do one_more_step() done;;

let u1=(!reference_for_peggy);;

let u2=List.filter ( fun ((x1,x2),y)->x2<0 ) u1;;

let u3=List.filter ( fun (x,y)->y<>(gam x,0) ) u2;;

let u4=Min.minimize_it_with_care ( fun ((x1,x2),y)-> x1) u3;;

let old_ff i=List.filter (fun ((x1,x2),y)->(x1=i)) u1;;
let ff i=
    let temp1=old_ff i in
    let temp2=Image.image (fun ((x1,x2),y)->(x2,y)) temp1 in
    let temp3=Tidel2.diforchan temp2 in
    Tidel2.forget_order temp3;;



let alf=function
  1->0 |2->

*)
















