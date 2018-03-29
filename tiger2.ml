(*

#use"tiger2.ml";;

*)



exception Redundancy of (int*int*int) list;;

let check_for_redundancies l= 
    let temp1=Listennou.universal_delta_list l in
    let temp2=Option.filter_and_unpack (fun ((x1,y1),(x2,y2))->
      if (x1=x2)&&(y1<>y2) then Some(x1,y1,y2) else None
    ) temp1 in
    if temp2<>[]
    then raise(Redundancy(temp2))
    else ();;

exception Noninjectivity of ((int*int*int) list)*((int*int) list);;    

let check_for_noninjectivity old_l= 
        let temp3=Image.image (fun (x,y)->(y,x)) old_l in
        let temp4=Tidel2.forget_order(Tidel2.diforchan temp3) in
        let temp1=Listennou.universal_delta_list temp4 in
        let temp2=Option.filter_and_unpack (fun ((x1,y1),(x2,y2))->
          if (x1=x2)&&(y1<>y2) then Some(y1,y2,x1) else None
        ) temp1 in
        if temp2<>[]
        then raise(Noninjectivity(temp2,old_l))
        else ();;
    

exception Badminton of (((int*int*int*int) list)*((int*int) list) );; 

let sons tl=
    let l=Tidel2.forget_order tl in
    let temp1=Image.image (fun (a,b)->
        let opt1=Option.find_and_stop (
        fun (x,y)->if x=b then Some(y) else None
        ) l 
        and opt2=Option.find_and_stop(
            fun (x,y)->if x=a-1 then Some(y) else None
            ) l in
      (b,a-1,opt1,opt2)) l in
    let temp2=Option.filter_and_unpack (
     fun (c,d,opt1,opt2)->
       if (opt1=None) || (opt2=None) then None else
       let v1=Option.unpack opt1
       and v2=Option.unpack opt2 in
       if v1=v2+1 then None else
       Some(c,d,v1,v2)
    ) temp1 in
    if temp2<>[]
    then raise(Badminton(temp2,l))
    else 
    let temp3=Option.filter_and_unpack (
        fun (c,d,opt1,opt2)->
         if (opt1=None) && (opt2<>None)
         then Some(c,(Option.unpack opt2)+1)
         else 
         if (opt1<>None) && (opt2=None)
         then Some(d,(Option.unpack opt1)-1)
         else None
    ) temp1 in
    let temp4=Tidel2.diforchan temp3 in
    let temp5=Tidel2.forget_order temp4 in
    let _=check_for_redundancies temp5 in
    temp4;;

type big=(int, int) Tidel2.set list * (int, int) Tidel2.set;;

let rec sons2=((fun x->
   let (cut,uncut)=x in
   let temp1=sons uncut in
   if Tidel.length temp1=0 
   then x
   else 
   let temp2=Tidel2.teuzin temp1 uncut in
   let temp3=Tidel2.forget_order temp2 in
   let _=check_for_noninjectivity temp3 in
   sons2(temp1::cut,temp2)):big -> big);; 

let sons3=((fun x->
   let (cut,uncut)=x in
   let temp1=sons uncut in
   if Tidel.length temp1=0 
   then x
   else 
   let temp2=Tidel2.teuzin temp1 uncut in
   (temp1::cut,temp2)):big -> big);; 

exception Bad_insertion of int*int*int;;   

let insert_carefully (x,y) tl=
    let l=Tidel2.forget_order tl in
    match Option.seek (fun (x1,y1)->x1=x) l with
    Some(_,y1)->if y1=y then tl else raise(Bad_insertion(x,y,y1))
    |None->Tidel.insert (x,y) tl;;

let w0=Tidel2.singleton (0,1);;    

let walker=ref(([w0],w0):big);;

let restart ()=(walker:=([w0],w0));;

let imp new_pair=
    let (cut,uncut)=(!walker) in
     let temp1=insert_carefully new_pair uncut in
     let temp2=((Tidel2.singleton new_pair)::cut,temp1) in
     let temp3=sons2 temp2 in
     let _=(walker:=temp3) in
     temp3;;

restart();;     
imp (-1,4);;
imp (-2,7);;
imp (-3,14);;



(*

let g1=([],Ordered.S [(-3, 12); (-2, 7); (-1, 4); (0, 1); (1, 5); (2, 9); 
 (3,100); (4, 8); (5, 2);
     (7, 13); (9, 6)]);;
let ff=Memoized.small sons3 g1;;


*)


    