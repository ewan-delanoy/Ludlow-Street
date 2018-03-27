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





   
  
