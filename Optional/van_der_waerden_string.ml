




type fundamental_parameter=int;;

type vdw_bytes_size=int;;

type t=S of fundamental_parameter*bytes;;

let unveil (S(r,s))=(r,s);;

let content (S(r,s))=s;;

let length (S(r,s))=Bytes.length s;;

let pairs_to_be_checked (r:fundamental_parameter) (n: vdw_bytes_size) i=
   let mr=min r in
   let temp1=Ennig.doyle(fun t->(i-2*t,i-t))(1)(mr((i-1)/2))
   and temp2=Ennig.doyle(fun t->(i-t,i+t))(1)(mr(min(i-1)(n-i)))
   and temp3=Ennig.doyle(fun t->(i+t,i+2*t))(1)(mr((n-i)/2))
   in
   temp1@temp2@temp3;;
   
   

let check_pair s (i,j)=
  (*we assume that s has been correctly constructed, so (si,sj)<>(1,1) *)
  let s_copy=(s) in
  if (Bytes.get s (i-1)='1')&&((Bytes.get s (j-1))<>'0')
  then let _=Bytes.set s_copy (j-1) '0' in
       s_copy
  else 
  if (Bytes.get s (j-1)='1')&&((Bytes.get s (i-1))<>'0')
  then let _=Bytes.set s_copy (i-1) '0' in
       s_copy
  else s_copy;;
  

  
let check_pairs s l=
 if l=[] then  s else
 List.fold_left   check_pair s l;;


let left_watchers (r:fundamental_parameter) s m=
  let n=Bytes.length(s) in
  let temp1=Ennig.doyle(fun t->(m+t,m+2*t))(max(1)(1-m))(min(r)((n-m)/2)) in
  Image.image (fun (i,j)->(Bytes.get s (i-1),Bytes.get s (j-1)) ) temp1;; 

let general_left_extensiblity_test(r:fundamental_parameter) s m=
   List.for_all (fun (si,sj)->
      (si,sj)<>('1','1')
   ) (left_watchers r s m);;


let right_watchers (r:fundamental_parameter) s m=
  let n=Bytes.length(s) in
  let temp1=Ennig.doyle(fun t->(m-2*t,m-t))(max(1)(m-n))(min(r)((m-1)/2)) in
  Image.image (fun (i,j)->(Bytes.get s (i-1),Bytes.get s (j-1)) ) temp1;; 

let general_right_extensiblity_test(r:fundamental_parameter) s m=
   List.for_all (fun (si,sj)->
      (si,sj)<>('1','1')
   ) (right_watchers r s m);;

let is_extensible (r:fundamental_parameter) s=
  general_right_extensiblity_test r s (Bytes.length(s)+1);;

let measure_extensibility (r:fundamental_parameter) s=
  let rec tempf=(fun (k,da_ober)->match da_ober with
      []->k
      |(a,b)::peurrest->
         if (a,b)=('1','1') then 1 else
         if ((a,b)=('1','F'))||
            ((a,b)=('F','1')) then tempf(2,peurrest) else
         tempf(k,peurrest)   
  ) in
  tempf(3,right_watchers r s (Bytes.length(s)+1));;
 
let bos s=Bytes.of_string s;; 
let adj b s=Bytes.cat b (Bytes.of_string s);; 
 
let all_extensions (S(r,s))=
   match measure_extensibility r s with
    1->[S(r,adj s "0")]
   |2->[S(r,adj s "0");S(r,adj s "F")]
   |3->[S(r,adj s "0");S(r,adj s "1");S(r,adj s "F")]
   |arall->failwith("Im a backdoor man");;
  
let create (r:fundamental_parameter) (n: vdw_bytes_size)=S(r,Bytes.make n 'F');;

let set_to_white vdw_s j=
  let (S(r,s))=vdw_s in
  let n=Bytes.length(s) in
  if (j<1)||(j>n) then vdw_s else
  let zs=Bytes.to_string s in
  let old_c=Bytes.get s (j-1) in
  if old_c='0' 
  then let _=(print_string ("\nForbidden move : s["^(string_of_int j)^"]<-1 for s="^zs^"\n");
        flush stdout) in
        vdw_s
  else 
  if old_c='1' 
  then let _=(print_string ("\nRedundant move : s["^(string_of_int j)^"]<-1 for s="^zs^"\n");
        flush stdout) in
        vdw_s
  else 
  let temp1=pairs_to_be_checked r n j in
  let new_s=check_pairs s temp1 in
  let _=Bytes.set new_s (j-1) '1' in
  S(r,new_s);;
  
let set_to_black vdw_s j=  
  let (S(r,s))=vdw_s in
  let n=Bytes.length(s) in
  if (j<1)||(j>n) then vdw_s else
  let old_c=Bytes.get s (j-1) in
  let zs=Bytes.to_string s in
  if old_c='1' 
  then let _=(print_string ("\nForbidden move : s["^(string_of_int j)^"]<-0 for s="^zs^"\n");
        flush stdout) in
        vdw_s
  else 
  if old_c='0' 
  then let _=(print_string ("\nRedundant move : s["^(string_of_int j)^"]<-0 for s="^zs^"\n");
        flush stdout) in
        vdw_s
  else 
  let new_s= s in
  let _=Bytes.set new_s (j-1) '0' in
  S(r,new_s);;  
  
let make_move vdw_s (j,c)=
  if c=0 then set_to_black vdw_s j else
  if c=1 then set_to_white vdw_s j else
  vdw_s;;
  
  
let first_free_cell (S(r,s))= 
   let n=Bytes.length(s) in
   Option.seek 
   (fun j->let c=Bytes.get(s)(j-1) in (c<>'0')&&(c<>'1')) 
   (Ennig.ennig 1 n);;

let rec fill_greedily vdw_s=
  match first_free_cell vdw_s with
  None->vdw_s
  |Some(j0)->
     let new_vdw_s=set_to_white vdw_s j0 in
     fill_greedily new_vdw_s;;

let uncurried_sphere =
  Memoized.recursive(fun old_f ((r:fundamental_parameter),n)->
   if n<1 then [S(r,bos "")] else
   let temp1=old_f(r,n-1) in
   let temp2=Image.image all_extensions temp1 in
   List.flatten temp2
  );;
     
let sphere (r:fundamental_parameter) n=uncurried_sphere(r,n);;
  
let uncurried_ball =
  Memoized.make(fun ((r:fundamental_parameter),n)->
   let temp1=Ennig.doyle (sphere r) 0 n in
   List.flatten temp1
  );;

let ball (r:fundamental_parameter) n=uncurried_ball(r,n);;


 let hard_measure (S(r,s))= 
   let n=Bytes.length(s) in
   List.length(List.filter (fun j->Bytes.get s (j-1)='1') (Ennig.ennig 1 n));;

exception Beginning_failure;;

let beginning k s=
   if k<1 then bos "" else
   let n=Bytes.length(s) in
   if (k>n)
   then raise(Beginning_failure)
   else Bytes.sub s 0 k;;

exception Ending_failure;;   
   
 let ending k s=
   if k<1 then bos "" else
   let n=Bytes.length(s) in
   if (k>n)
   then raise(Ending_failure)
   else Bytes.sub s (n-k) k;;
    

let cobeginning k s=ending (Bytes.length(s)-k) s;; 

let duracell_decomposition vdw_s=
 let (S(r,s))=vdw_s in
  match first_free_cell vdw_s with
  None->(s,S(r,bos ""))
  |Some(j0)->(beginning (j0-1) s,S(r,cobeginning (j0-1) s) );;

let checks_in_generic_construction (r:fundamental_parameter) s ones accu=
  let n=Bytes.length s in
  let tempf1=(fun (i,j)->
     let t=j-i in
     let x=i-t in
     if (t>r)||(x<1) then () else
     let c=Bytes.get s (x-1) in
     if c='1' 
     then let sx=string_of_int x
          and si=string_of_int i
          and sj=string_of_int j in
          failwith("Problem with s["^sx^"],s["^si^"],s["^sj^"]")
     else 
     if c='F' 
     then accu:=(x,(i,j))::(!accu)
     else ()
  )
  and tempf2=(fun (i,j)->
     let t=j-i in
     let x=j+t in
     if (t>r)||(x>n) then () else
     let c=Bytes.get s (x-1) in
     if c='1' 
     then let sx=string_of_int x
          and si=string_of_int i
          and sj=string_of_int j in
          failwith("Problem with s["^si^"],s["^sj^"],s["^sx^"]")
     else 
     if c='F' 
     then accu:=(x,(i,j))::(!accu)
     else ()
  )
   and tempf3=(fun (i,j)->
     let d=j-i in
     if (d mod 2)>0 then () else
     let t=d/2 in
     let x=i+t in
     if (t>r) then () else
     let c=Bytes.get s (x-1) in
     if c='1' 
     then let sx=string_of_int x
          and si=string_of_int i
          and sj=string_of_int j in
          failwith("Problem with s["^si^"],s["^sx^"],s["^sj^"]")
     else 
     if c='F' 
     then accu:=(x,(i,j))::(!accu)
     else ()
  ) in
  (List.iter tempf1 ones;List.iter tempf2 ones;List.iter tempf3 ones);;
  
let construct (r:fundamental_parameter) s=
  let n=Bytes.length(s) in
  let temp0=List.filter(fun j->Bytes.get s (j-1)='1')(Ennig.ennig 1 n)
  and accu=ref([]) in
  let ones=Uple.list_of_pairs temp0 in
  let _=checks_in_generic_construction r s ones accu in
  let corrections=Ordered.forget_order(Tidel2.diforchan(!accu)) in
  if corrections=[] then S(r,s) else 
  let _=List.iter(fun (x,(i,j))->Bytes.set s (x-1) '0') corrections in
  let temp1=Image.image(fun (x,(i,j))->
    (string_of_int x,string_of_int i,string_of_int j)
  )(corrections) in
  let temp2=Image.image (fun (sx,si,sj)->
    "s["^sx^"]<-0, because of s["^si^"] and s["^sj^"]."
  ) temp1 in
  let temp3="\n\n\n Some corrections : \n\n"^(String.concat "\n" temp2)^"\n\n\n" in
  let _=(print_string temp3;flush stdout) in
  S(r,s);;
  
let construct_discreetly0 (r:fundamental_parameter) s=
  let n=Bytes.length(s) in
  let temp0=List.filter(fun j->Bytes.get s (j-1)='1')(Ennig.ennig 1 n)
  and accu=ref([]) in
  let ones=Uple.list_of_pairs temp0 in
  let _=checks_in_generic_construction r s ones accu in
  let corrections=Ordered.forget_order(Tidel2.diforchan(!accu)) in
  if corrections=[] then Some(s) else 
  let s_copy= s in
  let _=List.iter(fun (x,(i,j))->Bytes.set s_copy (x-1) '0') corrections in
  Some s_copy;;
  
let construct_discreetly (r:fundamental_parameter) s=
  try construct_discreetly0 r s with _->
  None;;
   
let begins_with x y=
      let ly=Bytes.length(y) in
      if Bytes.length(x)<ly
      then false
      else (Bytes.sub x 0 ly)=y;;   
   
   
let concat a (S(r,b))=
  match construct_discreetly r (Bytes.cat a b) with
  None->None
  |Some(w)->if begins_with w a
                  then Some(S(r,w))
                  else None;;
 
let add_blanks_on_the_right (S(r,s)) b=
  let n=Bytes.length(s) in
  let temp1=List.filter (fun j->not(general_right_extensiblity_test r s (n+j))) (Ennig.ennig 1 b) in
  let rightmost_part=Bytes.make b 'F' in
  let _=List.iter (fun j->Bytes.set rightmost_part (j-1) '0') temp1 in
  S(r,Bytes.cat s rightmost_part);;



let add_blanks_repeatedly_on_the_right (S(r,s)) b=
  let n=Bytes.length(s) in
  let temp1=List.filter (fun j->not(general_right_extensiblity_test r s (n+j))) (Ennig.ennig 1 b) in
  let rightmost_part=Bytes.make b 'F' in
  let _=List.iter (fun j->Bytes.set rightmost_part (j-1) '0') temp1 in
  Ennig.doyle (fun j->S(r,Bytes.cat s (beginning j rightmost_part))) 1 b;;


let add_blanks_on_the_left (S(r,s)) b=
  let temp1=List.filter (fun j->not(general_left_extensiblity_test r s (1-j))) (Ennig.ennig 1 b) in
  let leftmost_part=Bytes.make b 'F' in
  let _=List.iter (fun j->Bytes.set leftmost_part (b-j) '0') temp1 in
  S(r,Bytes.cat leftmost_part s);;



let add_blanks_repeatedly_on_the_left (S(r,s)) b=
  let temp1=List.filter (fun j->not(general_left_extensiblity_test r s (1-j))) (Ennig.ennig 1 b) in
  let leftmost_part=Bytes.make b 'F' in
  let _=List.iter (fun j->Bytes.set leftmost_part (b-j) '0') temp1 in
  Ennig.doyle (fun j->S(r,Bytes.cat (ending j leftmost_part) s )) 1 b;;


let resize_on_the_left (S(r,s))=
  let n=Bytes.length(s) and intended_n=2*r in
  let d=intended_n-n in
  if (d<0) then S(r,ending intended_n s) else
  if (d=0) then S(r,s) else
  add_blanks_on_the_left (S(r,s)) d;;

let is_weakly_admissible (r:fundamental_parameter) s=
   let n=Bytes.length(s) in
   let temp1=List.filter (fun i->Bytes.get s (i-1)='1') (Ennig.ennig 1 n) in
   let temp2=Uple.list_of_triples temp1 in
   List.for_all 
   (fun (x,y,z)->
      let d=y-x in (z-y<>d)||(d>r)
   )
   temp2;;
   
let is_strongly_admissible (r:fundamental_parameter) s=
   match construct_discreetly r s with
   None->false
   |Some(t)->t=s;;
   
let strongly_admissible_extensions (r:fundamental_parameter) l=
   let temp1=Cartesian.product l (Image.image bos ["0";"1";"F"]) in
   let temp2=Image.image (fun (x,y)->Bytes.cat x y) temp1 in
   let temp3=List.filter (is_strongly_admissible r) temp2 in
   temp3;;
   

let pair_is_weakly_concatable (S(r1,s1)) (S(r2,s2))=
  if r1<>r2
  then failwith("No concat possible")
  else is_weakly_admissible r1 (Bytes.cat s1 s2);;


let solve_naively=Memoized.recursive(fun old_f vdw_s->
   match first_free_cell vdw_s with
   None->(hard_measure vdw_s,[vdw_s])
   |Some(j0)->
      let vdw_s1=set_to_black vdw_s j0
      and vdw_s2=set_to_white vdw_s j0 in
      let (m1,l1)=old_f(vdw_s1)
      and (m2,l2)=old_f(vdw_s2) in
      if m1<m2 then (m2,l2) else
      if m2<m1 then (m1,l1) else
      (m1,l1@l2)
);;


let all_realizations=Memoized.recursive(fun old_f vdw_s->
   match first_free_cell vdw_s with
   None->[vdw_s]
   |Some(j0)->
      let vdw_s1=set_to_black vdw_s j0
      and vdw_s2=set_to_white vdw_s j0 in
      let l1=old_f(vdw_s1)
      and l2=old_f(vdw_s2) in
      l1@l2
);;


let graded_realization=Memoized.make(fun (vdw_s,j)->
   let p=fst(solve_naively vdw_s)
   and temp1=all_realizations vdw_s in
   List.filter (fun t->hard_measure t=p-j) temp1
);;

let direct_decompositions=Memoized.make(fun (S(r,s))->
   let h=(fun t->fst(solve_naively(S(r,t))) ) 
   and n=Bytes.length(s) in
   let c0=h(s) in
   let tester=(fun j->
     let beg=beginning j s
     and cobeg=cobeginning j s in
     h(beg)+h(cobeg)=c0
   ) in
   List.filter tester (Ennig.ennig 1 (n-1))
);;


let shadow_on_the_left (r:fundamental_parameter) p s=
   if Bytes.length(s)<p then failwith("bytes too short") else
   let s1=Option.unpack(construct_discreetly(r)(s)) in
   let temp1=S(r,s1) in
   let temp2=snd(solve_naively(temp1)) in
   let temp3=Image.image (fun vdw_s->beginning p (content vdw_s)) temp2 in
   let z_temp3=Image.image Bytes.to_string temp3 in
   let z_temp4=Ordered.forget_order(Ordered_string.diforchan z_temp3) in
   let temp4=Image.image Bytes.of_string z_temp4 in
   Ordered.S(temp4);;
   
type bytes_set_set=bytes Ordered_bare_set.set2;;   
   
let dances_for_spectator_on_the_right (r:fundamental_parameter) s l=
   let srs=S(r,s) and p=Bytes.length(s) in
   let tempf=(fun w->
     if pair_is_weakly_concatable srs (S(r,w))
     then Some(shadow_on_the_left r p (Bytes.cat s w))
     else None
   ) in
   let temp1=Option.filter_and_unpack tempf l in
   let temp2=Ordered_bare_set.diforchan temp1 in
   (temp2:bytes_set_set);;


 let print_out (dummy:Format.formatter) (S(n,x))=
   Format.open_box 0;
   Format.print_string(Bytes.to_string x);
   Format.close_box();;

  
