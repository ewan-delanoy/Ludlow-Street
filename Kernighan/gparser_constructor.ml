(*

#use"Kernighan/gparser_constructor.ml";;

*)



let enclosure (left_encloser,right_encloser)=
   let descr=Gparser_description.enclosure (left_encloser,right_encloser) in
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
               descr
               (i1,i4)
               [i2,i3-1]
               (i4+1)
               None in
   Some(res)) in
   Gparser.veil descr tempf;;
   
let constant t=
   let descr=Gparser_description.constant t in
   let tempf=(fun s i1->
   if (not(Substring.is_a_substring_located_at t s i1))
   then None
   else 
   let i2=i1+(String.length t) in
   let res= Gparser_result.veil
               descr
               (i1,i2-1)
               []
               i2
               None in
   Some(res)) in
   Gparser.veil descr tempf;;

let simple_star t=
   let descr=Gparser_description.simple_star t in
   let lc=Strung.explode t in
   let tempf=(fun s i1->
        let j=Strung.finder (fun c->not(List.mem c lc)) s i1 in
        let better_j=(if j<1 then (String.length s)+1 else j) in
        let res=Gparser_result.veil
               descr
               (i1,better_j-1)
               []
               better_j
               None in
   Some(res)) in
   Gparser.veil descr tempf;;

let race (continuer,finalizer)=
   let descr=Gparser_description.race (continuer,finalizer) in
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
               descr
               (i1,j1-1)
               []
               j1
               None in
        Some(res)) in
   Gparser.veil descr (fun s i->tempf(s,i,i));;   
   
module Private=struct

let first_case_in_hwd 
  old_f (main_opener,main_closer,other_enclosers,s,i1,k,depth)=
    old_f (main_opener,main_closer,other_enclosers,s,i1,k,depth,None);;

let second_case_in_hwd 
  old_f (main_opener,main_closer,other_enclosers,s,i1,k,depth,s_opt)=
    old_f (main_opener,main_closer,other_enclosers,s,i1,k,depth,Some(s_opt)) ;;

let rec iterator_for_house_of_doors 
   (main_opener,main_closer,other_enclosers,s,i1,k,depth,opt)=
   if k>(String.length s)
   then None
   else
          match opt with
           None->first_case_in_hwd
                 iterator_for_house_of_doors 
                   (main_opener,main_closer,other_enclosers,s,i1,k,depth)
          |Some(rparen)->second_case_in_hwd
                 iterator_for_house_of_doors 
                   (main_opener,main_closer,other_enclosers,s,i1,k,depth,rparen)
    ;; 


end;;   
   
   
let house_with_doors
   (main_opener,main_closer)
     other_enclosers=
   let descr=Gparser_description.house_with_doors (main_opener,main_closer) other_enclosers in
   let rec tempf=(fun s i->
        Private.iterator_for_house_of_doors 
         (main_opener,main_closer,other_enclosers,s,i,i,1,None)
   ) in
   Gparser.veil descr tempf;;   
     
        
   
(*

Gparser.apply (enclosure  ("ab","cde")) "ab345cde901" 1;;
Gparser.apply (constant  "ab") "ab345cde901" 1;;
Gparser.apply (simple_star  "ab") "ab345cde901" 1;;
Gparser.apply (race  ("ab","b")) "123ab6ab90b234" 1;;

paolo.xella@iscima.cnr.it

*)   
   
