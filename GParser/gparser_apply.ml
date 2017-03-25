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
   


(*

In the first case, no encloser is opened except for the main one.
In the second, the "rparen" opener is waited for before
making any other change.

*)

let first_case_in_hwd 
  old_f (main_opener,main_closer,other_enclosers,s,i1,k,depth)=
   let opt1=Option.find_it(fun (opener,closer)->
     Substring.is_a_substring_located_at opener s k
   ) other_enclosers in
   if opt1<>None
   then let (op1,cl1)=Option.unpack opt1 in
        old_f (main_opener,main_closer,other_enclosers,s,i1,k+(String.length op1),depth,Some(cl1))
   else 
   if Substring.is_a_substring_located_at main_opener s k
   then old_f (main_opener,main_closer,other_enclosers,s,i1,k+(String.length main_opener),depth+1,None)
   else 
   if not(Substring.is_a_substring_located_at main_closer s k)
   then old_f (main_opener,main_closer,other_enclosers,s,i1,k+1,depth,None)
   else 
   if depth>1
   then old_f (main_opener,main_closer,other_enclosers,s,i1,k+(String.length main_closer),depth-1,None)
   else 
   let j1=k+(String.length main_closer) in
   let res=Gparser_result.veil
               (i1,j1-1)
               []
               j1
               None in
   Some(res);;
    
    
    
let second_case_in_hwd 
  old_f (main_opener,main_closer,other_enclosers,s,i1,k,depth,rparen)=
  if Substring.is_a_substring_located_at rparen s k
  then old_f(main_opener,main_closer,other_enclosers,s,i1,k+(String.length rparen),depth,None)
  else old_f(main_opener,main_closer,other_enclosers,s,i1,k+1,depth,Some(rparen)) ;;

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


 
   
   
let house_with_doors
   (main_opener,main_closer)
     other_enclosers=
   let rec tempf=(fun s i->
        if not(Substring.is_a_substring_located_at main_opener s i)
        then None 
        else 
          let j=i+(String.length main_opener) in
          iterator_for_house_of_doors 
         (main_opener,main_closer,other_enclosers,s,i,j,1,None)
   ) in
   (tempf:Gparser_fun.t);;   

type chain_artefact=
     Usual of (int * int) list * Gparser_fun.t list * bytes * int * int 
    |Result_found of Gparser_result.t
    |Failure_found;;

let pusher_for_chain=function
  Usual(imp_ranges,da_ober,s,i0,k)->
      (
      match da_ober with
      []->Result_found(
           Gparser_result.veil
               (i0,k-1)
               imp_ranges
               k
               None
          )
      |prsr::rest->   
         (
           match prsr s k with
            None->Failure_found
           |Some(res)->Usual(imp_ranges@(Gparser_result.important_ranges res),
                       rest,s,i0,Gparser_result.final_cursor_position res)
         )  
        )
    |x->x;;
    
let rec iterator_for_chain=function
   Result_found(res)->Some(res)
  |Failure_found->None
  |x->iterator_for_chain(pusher_for_chain x);;
    
let starter_for_chain l s i=Usual([],l,s,i,i);;     


let chain l=
    ((fun s i->
     iterator_for_chain(starter_for_chain  l s i)):Gparser_fun.t);;
     
let pusher_for_detailed_chain=function
  Usual(imp_ranges,da_ober,s,i0,k)->
      (
      match da_ober with
      []->Result_found(
           Gparser_result.veil
               (i0,k-1)
               (List.rev imp_ranges)
               k
               None
          )
      |prsr::rest->   
         (
           match prsr s k with
            None->Failure_found
           |Some(res)->Usual((Gparser_result.whole_range res)::imp_ranges,
                       rest,s,i0,Gparser_result.final_cursor_position res)
         )  
        )
    |x->x;;
    
let rec iterator_for_detailed_chain=function
   Result_found(res)->Some(res)
  |Failure_found->None
  |x->iterator_for_detailed_chain(pusher_for_detailed_chain x);;
    

let detailed_chain l=
    ((fun s i->
     iterator_for_detailed_chain(starter_for_chain  l s i)):Gparser_fun.t);;     

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
    |Gparser.House_with_doors(s1,s2,l)->house_with_doors (s1,s2) l
    
    |Gparser.Chain(l)->chain(Image.image apply l)
    |Gparser.Disjunction(l)->disjunction(Image.image apply l)
    |Gparser.Star(x)->star(apply x)
    |Gparser.One_or_more(x)->one_or_more(apply x)
    |Gparser.Optional(x)->optional(apply x)
    |Gparser.Recoiling_ending(x,y)->recoiling_ending (apply x) (apply y)
    |Gparser.Detailed_chain(l)->detailed_chain(Image.image apply l);;
   
end;;   
   
let apply=Private.apply;;   
   
(*



*)   
   