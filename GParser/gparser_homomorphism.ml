(*

#use"GParser/gparser_homomorphism.ml";;

*)

module Private=struct

type chain_artefact=
     Usual of (int * int) list * Gparser.t list * bytes * Gparser_description.t * int * int 
    |Result_found of Gparser_result.t
    |Failure_found;;

let pusher_for_chain=function
  Usual(imp_ranges,da_ober,s,descr,i0,k)->
      (
      match da_ober with
      []->Result_found(
           Gparser_result.veil
               descr
               (i0,k-1)
               imp_ranges
               k
               None
          )
      |prsr::rest->   
         (
           match Gparser.apply prsr s k with
            None->Failure_found
           |Some(res)->Usual(imp_ranges@(Gparser_result.important_ranges res),
                       rest,s,descr,i0,Gparser_result.final_cursor_position res)
         )  
        )
    |x->x;;
    
let rec iterator_for_chain=function
   Result_found(res)->Some(res)
  |Failure_found->None
  |x->iterator_for_chain(pusher_for_chain x);;
    

end;;

let chain l=
   let descr=Gparser_description.chain
      (Image.image Gparser.description l) in
   Gparser.veil descr (fun s i->Private.iterator_for_chain(Private.Usual([],l,s,descr,i,i)));;

let disjunction l=
   let descr=Gparser_description.disjunction
      (Image.image Gparser.description l) in
   let indexed_l=Ennig.index_everything l in   
   let rec tempf=(fun
   (da_ober,s,i0)->
      match da_ober with
      []->None 
      |(j,prsr)::rest->
         (
           match Gparser.apply prsr s i0 with
             None->tempf(rest,s,i0)
           |Some(res)->
          Some(
             Gparser_result.veil
               descr
               (Gparser_result.whole_range res)
               (Gparser_result.important_ranges res)
               (Gparser_result.final_cursor_position res)
               (Some j)
           )
         )   
   ) in
   Gparser.veil descr (fun s i->tempf (indexed_l,s,i));;

let star prsr=
   let descr=Gparser_description.star(Gparser.description prsr) in
   let rec tempf=(fun
   (imp_ranges,s,i0,k)->
      match Gparser.apply prsr s k with
       None->Some(
             Gparser_result.veil
               descr
               (i0,k-1)
               (imp_ranges)
               k
               None
            )
      |Some(res)->tempf(imp_ranges@(Gparser_result.important_ranges res),
                       s,i0,Gparser_result.final_cursor_position res)
   
   ) in
   Gparser.veil descr (fun s i->tempf ([],s,i,i));;
   
   
let one_or_more prsr=chain [prsr;star prsr];;

let optional prsr=
   let descr=Gparser_description.optional(Gparser.description prsr) in
   let rec tempf=(fun s i->
      match Gparser.apply prsr s i with
       Some(res)->Some(
            Gparser_result.veil
               descr
               (Gparser_result.whole_range res)
               (Gparser_result.important_ranges res)
               (Gparser_result.final_cursor_position res)
               None
            )
      |None->Some(
            Gparser_result.veil
               descr
               (i,i-1)
               []
               i
               None
            )
   
   ) in
   Gparser.veil descr tempf;;


let recoiling_ending x y=
   let descr=Gparser_description.recoiling_ending
      (Gparser.description x) (Gparser.description y)  in
   let tempf=(fun s i->
      match Gparser.apply x s i with
       None->None
      |Some(res)->
                  
                  let j=Gparser_result.final_cursor_position res in
                  if Gparser.apply y s j=None then None else
                  Some(
                  Gparser_result.veil
                  descr
                  (i,j-1)
                  (Gparser_result.important_ranges res)
                  j
                  None
                  )
   ) in
   Gparser.veil descr tempf;;
