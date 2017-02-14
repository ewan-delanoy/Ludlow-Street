(*

#use"GParser/gparser_homomorphism.ml";;

*)



let chain l=
   let descr=Gparser_description.chain
      (Image.image Gparser.description l) in
   let rec tempf=(fun
   (imp_ranges,da_ober,s,i0,k)->
      match da_ober with
      []->Some(
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
            None->None
           |Some(res)->tempf(
                       imp_ranges@(Gparser_result.important_ranges res),
                       rest,s,i0,Gparser_result.final_cursor_position res)
         )  
   ) in
   Gparser.veil descr (fun s i->tempf ([],l,s,i,i));;

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

