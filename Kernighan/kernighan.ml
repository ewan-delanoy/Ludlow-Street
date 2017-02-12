(*

#use"Kernighan/kernighan.ml";;

*)

(*
type parser_result={
   description : string;
   parameters : string list;
   whole_range : int*int ;
   important_ranges : (int*int) list;
   final_cursor_position : int; 
};;


type parser=Prsr of 
(string->int->(parser_result option));;

let apply_parser (Prsr f) s i=f s i;;

let chain descr l=
   let rec tempf=(fun
   (params,imp_ranges,da_ober,s,i0,k)->
      match da_ober with
      []->Some(
            {
   				description =descr;
   				parameters =params;
   				whole_range =(i0,k-1) ;
        		important_ranges=imp_ranges;
        		final_cursor_position =k; 
             }
           )
      |prsr::rest->   
         (
           match apply_parser prsr s k with
            None->None
           |Some(res)->tempf(params@res.parameters,
                       imp_ranges@res.important_ranges,
                       rest,s,i0,res.final_cursor_position)
         )  
   ) in
   Prsr(fun s i->tempf ([],[],l,s,i,i));;


let disjunction descr l=
   let rec tempf=(fun
   (da_ober,s,k)->
      match da_ober with
      []->None
      |prsr::rest->   
         (
           match apply_parser prsr s k with
            None->None
           |Some(res)->tempf(params@res.parameters,
                       imp_ranges@res.important_ranges,
                       rest,s,i0,res.final_cursor_position)
         )  
         Some(
            {
   				description =descr;
   				parameters =params;
   				whole_range =(i0,k-1) ;
        		important_ranges=imp_ranges;
        		final_cursor_position =k; 
             }
           )
   ) in
   Prsr(fun s i->tempf ([],[],l,s,i,i));;

let parse_enclosure (left_encloser,right_encloser)=
   Prsr(fun s i1->
   if (not(Substring.is_a_substring_located_at left_encloser s i1))
   then None
   else 
   let i2=i1+(String.length left_encloser) in
   let i3=Substring.leftmost_index_of_in_from right_encloser s i2 in
   if i3<1
   then None 
   else
   let i4=i3+(String.length right_encloser)-1 in
   let res={
   	description ="enclosure";
    parameters =[left_encloser;right_encloser];
    whole_range=(i1,i4);
    important_ranges=[i2,i3-1];
    final_cursor_position=i4+1; 
   } in
   Some(res));;
   
*)   
   
(*

apply_parser (parse_enclosure  ("ab","cde")) "ab345cde901" 1;;

*)   
   
