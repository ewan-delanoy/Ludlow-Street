(*

#use"Kernighan/gparser_constructor.ml";;

*)



let parse_enclosure (left_encloser,right_encloser)=
   let descr=Gparser_description.veil "enclosure" [left_encloser;right_encloser] in
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
               (i4+1) in
   Some(res)) in
   Gparser.veil descr tempf;;
   

   
(*

Gparser.apply (parse_enclosure  ("ab","cde")) "ab345cde901" 1;;

*)   
   
