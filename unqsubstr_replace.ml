(*

Operation on substring finding, with indexes starting from 1.

#use"unqsubstr_replace.ml";;

*)


 
exception Beginning_of_string_appears_twice;;   
   
let left_helper_for_unique_occurrence s i j=
   let tester=(
     fun k->List.length(
       Substring.occurrences_of_in(String.sub s (k-1) (j-k+1)) s)=1
   ) in  
   if (not(tester 1))
   then raise(Beginning_of_string_appears_twice)
   else let rec tempf=(fun k->
           if tester k
           then k
           else tempf(k-1)
        ) in
        let k0=tempf(i) in
        Unqsubstr_helper.of_string(String.sub s (k0-1) (i-k0));;

exception Nonunique_substring of string;;

let small_in_string ((lc,a),ovw_b) s=
       let left_complement=Unqsubstr_helper.to_string lc in
       let b=Overwriter.to_string ovw_b in
       let unique_place=left_complement^a in
       let temp1=Substring.occurrences_of_in unique_place s in
       if List.length(temp1)<>1
       then raise(Nonunique_substring(unique_place))
       else 
       let i1=List.hd(temp1) in
       let i=i1+(String.length left_complement) in
       let j=i+(String.length a)-1 in
       (String.sub s 0 (i-1))^b^(String.sub s j ((String.length s)-j))

(*

let hlpr=Unqsubstr_helper.of_string "2";;
let ovw=Overwriter.of_string "bar";;


small_in_string ((hlpr,"foo"),ovw) "AB1fooC2foo3fooDEF";;
small_in_string ((hlpr,"\n"),ovw) "AB1fooC2\n3fooDEF";;



*)

 
 
 
   