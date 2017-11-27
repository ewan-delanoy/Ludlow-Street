(*

Operation on substring finding, with indexes starting from 1.

#use"unqsubstr_replace.ml";;

*)


 
exception Beginning_of_string_appears_twice of int*int*int;;   
   
let left_helper s i j=
   let temp=Substring.occurrences_of_in (String.sub s (i-1) (j-i+1)) s in
   if List.length(temp)=1
   then Unqsubstr_helper.of_string ""
   else 
   let bad_ones=List.filter (fun t->t<>i) temp in
   let bounds=Ennig.ennig 1 (i-1) in
   let measure=(
       fun t->match Option.seek(
         fun d->(String.get s (i-d-1))<>(String.get s (t-d-1))
       ) bounds with
       None->raise(Beginning_of_string_appears_twice(i,j,t))
       |Some(d0)->d0
   ) in
   let (_,m)=Max.maximize_it measure bad_ones in
   Unqsubstr_helper.of_string(String.sub s (i-m-1) m);;

exception Nonunique_substring of string;;

let compute_unique_index (lc,a) s=
  let left_complement=Unqsubstr_helper.to_string lc in
  let unique_place=left_complement^a in
  let temp1=Substring.occurrences_of_in unique_place s in
  if List.length(temp1)<>1
  then raise(Nonunique_substring(unique_place))
  else 
  List.hd(temp1);;

let small_in_string ((lc,a),ovw_b) s=
       let b=Overwriter.to_string ovw_b 
       and i1=compute_unique_index (lc,a) s in
       let left_complement=Unqsubstr_helper.to_string lc in
       let i=i1+(String.length left_complement) in
       let j=i+(String.length a)-1 in
       (String.sub s 0 (i-1))^b^(String.sub s j ((String.length s)-j))

let small_in_file ((lc,a),ovw_b) ap=
     let old_text=Io.read_whole_file ap in
     let new_text=small_in_string ((lc,a),ovw_b) old_text in
     Io.overwrite_with ap new_text;;

let large_in_string (lc1,a1) (lc2,a2) ovw_b s=
      let b=Overwriter.to_string ovw_b 
      and i1=compute_unique_index (lc1,a1) s 
      and i2=compute_unique_index (lc2,a2) s in
      let left_complement1=Unqsubstr_helper.to_string lc1 
      and left_complement2=Unqsubstr_helper.to_string lc2  in
      let i=i1+(String.length left_complement1) in
      let j=i2+(String.length left_complement2)+(String.length a2)-1 in
      (String.sub s 0 (i-1))^b^(String.sub s j ((String.length s)-j));;

let large_in_file (lc1,a1) (lc2,a2) ovw_b ap=
    let old_text=Io.read_whole_file ap in
    let new_text=large_in_string (lc1,a1) (lc2,a2) ovw_b old_text in
    Io.overwrite_with ap new_text;;


(*

let hlpr=Unqsubstr_helper.of_string "2";;
let ovw=Overwriter.of_string "foo";;


small_in_string ((hlpr,"34"),ovw) "1234567890";;
large_in_string (hlpr,"34") (hlpr,"cd") ovw  "1234567890a2cdEFGH";;



*)

 
 
 
   