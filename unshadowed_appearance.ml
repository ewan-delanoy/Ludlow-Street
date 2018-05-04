(*

#use"unshadowed_appearance.ml";;

*)

let test_for_perfect_match s k kwd=
   if not(Substring.is_a_substring_located_at kwd s k)
   then false
   else 
   let constraint1=(
       if k<2 then true else
       not(List.mem (Strung.get s (k-1)) 
       ('$'::Charset.php_label_nonfirst_letters ))
   )   in
   if not(constraint1)
   then false
   else
   let n=String.length(s) and m=k+(String.length kwd) in
   if m>n
   then true
   else not(List.mem (Strung.get s m) 
        Charset.php_label_nonfirst_letters );;

let next s l_kwds=
  let n=String.length s in
  let rec tempf=(fun k->
   if k>n then None else
   if List.exists (test_for_perfect_match s k) l_kwds
   then Some(k)         
   else 
   if (Substring.is_a_substring_located_at "<<<EOF\n" s k)
      ||
      (Substring.is_a_substring_located_at "<<<'EOF'\n" s k) 
   then let m=Substring.leftmost_index_of_in_from "\nEOF;\n" s (k+7) in
         tempf(m+6)
   else 
   if Substring.is_a_substring_located_at "/*" s k
   then let m=Substring.leftmost_index_of_in_from "*/" s (k+2) in
        tempf (m+2)
   else
   if Substring.is_a_substring_located_at "//" s k
   then let m=Substring.leftmost_index_of_in_from "\n" s (k+2) in
        tempf (m+1)
   else
   let c=Strung.get s k in
   if c='\''
   then let m=After.after_simple_quoted_string s k in tempf m
   else
   if c='"'
   then let m=After.after_double_quoted_string s k in tempf m
   else tempf (k+1)
  ) in
  tempf;;

let all s l_kwds=
    let n=String.length s in
    let rec tempf=(fun (graet,k)->
     if k>n then List.rev graet else
     match Option.seek (test_for_perfect_match s k) l_kwds with
     Some(kwd)->tempf(k::graet,k+String.length kwd)
     |None->
     if (Substring.is_a_substring_located_at "<<<EOF\n" s k)
     ||
     (Substring.is_a_substring_located_at "<<<'EOF'\n" s k) 
     then let m=Substring.leftmost_index_of_in_from "\nEOF;\n" s (k+7) in
          tempf(graet,m+6)
     else 
     if Substring.is_a_substring_located_at "/*" s k
     then let m=Substring.leftmost_index_of_in_from "*/" s (k+2) in
          tempf (graet,m+2)
     else
     if Substring.is_a_substring_located_at "//" s k
     then let m=Substring.leftmost_index_of_in_from "\n" s (k+2) in
          tempf (graet,m+1)
     else
     let c=Strung.get s k in
     if c='\''
     then let m=After.after_simple_quoted_string s k in tempf (graet, m)
     else
     if c='"'
     then let m=After.after_double_quoted_string s k in tempf (graet,m)
     else tempf (graet,k+1)
    ) in
    tempf;;
  

(*

nua "/* include */ \"require_once\" 'include_once' require 3" 
["include_once";"require_once";"include";"require"]
1;;

*)

