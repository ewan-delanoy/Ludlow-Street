(*

#use"after.ml";;

*)

let list_of_whites=[' ';'\n';'\r';'\t'];;

let after_whites s =
  let n=String.length s in
  let rec tempf=(
    fun j->
      if j>n then None else
      if  List.mem (String.get s (j-1)) list_of_whites
      then tempf(j+1)
      else Some(j)
  ) in
  tempf;;

  let after_whites_and_comments s=
    let n=String.length s in
    let rec tempf=(
      fun j->
        if j>n then None else
        if List.mem (String.get s (j-1)) list_of_whites
        then tempf(j+1)
        else 
        if Substring.is_a_substring_located_at "/*" s j
        then let k=Substring.leftmost_index_of_in_from "*/" s (j+2) in
             if k<0
             then None
             else tempf(k+2)
        else Some(j)
    ) in
    tempf;;
  
  (*    
  after_whites_and_comments "\n/* 567 */\t\r\n\n/* 89 ** // 78*/123";;    
  *)
  
exception Unfinished_simple_quoted_string of int;;  

let after_simple_quoted_string s k0=
    let n=String.length s in
    if (Strung.get s k0)<>'\''
    then k0
    else 
    let rec tempf=(fun k->
       if k>n
       then raise(Unfinished_simple_quoted_string(k0))
       else 
       let c=String.get s (k-1) in
       if c='\\'
       then tempf(k+2)
       else 
       if c='\''
       then k+1
       else tempf(k+1)
    ) in
    tempf (k0+1);;

exception Unfinished_double_quoted_string of int;;  
    
let after_double_quoted_string s k0=
        let n=String.length s in
        if (Strung.get s k0)<>'"'
        then k0
        else 
        let rec tempf=(fun k->
           if k>n
           then raise(Unfinished_double_quoted_string(k0))
           else 
           let c=String.get s (k-1) in
           if c='\\'
           then tempf(k+2)
           else 
           if c='"'
           then k+1
           else tempf(k+1)
        ) in
        tempf (k0+1);;     



exception Unbalanced_expression of char*char;;

let after_closing_character (lchar,rchar) s=
  let n=String.length s in
  let rec tempf=(
    fun (k,count)->
      if k>n
      then raise(Unbalanced_expression(lchar,rchar))
      else 
      if Substring.is_a_substring_located_at "/*" s k
      then let j=Substring.leftmost_index_of_in_from "*/" s (k+2) in
           tempf(j+2,count)
      else 
      if (Substring.is_a_substring_located_at "<<<EOF\n" s k)
         ||
         (Substring.is_a_substring_located_at "<<<'EOF'\n" s k) 
      then let j=Substring.leftmost_index_of_in_from "\nEOF;\n" s (k+7) in
           tempf(j+6,count)
      else 
      let c=String.get s (k-1) in
      if c=lchar
      then tempf(k+1,count+1)
      else 
      if c='\''
      then let j=after_simple_quoted_string s k in
           tempf(j,count)
      else
      if c='"'
      then let j=after_double_quoted_string s k in
           tempf(j,count)
      else     
      if c<>rchar
      then tempf(k+1,count)
      else 
        if count=1
        then k+1
        else tempf(k+1,count-1)
  ) in
  tempf;;

(*

after_closing_character ('{','}') "{ 345 }89" (1,0);;
after_closing_character ('{','}') "{2{4}6{8{0}2}4}67" (1,0);;
after_closing_character ('{','}') "{\"3}5\"}89" (1,0);;
after_closing_character ('{','}') "{'3}5'}89" (1,0);;
after_closing_character ('{','}') "{/*4}6*/}01" (1,0);;
after_closing_character ('{','}') "{<<<EOF\n}\nEOF;\n}78" (1,0);;
after_closing_character ('{','}') "{<<<'EOF'\n}\nEOF;\n}90" (1,0);;

*)




     




     