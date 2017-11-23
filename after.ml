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
      if Substring.is_a_substring_located_at "//" s k
      then let j=Substring.leftmost_index_of_in_from "\n" s (k+2) in
           tempf(j+1,count)
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

let classlike_declaration_chars=list_of_whites@Characters_in_namespace_name.chars;;

let after_classlike_declaration s i=
    Option.seek(
     fun j->not(List.mem 
         (String.get s (j-1)) classlike_declaration_chars
     )
    )(Ennig.ennig i (String.length s));;


let after_abstract_class s i0=
  if not(Substring.is_a_substring_located_at "abstract" s i0)
  then None
  else
  let opt1=after_whites s (i0+8) in
  if opt1=None then None else
  let i1=Option.unpack opt1 in
  if not(Substring.is_a_substring_located_at "class" s i1)
  then None
  else 
  let opt2=after_classlike_declaration s (i1+5) in
  if opt2=None then None else
  let i2=Option.unpack opt2 in
  if (Strung.get s i2)<>'{' then None else 
  Some(after_closing_character ('{','}') s (i2+1,1));;

(*

after_abstract_class "abstract  class {u\nv}234" 1;;

*)

let after_final_class s i0=
  if not(Substring.is_a_substring_located_at "final" s i0)
  then None
  else
  let opt1=after_whites s (i0+5) in
  if opt1=None then None else
  let i1=Option.unpack opt1 in
  if not(Substring.is_a_substring_located_at "class" s i1)
  then None
  else 
  let opt2=after_classlike_declaration s (i1+5) in
  if opt2=None then None else
  let i2=Option.unpack opt2 in
  if (Strung.get s i2)<>'{' then None else 
  Some(after_closing_character ('{','}') s (i2+1,1));;     

(*

after_final_class "final  class {u\nv}901" 1;;

*)

let after_usual_class s i0=
  if not(Substring.is_a_substring_located_at "class" s i0)
  then None
  else 
  let opt2=after_classlike_declaration s (i0+5) in
  if opt2=None then None else
  let i2=Option.unpack opt2 in
  if (Strung.get s i2)<>'{' then None else 
  Some(after_closing_character ('{','}') s (i2+1,1));;     

(*

after_usual_class "class {u\nv}234" 1;;
after_usual_class "class_loader { }" 1;;

*)

let after_interface s i0=
  if not(Substring.is_a_substring_located_at "interface" s i0)
  then None
  else 
  let opt2=after_classlike_declaration s (i0+5) in
  if opt2=None then None else
  let i2=Option.unpack opt2 in
  if (Strung.get s i2)<>'{' then None else 
  Some(after_closing_character ('{','}') s (i2+1,1));;  

(*

after_interface "interface {u\nv}678" 1;;

*)

let after_classlike_block s i=
   Option.find_and_stop(
     fun f->f s i
   )[
       after_abstract_class;
       after_final_class;
       after_usual_class;
       after_interface;
    ];;


(*

after_classlike_block "abstract  class {u\nv}234" 1;;
after_classlike_block "final  class {u\nv}901" 1;;
after_classlike_block "class {u\nv}234" 1;;
after_classlike_block "interface {u\nv}678" 1;;

*)    

let after_classlike_block_with_linebreak s i=
  let n=String.length s in
  let opt1=after_classlike_block s i in
  if opt1=None then None else
  let i1=Option.unpack opt1 in
  let opt2=Option.seek(fun j->
     not(List.mem (Strung.get s j) [' ';'\r';'\t']) )
  (Ennig.ennig i1 n) in
  if opt2=None then None else
  let i2=Option.unpack opt2 in
  if Strung.get s i2='\n'
  then Some(i2+1)
  else None;;
    
(*

after_classlike_block_with_linebreak "abstract  class {u\nv}  \t \n7" 1;;
after_classlike_block_with_linebreak "final  class {u\nv} \t\t\n3" 1;;
after_classlike_block_with_linebreak "class {u\nv}\n3" 1;;
after_classlike_block_with_linebreak "interface {u\nv} \t\n9" 1;;

*)    

exception End_of_div_not_found;;

let rec main_helper_for_div (s,n,div_count,idx)=
    if idx>n
    then raise(End_of_div_not_found)
    else
    if Substring.is_a_substring_located_at "</div>" s idx
    then if div_count=1
         then idx+6
         else main_helper_for_div(s,n,div_count-1,idx+6)
    else 
    if not(Substring.is_a_substring_located_at "<div " s idx)
    then main_helper_for_div(s,n,div_count,idx+1)
    else  
    let jdx=Substring.leftmost_index_of_in_from ">" s (idx+5) in
    main_helper_for_div(s,n,div_count+1,jdx);;

let after_div s idx=main_helper_for_div(s,String.length s,0,idx);;

(*

after_div "<div val=\"abc\"> xyz </div>789" 1;;

*)

(*
let after_one_among_several


let after_fnctn s idx=
   let opt1=after_whites s idx in
   if opt1=None then None else
*)