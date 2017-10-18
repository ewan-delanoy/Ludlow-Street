(*

#use"Php_analizer/Great_Replacement/namespacize.ml";;

*)

module Private=struct

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

exception Unbalanced_expression of char*char;;

let after_closing_character (lchar,rchar) s=
  let n=String.length s in
  let rec tempf=(
    fun (k,count)->
      if k>n
      then raise(Unbalanced_expression(lchar,rchar))
      else 
      let c=String.get s (k-1) in
      if c=lchar
      then tempf(k+1,count+1)
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

*)


let after_first_whites_and_comments s=
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
after_first_whites_and_comments "\n/* 567 */\t\r\n\n/* 89 ** // 78*/123";;    
*)

let nspc_list=
  (Ennig.doyle char_of_int 65 90)@
  (Ennig.doyle char_of_int 97 122)@
  (Ennig.doyle char_of_int 48 57)@
  ['\\'];;

let after_nspc_name s i=
    let n=String.length s in
    Option.seek(fun j->
    not(List.mem (Strung.get s j) nspc_list)
    )(Ennig.ennig i n);;  

(*

The namespace_computation function below returns 
an uple (nspc_name,nspc_idx,left_idx,right_idx,is_standard)
such that :
1) The interval between left_idx and right_idx is a right place
to insert in a new namespace keyword
2) nspc_idx is the location of the namespace keyword if it is
present, 0 otherwise
3) Whenever there already is a namespace keyword, left_idx coincides
with npsc_idx.


*)  

let namespace_computation s k=
  let opt1=after_whites s k in
  let i1=Option.unpack opt1 in
  if not(Substring.is_a_substring_located_at "namespace" s i1)
  then ("",0,i1-1,i1,false)
  else 
  let opt2=after_whites s (i1+9) in
  let i2=Option.unpack opt2 in
  let opt3=after_nspc_name s i2 in
  let i3=Option.unpack opt3 in
  let opt4=after_whites s i3 in
  let i4=Option.unpack opt4 in
  (Cull_string.interval s i2 (i3-1),i1,i1,i4,(String.get s (i4-1))='{') 
  ;;

(*

namespace_computation "  not_a_namespace" 1;;
namespace_computation "  namespace 345\\789\\123\\56    ; 345" 1;;
namespace_computation "  namespace 345\\789\\123\\56    { 345" 1;;
namespace_computation "  namespace                      { 678" 1;;
namespace_computation "  namespace { 567" 1;;

*)

  
exception Absent_php_open_tag;;
exception Incomplete_declaration;;  
exception Badly_ended_declaration;; 

let decompose s=
  if not(Substring.begins_with s "<?php") 
  then raise(Absent_php_open_tag)
  else
  let opt1=after_first_whites_and_comments s 6 in
  if opt1=None then ("",0,1,2,false) else
  let i1=Option.unpack opt1 in
  let first_try=namespace_computation s i1 in 
  let (nspc_name,nspc_idx,left_idx,right_idx,is_standard)=first_try in
  if nspc_idx<>0
  then first_try
  else 
  if not(Substring.is_a_substring_located_at "declare" s i1)
  then ("",0,i1-1,i1,false)
  else 
  let opt2=after_whites s (i1+7) in
  if opt2=None then raise(Incomplete_declaration) else
  let i2=Option.unpack opt2 in
  let i3=after_closing_character ('(',')') s (i2,0) in
  let opt4=after_whites s (i3+1) in 
  let i4=Option.unpack opt4 in
  let second_try=namespace_computation s i4 in 
  let (* (nspc_name2,nspc_idx2,left_idx2,right_idx2,is_standard2) *)
  (_,nspc_idx2,_,_,_)=second_try in
  if nspc_idx2<>0
  then second_try
  else ("",0,i4-1,i4,false);;

(*

decompose "<?php  declare(678); namespace 2345; 890";;
decompose "<?php  declare(678); namespace 2345{ 890 }";;

decompose "<?php  declare(678){ namespace 2345; 890   }";;
decompose "<?php  declare(678){ namespace 2345{ 890 } }";;

decompose "<?php  declare(678);  x ";;
decompose "<?php  8=0=2+4;";;

*)

let standardize s=
    let  (nspc_name,nspc_idx,left_idx,right_idx,is_standard)=decompose s in
    if is_standard   then s else 
    let n=String.length s in
    (Cull_string.interval s 1 left_idx)^
    "\nnamespace "^nspc_name^
    " {\n"^
    (Cull_string.interval s right_idx  n)^
    "\n}\n\n\n";;

(*

standardize "<?php  declare(678); namespace 2345; 890";;
standardize "<?php  declare(678); namespace 2345{ 890 }";;

standardize "<?php  declare(678){ namespace 2345; 890   }";;
standardize "<?php  declare(678){ namespace 2345{ 890 } }";;
standardize "<?php  8=0=2+4;";;

*)

let name_and_end s j=
  (* the s argument is assumed to be already standardized *) 
  let j1=Substring.leftmost_index_of_in_from "namespace" s j in
  if j1<1 then ("",(String.length s)+1) else
  let (nspc_name,nspc_idx,_,right_idx,_)=namespace_computation s j1 in
  if nspc_idx=0
  then ("",(String.length s)+1)
  else (nspc_name,after_closing_character ('{','}') s (right_idx,0) );;

(*

name_and_end "<?php   namespace{90}23namespace 45 {8901}namespace{34}67" 7;;
name_and_end "<?php   namespace{90}23namespace 45 {8901}namespace{34}67" 22;;
name_and_end "<?php   namespace{90}23namespace 45 {8901}namespace{34}67" 43;;

*)


let namespace_at_index s j=
    (* the s argument is assumed to be already standardized *) 
    let rec tempf=(
      fun t->
        (* By assumption, t <=j *)
        let (nspc_name,new_t)=name_and_end s t in
        if new_t>j
        then nspc_name
        else tempf(new_t)
    ) in
    tempf 1;;

(*

namespace_at_index 
 "<?php   namespace{90}23namespace 45 {8901}namespace{34}67" 19;;
namespace_at_index 
 "<?php   namespace{90}23namespace 45 {8901}namespace{34}67" 38;;

*)

let cull_php_enclosers old_s=
    let s=Cull_string.trim_spaces old_s in
    let n=String.length s in
    let i1=(if Substring.begins_with s "<?php" then 6 else 1)
    and j1=(if Substring.ends_with s "?>" then (n-2) else n) in
    Cull_string.interval s i1 j1;;

let insert_at_interval inserted_text (i,j) container_text=
    (* container_text is assumed to be already standardized *)
    let n=String.length container_text in
    let nspc_name=namespace_at_index container_text i in
    (Cull_string.interval container_text 1 (i-1))^
    "\n}\n\n\n"^
    (cull_php_enclosers(standardize inserted_text))^
    "\nnamespace "^nspc_name^
    " {\n"^
    (Cull_string.interval container_text (j+1) n);;

exception Nonunique_place;;

let insert_at_unique_place_in_string 
     inserted_text
     (left_complement,place)
     container_text=
       let unique_place=left_complement^place in
       let temp1=Substring.occurrences_of_in unique_place container_text in
       if List.length(temp1)<>1
       then raise(Nonunique_place)
       else 
       let i1=List.hd(temp1) in
       let i=i1+(String.length left_complement) in
       let j=i+(String.length place)-1 in
       insert_at_interval inserted_text (i,j) container_text;;

end;;

(*

insert_at_unique_place_in_string 
 "<?php namespace uvw {x+y+z} "
 ("8","90")
 "<?php   namespace{90}23namespace 45 {8901}namespace{34}67";;

*)

let standardize ap=
  let new_content=Private.standardize(Io.read_whole_file ap) in
Io.erase_file_and_fill_it_with_string 
ap new_content;;

let expand_inclusion
inserted_file
(left_complement,place)
container_file=
   let new_content=Private.insert_at_unique_place_in_string 
                     (Io.read_whole_file inserted_file)
                      (left_complement,place)
                     (Io.read_whole_file container_file) in
   Io.erase_file_and_fill_it_with_string 
      container_file new_content;;

let rexpand_inclusion
      inserted_file
      (left_complement,place)
      container_file
      l_rep=
      let pre_content=
         Replace_inside.replace_several_inside_string l_rep
      (Io.read_whole_file container_file) in
         let new_content=Private.insert_at_unique_place_in_string 
                           (Io.read_whole_file inserted_file)
                            (left_complement,place)
                           pre_content in
         Io.erase_file_and_fill_it_with_string 
            container_file new_content;;









     

     




     