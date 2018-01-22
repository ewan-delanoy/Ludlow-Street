(*

#use"Php_analizer/HRecognizer/atomic_hrecognize.ml";;

*)

let recgnz_constant text s i=
  if not(Substring.is_a_substring_located_at text s i)
  then None
  else Some(i+String.length(text));;

let recgnz_later_constant text s i=
    let j=Substring.leftmost_index_of_in_from text s i in
    if j<1 
    then None
    else Some(j+(String.length text));;    

let recgnz_constant_list l_text s i=
  Option.find_and_stop (
    fun text->recgnz_constant text s i
    ) l_text;;     

let recgnz_later_constant_list l_text s i=
    Option.find_and_stop (
    fun text->recgnz_later_constant text s i
    ) l_text;;    
  
let recgnz_exactly_one l_chr s i=
     if (i<1)||(i>String.length s)
     then None
     else
     if List.mem (String.get s (i-1)) l_chr
     then Some(i+1)
     else None;;  

let recgnz_star l_chr s=
  let n=String.length s in
  let rec tempf=(
    fun j->
      if j>n then None else
      if  List.mem (String.get s (j-1)) l_chr
      then tempf(j+1)
      else Some(j)
  ) in
  tempf;;

let recgnz_enclosed (opener,closer) s i=
  if (i<1)||(i>String.length s)
  then None
  else
  if (String.get s (i-1))<>opener
  then None
  else 
  Some(After.after_closing_character ('(',')') s (i+1,1));;
 
let recgnz x=
  match x with
   Atomic_hrecognizer.Constant(s)->recgnz_constant s
  |Atomic_hrecognizer.Later_constant(s)->recgnz_later_constant s
  |Atomic_hrecognizer.Constant_list(l)->recgnz_constant_list l
  |Atomic_hrecognizer.Later_constant_list(l)->recgnz_later_constant_list l
  |Atomic_hrecognizer.Exactly_one(l_chr)->recgnz_exactly_one l_chr
  |Atomic_hrecognizer.Star(l_chr)->recgnz_star l_chr
  |Atomic_hrecognizer.Enclosed(opener,closer)->recgnz_enclosed (opener,closer);;

let rec chain_in_detail (s,i_start,current_i,graet,da_ober)=
  match da_ober with
  []->(Some(current_i),None)
  |atom::peurrest->
    match recgnz atom s current_i with
    None->(None,Some(List.rev graet,atom,peurrest)) 
    |Some(j)->chain_in_detail (s,i_start,j,current_i::graet,peurrest);;

let chain l_atoms s i=
  chain_in_detail (s,i,i,[],l_atoms);;    

 