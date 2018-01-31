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

let recgnz_star l_chr s i=
      let n=String.length s in
      let rec tempf=(
        fun j->
          if j>n then None else
          if  List.mem (String.get s (j-1)) l_chr
          then tempf(j+1)
          else Some(j)
      ) in
tempf i;;

let recgnz_nonempty_star l_chr s i=
  let n=String.length s in
  if n=0 then None else
  if (not(List.mem (String.get s (i-1)) l_chr)) then None else 
  let rec tempf=(
    fun j->
      if j>n then None else
      if  List.mem (String.get s (j-1)) l_chr
      then tempf(j+1)
      else Some(j)
  ) in
  tempf i;;

let recgnz_nonempty_star_outside l_chr s i=
    let n=String.length s in
    if n=0 then None else
    if (not(List.mem (String.get s (i-1)) l_chr)) then None else
    let rec tempf=(
      fun j->
        if j>n then None else
        if  (not(List.mem (String.get s (j-1)) l_chr))
        then tempf(j+1)
        else Some(j)
    ) in
    tempf i;;

let recgnz_enclosed (opener,closer) s i=
  if (i<1)||(i>String.length s)
  then None
  else
  if (String.get s (i-1))<>opener
  then None
  else 
  Some(After.after_closing_character (opener,closer) s (i+1,1));;
 
let recgnz x=
  match x with
   Atomic_hrecognizer.Constant(s)->recgnz_constant s
  |Atomic_hrecognizer.Later_constant(s)->recgnz_later_constant s
  |Atomic_hrecognizer.Constant_list(l)->recgnz_constant_list l
  |Atomic_hrecognizer.Later_constant_list(l)->recgnz_later_constant_list l
  |Atomic_hrecognizer.Exactly_one(l_chr)->recgnz_exactly_one l_chr
  |Atomic_hrecognizer.Star(l_chr)->recgnz_star l_chr
  |Atomic_hrecognizer.Nonempty_Star(l_chr)->recgnz_nonempty_star l_chr
  |Atomic_hrecognizer.Nonempty_Star_outside(l_chr)->recgnz_nonempty_star_outside l_chr
  |Atomic_hrecognizer.Enclosed(opener,closer)->recgnz_enclosed (opener,closer);;


  