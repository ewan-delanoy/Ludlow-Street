(*

#use"Php_analizer/Great_Replacement/first_pass_parse.ml";;

*)

let qualified_class qualification nspc_name s idx=
  let opt1=After.after_whites s idx in
  if opt1=None then None else 
  let i1=Option.unpack opt1 in
  if not(Substring.is_a_substring_located_at qualification s i1)
  then None
  else
  let opt2=After.after_whites s (i1+(String.length qualification)) in
  if opt2=None then None else 
  let i2=Option.unpack opt2 in
  if not(Substring.is_a_substring_located_at "class" s i2)
  then None
  else 
  let opt3=After.after_whites s (i2+5) in
  if opt3=None then None else 
  let i3=Option.unpack opt3 in
  let opt4=After.after_php_label s i3 in 
  if opt4=None then None else 
  let i4=Option.unpack opt4 in
  let i5=Substring.leftmost_index_of_in_from "{" s i4 in
  if i5<1 then None else
  let i6=After.after_closing_character ('{','}')  s (i5+1,1) in
  let answer=Classlike_item.make 
    (Classlike_kind.from_class_qualification qualification)
    nspc_name
    (Cull_string.interval s i3 (i4-1))
    (Cull_string.trim_spaces(Cull_string.interval s i4 (i5-1)))
    (Cull_string.interval s idx i5)
    (Cull_string.interval s (i5+1) (i6-2))
    (Cull_string.interval s (i6-1) (i6-1))
  in
  Some answer;;

let intrface nspc_name s idx=
    let opt1=After.after_whites s idx in
    if opt1=None then None else 
    let i1=Option.unpack opt1 in
    if not(Substring.is_a_substring_located_at "interface" s i1)
    then None
    else 
    let opt3=After.after_whites s (i1+9) in
    if opt3=None then None else 
    let i3=Option.unpack opt3 in
    let opt4=After.after_php_label s i3 in 
    if opt4=None then None else 
    let i4=Option.unpack opt4 in
    let i5=Substring.leftmost_index_of_in_from "{" s i4 in
    if i5<1 then None else
    let i6=After.after_closing_character ('{','}')  s (i5+1,1) in
    let answer=Classlike_item.make 
      Classlike_kind.interface
      nspc_name
      (Cull_string.interval s i3 (i4-1))
      (Cull_string.trim_spaces(Cull_string.interval s i4 (i5-1)))
      (Cull_string.interval s idx i5)
      (Cull_string.interval s (i5+1) (i6-2))
      (Cull_string.interval s (i6-1) (i6-1))
    in
    Some answer;;
  
let classlike_item nspc_name s idx=
    Option.find_and_stop (fun f->f nspc_name s idx)
    [
      intrface;
      qualified_class "abstract";
      qualified_class "final";
      qualified_class "";
    ];;


(*

qualified_class "" "NPK" " class Lane extends John {789}01" 1;;
qualified_class "abstract" "NPK" "  abstract class Lane extends John {789}01" 1;;
intrface "NPK" "  interface Lane extends John {789}01" 1;;

*)

let fnctn s idx=
      let opt1=After.after_whites s idx in
      if opt1=None then None else 
      let i1=Option.unpack opt1 in
      let opt2=After.after_one_among_several ["private";"protected";"public"] s i1 in
      let i2=(match opt2 with Some(i)->i |None->i1) in
      let opt3=After.after_whites s i2 in
      if opt3=None then None else 
      let i3=Option.unpack opt3 in
      if not(Substring.is_a_substring_located_at "function" s i3)
      then None
      else
      let opt4=After.after_whites s (i3+8) in
      if opt4=None then None else 
      let i4=Option.unpack opt4 in
      let i5=(
         if Strung.get s i4='&'
         then i4+1
         else i4
      ) in
      let opt6=After.after_php_label s i5 in
      if opt6=None then None else 
      let i6=Option.unpack opt6 in   
      let opt7=After.after_whites s i6 in
      if opt7=None then None else 
      let i7=Option.unpack opt7 in
      if not(Substring.is_a_substring_located_at "(" s i7)
      then None
      else
      let i8=After.after_closing_character ('(',')')  s (i7+1,1) in
      let opt9=After.after_whites s i8 in
      if opt9=None then None else 
      let i9=Option.unpack opt9 in
      if not(Substring.is_a_substring_located_at "{" s i9)
      then None
      else
      let i10=After.after_closing_character ('{','}')  s (i9+1,1) in
      Some(i10,(i1,i2,i3,i4,i5,i6,i7,i8,i9));;
      
  (*
  
  fnctn "private function amy($u,$v,$w=83) \n {for($x=7;x++) {a=b;} dann();} unt; " 1;; 
  fnctn "private function &amy($u,$v,$w=83) \n {for($x=7;x++) {a=b;} dann();} unt; " 1;; 
  
  *)
                
  let abstract_fnctn s idx=
    let opt0=After.after_whites s idx in
    if opt0=None then None else 
    let i0=Option.unpack opt0 in
    if not(Substring.is_a_substring_located_at "abstract" s i0)
    then None
    else 
    let opt1=After.after_whites s (i0+8) in
    if opt1=None then None else 
    let i1=Option.unpack opt1 in
    let opt2=After.after_one_among_several ["private";"protected";"public"] s i1 in
    let i2=(match opt2 with Some(i)->i |None->i1) in
    let opt3=After.after_whites s i2 in
    if opt3=None then None else 
    let i3=Option.unpack opt3 in
    if not(Substring.is_a_substring_located_at "function" s i3)
    then None
    else
    let opt4=After.after_whites s (i3+8) in
    if opt4=None then None else 
    let i4=Option.unpack opt4 in
    let i5=(
       if Strung.get s i4='&'
       then i4+1
       else i4
    ) in
    let opt6=After.after_php_label s i5 in
    if opt6=None then None else 
    let i6=Option.unpack opt6 in   
    let opt7=After.after_whites s i6 in
    if opt7=None then None else 
    let i7=Option.unpack opt7 in
    if not(Substring.is_a_substring_located_at "(" s i7)
    then None
    else
    let i8=After.after_closing_character ('(',')')  s (i7+1,1) in
    let opt9=After.after_whites s i8 in
    if opt9=None then None else 
    let i9=Option.unpack opt9 in
    if not(Substring.is_a_substring_located_at ";" s i9)
    then None
    else
    Some(i9,(i0,i1,i2,i3,i4,i5,i6,i7,i8));;

(*
  
  abstract_fnctn 
  "\tabstract protected function getOptimizationPasses();" 1;; 
  
  abstract_fnctn 
  "\tabstract  function getOptimizationPasses();" 1;; 

  *)    