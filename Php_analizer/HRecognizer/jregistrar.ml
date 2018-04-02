(*

#use"Php_analizer/HRecognizer/jregistrar.ml";;

*)


exception Name_already_used of string*
(string * Nonatomic_jrecognizer.t list * Atomic_hrecognizer.t option* (string list) option);;
exception Illegal_name of string;;

module Private=struct

let the_list=ref([]: (string*Nonatomic_jrecognizer.t) list);; 

let register name registered_one=
    if name=""
    then raise(Illegal_name(name))
    else
    if List.mem (String.get name 0) ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'] 
    then raise(Illegal_name(name))
    else 
    if List.exists (fun (name1,_)->name1=name) (!the_list)
    then raise(Name_already_used(name,Nonatomic_jrecognizer.unveil registered_one))
    else let _=(the_list:=(name,registered_one)::(!the_list)) in
         registered_one;;         

let ca=Special_chars_for_hrecognizer_name.chain_announcer;;
let da=Special_chars_for_hrecognizer_name.disjunction_announcer;;
let ka=Special_chars_for_hrecognizer_name.keyword_avoider_announcer;;


let cs=Special_chars_for_hrecognizer_name.chain_separator;;
let ds=Special_chars_for_hrecognizer_name.disjunction_separator;;
         
let op=Special_chars_for_hrecognizer_name.opener;;
let cl=Special_chars_for_hrecognizer_name.closer;;         

let anonymous_counter=ref(0);;

let default_name_for_atom ()=
    let j=(!anonymous_counter)+1 in
    let _=(anonymous_counter:=j) in
    "anon_"^(string_of_int j);;
   

let default_name_for_chain l=
   let temp1=Image.image Nonatomic_jrecognizer.name l in
   ca^op^(String.concat cs temp1)^cl;;

let default_name_for_disjunction l=
    let temp1=Image.image Nonatomic_jrecognizer.name l in
    da^op^(String.concat ds temp1)^cl;;   

 

let default_name_for_keyword_avoider (x,l)=
        ka^op^(Nonatomic_jrecognizer.name x)^
        (String.concat "#" l)^cl;;        

end;;

let leaf s_opt x=
  let name=(
    if s_opt="" 
    then Private.default_name_for_atom ()
    else s_opt
   ) in
   Private.register name (Nonatomic_jrecognizer.leaf name x);;

let chain s_opt l=
    let name=(
       if s_opt="" 
       then Private.default_name_for_chain l
       else s_opt
    ) in
    Private.register name (Nonatomic_jrecognizer.chain name l);;

let ordered_disjunction s_opt l=
      let name=(
        if s_opt="" 
        then Private.default_name_for_disjunction l
        else s_opt
      ) in
      Private.register name (Nonatomic_jrecognizer.ordered_disjunction name l);;

let keyword_avoider s_opt x=
        let name=(
          if s_opt="" 
          then Private.default_name_for_keyword_avoider x
          else s_opt    
        ) in
        Private.register name (Nonatomic_jrecognizer.keyword_avoider name x);;         

exception Unused_name of string;;

let recognizer_with_name name=
   try  List.assoc  name (!Private.the_list) with
   _->raise(Unused_name(name));; 

let replace_content_at_name name x=
    let new_list=Image.image (
         fun pair->let (name1,_)= pair in
         if name1=name then (name,x) else pair
    ) (!Private.the_list) in
    Private.the_list := new_list;;

let possibly_already_created_chain name l=
    try  List.assoc  name (!Private.the_list) with
   _->chain name l;;
    

