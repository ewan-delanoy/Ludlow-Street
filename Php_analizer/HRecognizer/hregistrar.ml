(*

#use"Php_analizer/HRecognizer/hregistrar.ml";;

*)



exception Name_already_used of string;;
exception Illegal_name of string;;

module Private=struct

let the_list=ref([]: (string*Nonatomic_hrecognizer.t) list);; 

let register name registered_one=
    if name=""
    then raise(Illegal_name(name))
    else
    if List.mem (String.get name 0) ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'] 
    then raise(Illegal_name(name))
    else 
    if List.exists (fun (name1,_)->name1=name) (!the_list)
    then raise(Name_already_used(name))
    else let _=(the_list:=(name,registered_one)::(!the_list)) in
         registered_one;;         

let ca=Special_chars_for_hrecognizer_name.chain_announcer;;
let da=Special_chars_for_hrecognizer_name.disjunction_announcer;;
let sa=Special_chars_for_hrecognizer_name.star_announcer;;
         
let cs=Special_chars_for_hrecognizer_name.chain_separator;;
let ds=Special_chars_for_hrecognizer_name.disjunction_separator;;
         
let op=Special_chars_for_hrecognizer_name.opener;;
let cl=Special_chars_for_hrecognizer_name.closer;;         

let default_name_for_chain l=
   let temp1=Image.image Nonatomic_hrecognizer.name l in
   ca^op^(String.concat cs temp1)^cl;;

let default_name_for_disjunction l=
    let temp1=Image.image Nonatomic_hrecognizer.name l in
    da^op^(String.concat ds temp1)^cl;;   

let default_name_for_star x=
    sa^op^(Nonatomic_hrecognizer.name x)^cl;;     

end;;

let leaf s x=Private.register s (Nonatomic_hrecognizer.leaf s x);;
let chain opt l=
    let name=(match opt with
       None->Private.default_name_for_chain l
       |Some(name1)->name1
    ) in
    Private.register name (Nonatomic_hrecognizer.chain name l);;

let ordered_disjunction opt l=
      let name=(match opt with
         None->Private.default_name_for_disjunction l
         |Some(name1)->name1
      ) in
      Private.register name (Nonatomic_hrecognizer.ordered_disjunction name l);;

let star opt x=
    let name=(match opt with
        None->Private.default_name_for_star x
        |Some(name1)->name1
    ) in
    Private.register name (Nonatomic_hrecognizer.star name x);;